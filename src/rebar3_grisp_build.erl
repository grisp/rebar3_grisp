-module(rebar3_grisp_build).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-include_lib("kernel/include/file.hrl").

-import(rebar3_grisp_util, [
    sh/1, sh/2, info/1, info/2, abort/2, console/1, console/2
]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, build},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}]},
            {example, "rebar3 grisp build"},
            {opts, [
                {clean, $c, "clean", {boolean, false},
                    "Completely clean Git repository before building"
                },
                {configure, $g, "configure", {boolean, true},
                    "Run autoconf & configure"
                }
            ]},
            {profiles, [default]},
            {short_desc, "Build a custom Erlang/OTP system for GRiSP"},
            {desc,
"Build a custom Erlang/OTP system for GRiSP.
"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _Rest} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, grisp, []),
    URL = "https://github.com/grisp/otp",
    Platform = "grisp_base",
    Version = rebar3_grisp_util:get([otp, version], Config, "19.3.6"),
    BuildRoot = rebar3_grisp_util:otp_build_root(State, Version),
    InstallRoot = rebar3_grisp_util:otp_install_root(State, Version),
    info("Checking out Erlang/OTP ~s", [Version]),
    ensure_clone(URL, BuildRoot, Version, Opts),
    Apps = apps(State),
    info("Preparing GRiSP code"),
    copy_code(Apps, Platform, BuildRoot, Version),
    info("Building"),
    build(Config, BuildRoot, InstallRoot, Opts),
    info("Done"),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

ensure_clone(URL, Dir, Version, Opts) ->
    Branch = "grisp/OTP-" ++ Version,
    case file:read_file_info(Dir ++ "/.git") of
        {error, enoent} ->
            console("* Cloning...  (this may take a while)"),
            sh(
                "git clone "
                "-b " ++ Branch ++ " "
                "--single-branch " ++
                URL ++ " " ++ Dir
            );
        {ok, #file_info{type = directory}} ->
            console("* Using existing checkout"),
            ok
    end,
    sh("git checkout " ++ Branch, [{cd, Dir}]),
    case rebar3_grisp_util:get(clean, Opts, false) of
        true ->
            console("* Cleaning..."),
            sh("git reset --hard", [{cd, Dir}]),
            sh("git clean -fXd", [{cd, Dir}]),
            sh("git clean -fxd", [{cd, Dir}]);
        false ->
            ok
    end,
    ok.

apps(State) ->
    Apps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
    {Grisp, Other} = rebar3_grisp_util:grisp_app(Apps),
    Other ++ Grisp.

copy_code(Apps, Platform, OTPRoot, Version) ->
    console("* Copying C code..."),
    Drivers = lists:foldl(
        fun(A, D) ->
            copy_app_code(A, Platform, OTPRoot, D)
        end,
        [],
         Apps
    ),
    patch_otp(OTPRoot, Drivers, Version).

copy_app_code(App, Platform, OTPRoot, Drivers) ->
    Source = filename:join([rebar_app_info:dir(App), "grisp", Platform]),
    copy_sys(Source, OTPRoot),
    Drivers ++ copy_drivers(Source, OTPRoot).

copy_sys(Source, OTPRoot) ->
    copy_files(
        {Source, "sys/*.h"},
        {OTPRoot, "erts/emulator/sys/unix"}
    ),
    copy_files(
        {Source, "sys/*.c"},
        {OTPRoot, "erts/emulator/sys/unix"}
    ).

copy_drivers(Source, OTPRoot) ->
    copy_files(
        {Source, "drivers/*.h"},
        {OTPRoot, "erts/emulator/drivers/unix"}
    ),
    copy_files(
        {Source, "drivers/*.c"},
        {OTPRoot, "erts/emulator/drivers/unix"}
    ).

copy_files({SourceRoot, Pattern}, Target) ->
    Files = filelib:wildcard(filename:join(SourceRoot, Pattern)),
    [copy_file(F, Target) || F <- Files].

copy_file(Source, {TargetRoot, TargetDir}) ->
    Base = filename:basename(Source),
    TargetFile = filename:join(TargetDir, Base),
    Target = filename:join(TargetRoot, TargetFile),
    rebar_api:debug("GRiSP - Copy ~p -> ~p", [Source, Target]),
    {ok, _} = file:copy(Source, Target),
    TargetFile.

patch_otp(OTPRoot, Drivers, Version) ->
    TemplateFile = filename:join([
        code:priv_dir(rebar3_grisp),
        "patches/otp-" ++ Version ++ ".patch.mustache"
    ]),
    case filelib:is_file(TemplateFile) of
        true  -> apply_patch(TemplateFile, Drivers, OTPRoot);
        false -> abort("Patch file for OTP ~s missing", [Version])
    end.

apply_patch(TemplateFile, Drivers, OTPRoot) ->
    Template = bbmustache:parse_file(TemplateFile),
    Context = [
        {erts_emulator_makefile_in, [
            {lines, 10 + length(Drivers)},
            {drivers, [[{name, filename:basename(N, ".c")}] || N <- Drivers]}
        ]}
    ],
    Patch = bbmustache:compile(Template, Context, [{key_type, atom}]),
    ok = file:write_file(filename:join(OTPRoot, "otp.patch"), Patch),
    case sh("git apply otp.patch --reverse --check", [{cd, OTPRoot}, return_on_error]) of
        {ok, _} ->
            console("* Patching OTP... (skipped, already patched)");
        {error, {1, _}} ->
            console("* Patching OTP..."),
            sh("git apply otp.patch", [{cd, OTPRoot}])
    end,
    sh("rm otp.patch", [{cd, OTPRoot}]).

build(Config, BuildRoot, InstallRoot, Opts) ->
    TcRoot = rebar3_grisp_util:get([toolchain, root], Config),
    PATH = os:getenv("PATH"),
    AllOpts = [{env, [
        {"GRISP_TC_ROOT", TcRoot},
        {"PATH", TcRoot ++ "/bin:" ++ PATH}
    ]}],
    BuildOpts = [{cd, BuildRoot}|AllOpts],
    InstallOpts = [{cd, InstallRoot}|AllOpts],
    rebar_api:debug("~p", [BuildOpts]),
    case rebar3_grisp_util:get(configure, Opts) of
        true ->
            console("* Running autoconf..."),
            sh("./otp_build autoconf", BuildOpts),
            console("* Running configure...  (this may take a while)"),
            sh(
                "./otp_build configure "
                "--xcomp-conf=xcomp/erl-xcomp-arm-rtems.conf "
                "--prefix=/",
                BuildOpts
            );
        false ->
            ok
    end,
    console("* Compiling...  (this may take a while)"),
    sh("./otp_build boot -a", BuildOpts),
    console("* Installing..."),
    ok = filelib:ensure_dir(filename:join(InstallRoot, ".")),
    sh("rm -rf " ++ InstallRoot ++ "/*", InstallOpts),
    sh("make install DESTDIR=\"" ++ InstallRoot ++ "\"", BuildOpts),
    sh("mv lib lib.old", InstallOpts),
    sh("mv lib.old/erlang/* .", InstallOpts),
    sh("rm -rf lib.old", InstallOpts),
    Wildcard = "erts-*/bin/beam.smp",
    [BeamSmp] = filelib:wildcard(filename:join(InstallRoot, Wildcard)),
    Beam = string:substr(BeamSmp, 1, length(BeamSmp) - 4),
    sh("rm -f " ++ Beam, InstallOpts),
    sh("mv " ++ BeamSmp ++ " " ++ Beam, InstallOpts),
    sh(
        "arm-rtems4.12-objcopy "
        "-O binary "
        ++ Beam ++ " "
        ++ Beam ++ ".bin",
        InstallOpts
    ).
