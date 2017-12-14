-module(rebar3_grisp_build).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-include_lib("kernel/include/file.hrl").

-import(rebar3_grisp_util, [
    sh/1, sh/2, info/1, info/2, warn/1, abort/1, console/1, console/2
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
                },
                {flavour, $f, "flavour", {atom, single},
                    "Beam flavour (single|plain). "
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
    Flavour = rebar3_grisp_util:get(flavour, Opts),
    case Flavour of
        single -> ok;
        plain -> ok;
        smp ->
            warn("SMP flavour not really supported yet, "
                 "the release will probably not work.");
        _ ->
            abort("unsupported flavour")
    end,
    URL = "https://github.com/grisp/otp",
    Platform = "grisp_base",
    Version = "19.3.6",
    BuildRoot = rebar3_grisp_util:otp_build_root(State, Version),
    InstallRoot = rebar3_grisp_util:otp_install_root(State, Version),
    info("Checking out Erlang/OTP ~s", [Version]),
    ensure_clone(URL, BuildRoot, Version, Opts),
    Apps = apps(State),
    info("Preparing GRiSP code"),
    copy_code(Apps, Platform, BuildRoot),
    info("Building (~p)", [Flavour]),
    build(Config, BuildRoot, InstallRoot, Opts),
    info("Done"),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

ensure_clone(URL, Dir, Version, Opts) ->
    Branch = "grisp/OTP-" ++ Version,
    ok = filelib:ensure_dir(filename:join(Dir, ".")),
    case file:read_file_info(Dir ++ "/.git") of
        {error, enoent} ->
            console("* Cloning...  (this may take a while)"),
            sh("git clone " ++ URL ++ " " ++ Dir);
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

copy_code(Apps, Platform, OTPRoot) ->
    console("* Copying C code..."),
    Drivers = lists:foldl(
        fun(A, D) ->
            copy_app_code(A, Platform, OTPRoot, D)
        end,
        [],
         Apps
    ),
    patch_otp(OTPRoot, Drivers).

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

patch_otp(OTPRoot, Drivers) ->
    Template = bbmustache:parse_file(
        filename:join(code:priv_dir(rebar3_grisp), "patches/otp.patch.mustache")
    ),
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
    Flavour = rebar3_grisp_util:get(flavour, Opts),
    ConfigureOpt = rebar3_grisp_util:get(configure, Opts),
    FlavourChanged = check_last_flavour(Flavour, BuildRoot),
    DoConfigure = case {ConfigureOpt, FlavourChanged} of
        {Opt, true} -> Opt;
        {true, false} -> true;
        {false, false} ->
            warn("Flavour changed, enforcing reconfiguration."),
            true
    end,

    case DoConfigure of
        true ->
            console("* Running autoconf..."),
            sh("./otp_build autoconf", BuildOpts),
            console("* Running configure...  (this may take a while)"),
            sh(
                "./otp_build configure "
                "--xcomp-conf=xcomp/erl-xcomp-arm-rtems.conf "
                "--prefix=/ "
                ++ configure_extra_options(Flavour),
                BuildOpts
            );
        false ->
            ok
    end,
    console("* Compiling...  (this may take a while)"),
    sh("./otp_build " ++ build_command(Flavour) ++ " -a", BuildOpts),
    store_flavour(Flavour, BuildRoot),
    console("* Installing..."),
    ok = filelib:ensure_dir(filename:join(InstallRoot, ".")),
    sh("rm -rf " ++ InstallRoot ++ "/*", InstallOpts),
    sh("make install DESTDIR=\"" ++ InstallRoot ++ "\"", BuildOpts),
    sh("mv lib lib.old", InstallOpts),
    sh("mv lib.old/erlang/* .", InstallOpts),
    sh("rm -rf lib.old", InstallOpts),
    Beam = install_beam(Flavour, InstallRoot, AllOpts),
    sh(
        "arm-rtems4.12-objcopy "
        "-O binary "
        ++ Beam ++ " "
        ++ Beam ++ ".bin",
        InstallOpts
    ).


%--- Internal Functions --------------------------------------------------------

check_last_flavour(Flavour, BuildRoot) ->
    FlavourPath = filename:join(BuildRoot, "GRISP_FLAVOUR"),
    case file:read_file(FlavourPath) of
        {error, _} -> false;
        {ok, FlavourStr} ->
            binary_to_existing_atom(FlavourStr, latin1) =:= Flavour
    end.

store_flavour(Flavour, BuildRoot) ->
    FlavourPath = filename:join(BuildRoot, "GRISP_FLAVOUR"),
    file:write_file(FlavourPath, atom_to_binary(Flavour, latin1)).

configure_extra_options(single) ->
    "--disable-threads";
configure_extra_options(plain) ->
    "--disable-smp-support";
configure_extra_options(smp) ->
    "".

build_command(single) -> "boot";
build_command(plain) -> "plain";
build_command(smp) -> "smp".

install_beam(Flavour, InstallRoot, Opts)
  when Flavour =:= single; Flavour =:= plain ->
    Wildcard = "erts-*/bin/beam",
    [Beam] = filelib:wildcard(filename:join(InstallRoot, Wildcard)),
    sh("rm -f " ++ Beam ++ ".smp", Opts),
    Beam;
install_beam(smp, InstallRoot, Opts) ->
    Wildcard = "erts-*/bin/beam.smp",
    [SmpBeam] = filelib:wildcard(filename:join(InstallRoot, Wildcard)),
    Beam = string:substr(SmpBeam, 1, length(SmpBeam) - 4),
    sh("rm -f " ++ Beam, Opts),
    sh("mv " ++ SmpBeam ++ " " ++ Beam, Opts),
    Beam.
