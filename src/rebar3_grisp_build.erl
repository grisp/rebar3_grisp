-module(rebar3_grisp_build).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-include_lib("kernel/include/file.hrl").

-import(rebar3_grisp_util, [
    sh/1, sh/2, debug/1, debug/2, info/1, info/2, abort/1, abort/2, console/1, console/2
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
                {tar, $t, "tar", {boolean, false},
                    "Create tarball with OTP installation for online repository"
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
    Config = rebar3_grisp_util:config(State),
    URL = rebar3_grisp_util:otp_git(),
    Board = rebar3_grisp_util:board(Config),
    Version = rebar3_grisp_util:otp_version(Config),

    Apps = rebar3_grisp_util:apps(State),

    [abort_no_build() || not rebar3_grisp_util:should_build(Config)],
    TcRoot = toolchain_root(Config),
    BuildRoot = rebar3_grisp_util:otp_build_root(State, Version),
    InstallRoot = rebar3_grisp_util:otp_build_install_root(State, Version),
    GrispFolder = rebar3_grisp_util:root(State),

    info("Checking out Erlang/OTP ~s", [Version]),
    ensure_clone(URL, BuildRoot, Version, Opts),

    console("* Copying C code..."),
    {DriverFiles, NIFFiles} = copy_files(Apps, Board, BuildRoot),

    console("* Patching OTP to include sys, driver and NIF files"),
    patch_otp(BuildRoot, maps:keys(DriverFiles), maps:keys(NIFFiles), Version),

    info("Building"),
    ErlXComp = "erl-xcomp-" ++ Version ++ ".conf",
    ErlXCompPath = find_file(Apps, Board, ["xcomp", ErlXComp]),
    BuildConfFile = config_file(Apps, Board, ["grisp.conf"]),
    BuildConfig = rebar3_grisp_util:merge_config(Config, BuildConfFile),
    build(BuildConfig, ErlXCompPath, BuildRoot, InstallRoot, TcRoot, Opts),

    info("Computing file hashes"),
    {Hash, HashString} = grisp_tools_util:source_hash(Apps, Board),

    info("Writing hashes to file. Hash: ~p", [Hash]),
    ok = file:write_file(rebar3_grisp_util:otp_hash_listing_path(InstallRoot),
                         list_to_binary(HashString)),

    info("Copying revision string into install dir"),
    RevSource = filename:join([TcRoot, "rtems/5", "GRISP_TOOLCHAIN_REVISION"]),
    RevDestination = filename:join(InstallRoot, "GRISP_TOOLCHAIN_REVISION"),
    case file:copy(RevSource, RevDestination) of
        {ok, _} -> ok;
        _       -> abort("Toolchain revision file not found:~n~s", [RevSource])
    end,

    case rebar3_grisp_util:get(tar, Opts, false) of
        true ->
            Filename = tar_file_name(GrispFolder, Version, Hash),
            info("Creating package ~p", [Filename]),
            create_tar(Filename, InstallRoot);
        false -> ok
    end,

    info("Done"),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

copy_files(Apps, Board, BuildRoot) ->
    {SystemFiles, DriverFiles, NIFFiles} = grisp_tools_util:source_files(Apps, Board),
    debug("Apps ~p, Board ~p~n", [Apps, Board]),
    debug("Detected the following C files:~nSystemFiles ~p~n, DriverFiles ~p~n, NIFFiles ~p~n", [SystemFiles, DriverFiles, NIFFiles]),

    ToFrom = maps:merge(SystemFiles, DriverFiles),
    ToFrom2 = maps:merge(ToFrom, NIFFiles),
    ToFromAbsolute = rebar3_grisp_util:filenames_join_copy_destination(ToFrom2, BuildRoot),
    maps:map(
      fun(Target, Source) ->
              debug("GRiSP - Copy ~p -> ~p", [Source, Target]),
              {ok, _} = file:copy(Source, Target)
      end,
      ToFromAbsolute
     ),
    {DriverFiles, NIFFiles}.

tar_file_name(GrispFolder, Version, Hash) ->
    filename:join([GrispFolder, "otp", Version, "package",
                   rebar3_grisp_util:otp_cache_file_name(Version, Hash)]).

create_tar(Filename, InstallRoot) ->
    rebar3_grisp_util:ensure_dir(Filename),
    sh("tar -zcf " ++ Filename ++ "  .", [{cd, InstallRoot}]).

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

find_file(Apps, Board, PathParts) ->
    Path = filename:join(["grisp", Board | PathParts]),
    FoldFun = fun
        ({_App, Dir}, undefined) ->
            AbsPath = filename:join([Dir, Path]),
            case filelib:is_file(AbsPath) of
                true -> AbsPath;
                false -> undefined
            end;
        (_, Result) -> Result
    end,
    case lists:foldl(FoldFun, undefined, Apps) of
        undefined -> abort("File ~s not found", [Path]);
        Result -> Result
    end.

config_file(Apps, Board, PathParts) ->
    config_file(Apps, Board, PathParts, []).

config_file(Apps, Board, PathParts, DefaultConf) ->
    Path = filename:join(["grisp", Board | PathParts]),
    FoldFun = fun({_App, Dir}, Old) ->
        AbsPath = filename:join([Dir, Path]),
        case filelib:is_file(AbsPath) of
            false -> Old;
            true ->
                case file:consult(AbsPath) of
                    {error, Reason} ->
                        abort("Bad config file ~s: ~p", [AbsPath, Reason]);
                    {ok, New} -> rebar3_grisp_util:merge_config(New, Old)
                end
        end
    end,
    lists:foldl(FoldFun, DefaultConf, lists:reverse(Apps)).

patch_otp(OTPRoot, Drivers, NIFs, Version) ->
    debug("Patching OTP Version ~p", [Version]),
    TemplateFile = filename:join([
        code:priv_dir(rebar3_grisp),
        "patches/otp-" ++ Version ++ ".patch.mustache"
    ]),
    case filelib:is_file(TemplateFile) of
        true  -> apply_patch(TemplateFile, Drivers, NIFs, OTPRoot);
        false -> abort("Patch file for OTP ~s missing", [Version])
    end.

apply_patch(TemplateFile, Drivers, NIFs, OTPRoot) ->
    debug("Using Template ~p", [TemplateFile]),
    DrvPatchLineCount = 10 + length(Drivers),
    NifPatchLineCount = 9 + length(NIFs),
    Context = #{
                erts_emulator_makefile_in =>
                    #{
                      driver_lines => DrvPatchLineCount,
                      nif_lines => NifPatchLineCount,
                      total_lines => DrvPatchLineCount + NifPatchLineCount,
                      drivers => [#{name => filename:basename(N, ".c")} || N <- Drivers],
                      nifs => [#{name => filename:basename(N, ".c")} || N <- NIFs]
                     }
               },
    Patch = grisp_tools_template:render(TemplateFile, Context),
    ok = file:write_file(filename:join(OTPRoot, "otp.patch"), Patch),
    case sh("git apply otp.patch --ignore-whitespace --reverse --check",
            [{cd, OTPRoot}, return_on_error]) of
        {ok, _} ->
            console("  (skipped, already patched)");
        {error, {1, _}} ->
            sh("git apply --ignore-whitespace otp.patch", [{cd, OTPRoot}])
    end,
    sh("rm otp.patch", [{cd, OTPRoot}]).

build(Config, ErlXComp, BuildRoot, InstallRoot, TcRoot, Opts) ->
    PATH = os:getenv("PATH"),
    AllOpts = [{env, [
                      {"GRISP_TC_ROOT", TcRoot},
                      {"PATH", filename:join([TcRoot, "rtems/5/bin"]) ++ ":" ++ PATH}
                     ]}],
    BuildOpts = [{cd, BuildRoot}|AllOpts],
    InstallOpts = [{cd, InstallRoot}|AllOpts],
    debug("~p", [BuildOpts]),
    case rebar3_grisp_util:get(configure, Opts) of
        true ->
            console("* Running autoconf..."),
            sh("./otp_build autoconf", BuildOpts),
            console("* Running configure...  (this may take a while)"),
            sh(
                "./otp_build configure "
                " --xcomp-conf=" ++ ErlXComp ++
                " --prefix=/",
                BuildOpts
            );
        false ->
            ok
    end,
    console("* Compiling...  (this may take a while)"),
    sh("./otp_build boot -a", BuildOpts),
    console("* Installing..."),
    rebar3_grisp_util:ensure_dir(filename:join(InstallRoot, ".")),
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

    case rebar3_grisp_util:get([build, post_script], Config, undefined) of
        undefined -> ok;
        PostCmd ->
            console("* Running post script..."),
            Rx = "/erts-([.0-9]*)/bin/beam$",
            {match, [Ver]} = re:run(Beam, Rx, [{capture, all_but_first, list}]),
            ScriptOpts = [
                          {cd, InstallRoot},
                          {env, [
                                 {"GRISP_TC_ROOT", TcRoot},
                                 {"PATH", filename:join([TcRoot, "rtems/5/bin"]) ++ ":" ++ PATH},
                                 {"OTP_ROOT", InstallRoot},
                                 {"ERTS_VERSION", Ver},
                                 {"BEAM_PATH", Beam}
                                ]}
                         ],
            sh(PostCmd, ScriptOpts)
    end.

toolchain_root(Config) ->
    Conf = rebar3_grisp_util:get([build, toolchain, directory], Config, error),
    case os:getenv("GRISP_TOOLCHAIN", Conf) of
        error ->
            abort("Please specify the full path to the toolchain directory " ++
"in your rebar.conf:

{grisp, [
    ...,
    {build, [
        {toolchain, [
                {directory, \"/PATH/TO/TOOLCHAIN\"}
        ]}
    ]}
]}.

Alternatively, you can set the GRISP_TOOLCHAIN environment variable."
        );
        Directory ->
            Directory
    end.

abort_no_build() ->
    abort("There was no build section found in your rebar.conf").
