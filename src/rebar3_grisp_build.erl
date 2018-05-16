-module(rebar3_grisp_build).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-include("rebar3_grisp.hrl").
-include_lib("kernel/include/file.hrl").

-import(rebar3_grisp_util, [
    sh/1, sh/2, info/1, info/2, abort/1, abort/2, console/1, console/2
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
    Config = rebar_state:get(State, grisp, []),
    URL = "https://github.com/grisp/otp",
    Board = rebar3_grisp_util:get([board], Config, ?DEFAULT_GRISP_BOARD),
    Version = rebar3_grisp_util:get([otp, version], Config, ?DEFAULT_OTP_VSN),

    Apps = rebar3_grisp_util:apps(State),

    case rebar3_grisp_util:should_build(Config) of
        false ->
            abort_no_toolchain();
        true ->
            TcRoot = try rebar3_grisp_util:get([build, toolchain, directory], Config)
            catch
                error:{key_not_found, _, _} -> abort_no_toolchain()
            end,

            BuildRoot = rebar3_grisp_util:otp_build_root(State, Version),
            InstallRoot = rebar3_grisp_util:otp_build_install_root(State, Version),
            GrispFolder = rebar3_grisp_util:root(State),

            info("Checking out Erlang/OTP ~s", [Version]),
            ensure_clone(URL, BuildRoot, Version, Opts),

            console("* Copying C code..."),
            DriverFiles = copy_files(Apps, Board, BuildRoot),

            console("* Patching OTP to include sys and driver files"),
            patch_otp(BuildRoot, maps:keys(DriverFiles), Version),

            info("Building"),
            ErlXComp = "erl-xcomp-" ++ Version ++ ".conf",
            ErlXCompPath = find_file(Apps, Board, ["xcomp", ErlXComp]),
            BuildConfFile = config_file(Apps, Board, ["grisp.conf"]),
            BuildConfig = rebar3_grisp_util:merge_config(Config, BuildConfFile),
            build(BuildConfig, ErlXCompPath, BuildRoot, InstallRoot, Opts, TcRoot),

            info("Computing file hashes"),
            {Hash, HashString} = rebar3_grisp_util:get_hash(Apps, Board),

            info("Writing hashes to file. Hash: ~p", [Hash]),
            ok = file:write_file(rebar3_grisp_util:otp_hash_listing_path(InstallRoot),
                            list_to_binary(HashString)),
            case rebar3_grisp_util:get(tar, Opts, false) of
                true ->
                    Filename = tar_file_name(GrispFolder, Version, Hash),
                    info("Creating tar archive ~p", [Filename]),
                    create_tar(Filename, InstallRoot);
                false -> ok
            end,

            info("Done"),
            {ok, State}
    end.
-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

copy_files(Apps, Board, BuildRoot) ->
    {SystemFiles, DriverFiles} = rebar3_grisp_util:files_copy_destination(Apps, Board),
    ToFrom = maps:merge(SystemFiles, DriverFiles),
    ToFromAbsolute = rebar3_grisp_util:filenames_join_copy_destination(ToFrom, BuildRoot),
    maps:map(
      fun(Target, Source) ->
              rebar_api:debug("GRiSP - Copy ~p -> ~p", [Source, Target]),
              {ok, _} = file:copy(Source, Target)
      end,
      ToFromAbsolute
     ),
    DriverFiles.

tar_file_name(GrispFolder, Version, Hash) ->
    filename:join([GrispFolder, "otp", Version, "package",
                   rebar3_grisp_util:otp_cache_file_name(Version, Hash)]).

create_tar(Filename, InstallRoot) ->
    sh("tar -zcf " ++
           Filename ++
           "  .", [{cd, InstallRoot}]).

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
        (App, undefined) ->
            AppDir = rebar_app_info:dir(App),
            AbsPath = filename:join([AppDir, Path]),
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
    FoldFun = fun(App, Old) ->
        AppDir = rebar_app_info:dir(App),
        AbsPath = filename:join([AppDir, Path]),
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

patch_otp(OTPRoot, Drivers, Version) ->
    rebar_api:debug("Patching OTP Version ~p", [Version]),
    TemplateFile = filename:join([
        code:priv_dir(rebar3_grisp),
        "patches/otp-" ++ Version ++ ".patch.mustache"
    ]),
    case filelib:is_file(TemplateFile) of
        true  -> apply_patch(TemplateFile, Drivers, OTPRoot);
        false -> abort("Patch file for OTP ~s missing", [Version])
    end.

apply_patch(TemplateFile, Drivers, OTPRoot) ->
    rebar_api:debug("Using Template ~p", [TemplateFile]),
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

build(Config, ErlXComp, BuildRoot, InstallRoot, Opts, TcRoot) ->
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
              "--xcomp-conf=" ++ ErlXComp ++
                  " --prefix=/",
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

    case rebar3_grisp_util:get([build, post_script], Config, undefined) of
        undefined -> ok;
        PostCmd ->
            console("* Running post script..."),
            Rx = "/erts-([.0-9]*)/bin/beam$",
            {match, [Ver]} = re:run(Beam, Rx, [{capture, all_but_first, list}]),
            ScriptOpts = [
                          {cd, InstallRoot},
                          {env, [
                                 {"PATH", TcRoot ++ "/bin:" ++ PATH},
                                 {"ERTS_VER", Ver},
                                 {"BEAM_PATH", Beam}
                                ]}
                         ],
            sh(PostCmd, ScriptOpts)
    end.

abort_no_toolchain() ->
    abort("Please specify a build toolchain to build the cross compiled OTP release:~n" ++
              "{build, [~n" ++
              "{toolchain, [{directory, \"PATH/TO/TOOLCHAIN\"}]}~n" ++
              "]}~n").
