-module(rebar3_grisp_deploy).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    debug/2,
    info/1,
    info/2,
    console/1,
    console/2,
    warn/1,
    warn/2,
    abort/1,
    abort/2
]).

-define(MAX_DDOT, 2).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, deploy},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}, {default, lock}]},
            {example, "rebar3 grisp deploy"},
            {opts, [
                {relname, $n, "relname", string, "Specify the name for the release that will be deployed"},
                {relvsn, $v, "relvsn", string, "Specify the version of the release"},
                {tar, $t, "tar", {boolean, false}, "Create tarball with the release in _grisp/deploy"},
                {destination, $d, "destination", string, "Path to put deployed release in"},
                {force, $f, "force", {boolean, false}, "Replace existing files"},
                {pre_script, undefined, "pre-script", string, "Shell script to run before deploying begins"},
                {post_script, undefined, "post-script", string, "Shell script to run after deploying has finished"}
            ]},
            {profiles, [grisp]},
            {short_desc, "Deploy a GRiSP release to a destination"},
            {desc,
                "Deploys a GRiSP application.\n"
                "\n"
                "The command requires the release name and version to be "
                "provided. Options passed after '--' are sent to the Rebar 3 "
                "release task.\n"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    Config = rebar3_grisp_util:config(RState),
    OTPVersion = rebar3_grisp_util:otp_version(Config),
    Board = rebar3_grisp_util:platform(Config),
    CopyDest = get_option(destination, [deploy, destination], RState, undefined),
    {Args, _} = rebar_state:command_parsed_args(RState),
    Force = proplists:get_value(force, Args, false),
    Tar = proplists:get_value(tar, Args, false),
    RelNameArg = proplists:get_value(relname, Args),
    RelVsnArg = proplists:get_value(relvsn, Args),

    ProjectRoot = rebar_dir:root_dir(RState),
    Apps = rebar3_grisp_util:apps(RState),

    CustomBuild = rebar3_grisp_util:should_build(Config),

    RState2 = compile_for_grisp(Config, RState),

    try
        {RelName, RelVsn}
            = rebar3_grisp_util:select_release(RState2, RelNameArg, RelVsnArg),
        DistSpec = case {Tar, CopyDest}  of
            {false, D} when D =:= undefined; D =:= "" ->
                error(no_deploy_destination);
            {true, D}  when D =:= undefined; D =:= "" -> [
                bundle_dist_spec(RState2, RelName, RelVsn, Force)
            ];
            {false, _} -> [
                copy_dist_spec(RState2, Force)
            ];
            {true, _} -> [
                bundle_dist_spec(RState2, RelName, RelVsn, Force),
                copy_dist_spec(RState2, Force)
            ]
        end,
        Profiles = [P || P <- rebar_state:current_profiles(RState2),
                         P =/= default, P =/= grisp, P =/= test],
        DeploySpec = #{
            project_root => ProjectRoot,
            apps => Apps,
            otp_version_requirement => OTPVersion,
            platform => Board,
            custom_build => CustomBuild,
            distribute => DistSpec,
            release => #{
                name => RelName,
                version => RelVsn,
                profiles => Profiles
            },
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{
                    name => RelName,
                    version => RelVsn
                }},
                shell => {fun rebar3_grisp_handler:shell/3, #{}},
                release => {fun release_handler/2, RState2}
            })
        },
        State = grisp_tools:deploy(DeploySpec),
        #{release := RState3} = grisp_tools:handlers_finalize(State),
        info("Deployment done"),
        {ok, RState3}
    catch
        error:no_deploy_destination ->
            abort(
                "No deploy destination specified~n"
                "The -t/--tar option should be specified, or the copy "
                "destination should be defined either with the command line "
                "option -d/--destination or by configuring it in "
                "rebar.config:~n"
                "~n"
                "    {grisp, [{deploy, [{destination, \"/tmp/grisp\"}]}]}.~n",
                []
            );
        error:{release_not_selected, [{Name, [Version|_]}|_]} ->
            abort(
                "Multiple releases defined!~n"
                "You must specify a name and optionally a version. Examples:~n"
                "~n"
                "    rebar3 grisp deploy --relname ~p~n"
                "    rebar3 grisp deploy --relname ~p --relvsn ~s~n",
                [Name, Name, Version]
            );
        error:no_release_configured ->
            App = rebar_app_info:name(hd(rebar_state:project_apps(RState2))),
            abort(
                "No release configured"
                "~n"
                "You must specify at least one release in 'rebar.config' to be "
                "able to deploy~nyour project. Example:~n"
                "~n"
                "    {relx,~n"
                "        {~s, \"0.1.0\", [~s]}~n"
                "    }.~n",
                [App, App]
            );
        error:{unknown_release_name, {Name, _Version}, Names} ->
            abort(
                "Unknown release '~p'~n"
                "~n"
                "Must be one of:" ++ [["~n  ", atom_to_list(N)] || N <- Names],
                [Name]
            );
        error:{unknown_release_version, {Name, Version}, Versions} ->
            abort(
                "Release '~p' has no version ~s~n"
                "~n"
                "Must be one of:" ++ [["~n  ", V] || V <- Versions],
                [Name, Version]
            );
        error:{could_not_create_dir, Dir, Reason} ->
            abort("Could not create directory ~s:~n  ~p", [Dir, Reason]);
        error:{could_not_delete_file, File, Reason} ->
            abort("Error deleting ~s: ~p", [File, Reason]);
        error:{package, {not_found, Version, Hash}} ->
            abort(
                "Could not find a pre-built package for Erlang ~s!~n"
                "Possible causes:~n"
                "  - You have custom C sources and need to do a manual build:~n"
                "      rebar3 grisp build~n"
                "  - There is no pre-built package matching this hash:~n"
                "      ~s",
                [Version, Hash]
            );
        error:{release_unspecified, _} ->
            abort("Release name and/or version not specified");
        error:{template_error, File, {missing_key, Key}} ->
            Root = rebar_dir:root_dir(RState2),
            {ok, Relative} = rebar_file_utils:path_from_ancestor(File, Root),
            abort(
                "Error rendering ~s:~nmissing template key: ~s",
                [Relative, Key]
            );
        error:{template_error, File, {include_not_found, Include}} ->
            Root = rebar_dir:root_dir(RState2),
            {ok, Relative} = rebar_file_utils:path_from_ancestor(File, Root),
            abort(
                "Error rendering ~s:~nmissing include file: ~s",
                [Relative, Include]
            );
        error:{create_dir_failed, Dir, {error, Reason}} ->
            abort(
                "Error creating directory ~s: ~s",
                [Dir, file:format_error(Reason)]
            );
        error:{otp_version_not_found, Configured} ->
            Error = "Could not find an OTP version matching the configured "
                "version ~p~n"
                "~n",
            case CustomBuild of
                false ->
                    abort(
                        Error ++
                        "To see a list of available versions, run:~n"
                        "~n"
                        "    rebar3 grisp package list~n",
                        [Configured]
                    );
                true ->
                    abort(
                        Error ++
                        "You need to build OTP before continuing:~n"
                        "~n"
                        "    rebar3 grisp build~n",
                        [Configured]
                    )
            end
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

copy_dist_spec(RState, Force) ->
    CopyDest = get_option(destination, [deploy, destination], RState, undefined),
    PreScript = get_option(pre_script, [deploy, pre_script], RState, undefined),
    PostScript = get_option(pre_script, [deploy, post_script], RState, undefined),
    {copy, #{
        type => copy,
        force => Force,
        destination => CopyDest,
        scripts => #{
            pre_script => PreScript,
            post_script => PostScript
        }
    }}.

bundle_dist_spec(RState, RelName, RelVsn, Force) ->
    BundleFile = rebar3_grisp_util:bundle_file_path(RState, RelName, RelVsn),
    {bundle, #{
        type => archive,
        force => Force,
        compressed => true,
        destination => BundleFile
    }}.

% grisp_tools Events

event_handler(Event, State) ->
    case Event of
        [deploy, package, download|_] ->
            {ok, download(Event, State)};
        _ ->
            event(Event),
            {ok, State}
    end.

event([deploy, validate, version]) ->
    console("* Resolving OTP version");
event([deploy, validate, version, {selected, Version, Target}]) ->
    io:format("    ~s (requirement was \"~s\")~n", [Version, Target]);
event([deploy, validate, version, {mismatch, Target, Current}]) ->
    abort(
        "Current Erlang version (~p) does not match target "
        "Erlang version (~p)", [Current, Target]
    );
event([deploy, validate, version, {connection_error, Error}]) ->
    console("    (Could not list packages [error: ~p], using cache)", [Error]);
event([deploy, collect, {hash, Hash, Index}]) ->
    debug("GRiSP hash:~n~s~n~n~p", [Hash, Index]);
event([deploy, package, {type, {custom_build, Hash}}]) ->
    console("* Using custom OTP (~s)", [short(Hash)]);
event([deploy, package, {type, {package, Hash}}]) ->
    console("* Using pre-built OTP package (~s)", [short(Hash)]);
event([deploy, package, extract]) ->
    console("* Extracting package");
event([deploy, package, extract, '_skip']) ->
    io:format("    (already extracted)~n");
event([deploy, package, extract, {error, Reason}]) ->
    abort("Extraction failed: ~p", [Reason]);
event([deploy, distribute, Name, ScriptName, {run, _Script}]) ->
    console("* Running ~s ~p", [Name, ScriptName]);
event([deploy, distribute, _Name, _ScriptName, {result, Output}]) ->
    case trim(Output) of
        ""      -> ok;
        Trimmed -> console(Trimmed)
    end;
event([deploy, distribute, bundle, release, {archive, _Source, _Target}]) ->
    console("* Bundling release...");
event([deploy, distribute, copy, release, {copy, _Source, _Target}]) ->
    console("* Copying release...");
event([deploy, distribute, copy, files, {init, _Dest}]) ->
    console("* Copying files...");
event([deploy, distribute, copy, files, {error, file_exists, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort(
        "Destination ~s already exists (use --force to overwrite)",
        [RelPath]
    );
event([deploy, distribute, bundle, files, {init, _Dest}]) ->
    console("* Bundling files...");
event([deploy, distribute, _Name, files, {_, #{app := App, target := File}}]) ->
    io:format("    [~p] ~s~n", [App, File]);
event([deploy, distribute, bundle, archive, {closed, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    console("* GRiSP deploy bundle archived in ~s", [RelPath]);
event([deploy, distribute, _Name, {error, dir_missing, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Missing directory: ~s", [RelPath]);
event([deploy, distribute, _Name, {error, dir_access, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Directory not accessible: ~s", [RelPath]);
event([deploy, distribute, _Name, {error, not_a_directory, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Not a proper directory: ~s", [RelPath]);
event([deploy, distribute, _Name, {error, file_access, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("File not accessible: ~s", [RelPath]);
event([deploy, distribute, _Name, {error, not_a_file, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Not a proper file: ~s", [RelPath]);
event([deploy, distribute, _Name, {error, file_exists, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort(
        "Destination ~s already exists (use --force to overwrite)",
        [RelPath]
    );
event(Event) ->
    debug("[rebar3_grisp] ~p", [Event]).

% grisp_tools Handlers

download([deploy, package, download, {start, Size}], State) ->
    console("* Downloading package"),
    io:format("    0%"),
    State#{progress => {0, Size}};
download([deploy, package, download, {progress, Current}], #{progress := {Tens, Total}} = State) ->
    NewTens = round(Current / Total * 10),
    case NewTens > Tens of
        true -> io:format(" ~p%", [NewTens * 10]);
        false -> ok
    end,
    State#{progress => {NewTens, Total}};
download([deploy, package, download, {complete, _ETag}], State) ->
    io:format(" OK~n"),
    State;
download([deploy, package, download, '_skip'], State) ->
    io:format("    (file cached)~n"),
    State;
download([deploy, package, download, {error, Reason}], State) ->
    warn("Download error: ~n~p", [Reason]),
    io:format("    (using cached file)~n"),
    State;
download([deploy, package, download, {progress, _Size}], State) ->
    State;
download(Event, State) ->
    debug("[rebar3_grisp] ~p", [Event]),
    State.

release_handler(#{name := Name, version := Version, erts := Root}, RState) ->
    OriginalArgs = rebar_state:command_args(RState),
    RelArgs = rel_args(Name, Version, OriginalArgs),
    debug("ARGS: ~p", [RelArgs]),
    debug("ROOT: ~p", [Root]),
    RState2 = rebar_state:command_args(RState, RelArgs),
    RState3 = rebar_state:set(RState2, relx, [
        {include_erts, binary_to_list(Root)},
        {system_libs, binary_to_list(filename:join(Root, "lib"))},
        {extended_start_script, false},
        {dev_mode, false}
        |rebar_state:get(RState2, relx, [])
    ]),
    {ok, RState4} = rebar_prv_release:do(RState3),
    Dir = filename:join([rebar_dir:base_dir(RState), "rel", Name]),
    {#{dir => Dir}, rebar_state:command_args(RState4, OriginalArgs)}.

% Utility functions

compile_for_grisp(Config, State) ->
    Providers = rebar_state:providers(State),
    CompileProvider = providers:get_provider(compile, Providers),
    Board = rebar3_grisp_util:platform(Config),
    GrispEnvs = [{"GRISP", "yes"},
                 {"GRISP_PLATFORM", atom_to_list(Board)}],
    ConfigEnvs = rebar_state:get(State, shell_hooks_env, []),
    State1 = rebar_state:set(State, shell_hooks_env, GrispEnvs ++ ConfigEnvs),
    {ok, State2} = providers:do(CompileProvider, State1),
    State2.

rel_args(Name, Version, Args) ->
    RelArgs = case lists:splitwith(fun("--") -> false; (_) -> true end, Args) of
        {_, ["--"|Rest]} -> Rest;
        {_, _}           -> []
    end,
    ["-n", Name, "-v", Version|RelArgs] -- ["-h", "--help", "--version"].

get_option(Arg, ConfigKey, State, Default) ->
    get_arg_option(Arg, State, fun(Config) ->
        rebar3_grisp_util:get(ConfigKey, Config, Default)
    end).

get_arg_option(Arg, State, Fun) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, grisp, []),
    case proplists:get_value(Arg, Args) of
        undefined -> Fun(Config);
        Value     -> Value
    end.

trim(String) ->
    re:replace(String, "(^[\s\n\t]+|[\s\n\t]+$)", "", [global, {return, list}]).

short(Hash) -> string:slice(Hash, 0, 8).
