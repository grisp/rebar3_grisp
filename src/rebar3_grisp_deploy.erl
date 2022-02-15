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

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, deploy},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}, {default, compile}]},
            {example, "rebar3 grisp deploy"},
            {opts, [
                {relname, $n, "relname", string, "Specify the name for the release that will be deployed"},
                {relvsn, $v, "relvsn", string, "Specify the version of the release"},
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
    Destination = get_option(destination, [deploy, destination], RState),
    PreScript = get_option(pre_script, [deploy, pre_script], RState, undefined),
    PostScript = get_option(pre_script, [deploy, post_script], RState, undefined),

    {Args, _} = rebar_state:command_parsed_args(RState),
    Force = proplists:get_value(force, Args, false),

    ProjectRoot = rebar_dir:root_dir(RState),
    Apps = rebar3_grisp_util:apps(RState),

    try
        {RelName, RelVsn} = select_release(Args, RState),
        State = grisp_tools:deploy(#{
            project_root => ProjectRoot,
            apps => Apps,
            otp_version_requirement => OTPVersion,
            platform => Board,
            custom_build => rebar3_grisp_util:should_build(Config),
            copy => #{
                force => Force,
                destination => Destination
            },
            release => #{
                name => RelName,
                version => RelVsn
            },
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{
                    name => RelName,
                    version => RelVsn
                }},
                shell => {fun rebar3_grisp_handler:shell/3, #{}},
                release => {fun release_handler/2, RState}
            }),
            scripts => #{
                pre_script => PreScript,
                post_script => PostScript
            }
        }),
        #{release := RState2} = grisp_tools:handlers_finalize(State),
        info("Deployment done"),
        {ok, RState2}
    catch
        error:{release_not_selected, [Rel|_]} ->
            {Name, Version} = element(2, Rel),
            abort(
                "Multiple releases defined!~n"
                "You must specify a name and version with -n and -v. Example:~n"
                "~n"
                "    rebar3 grisp release -n ~p -v ~s~n",
                [Name, Version]
            );
        error:no_release_configured ->
            App = rebar_app_info:name(hd(rebar_state:project_apps(RState))),
            abort(
                "No release configured! Deploy aborted~n"
                "~n"
                "You must specify at least one release in 'rebar.config' to be "
                "able to deploy~nyour project. Example:~n"
                "~n"
                "    {relx,~n"
                "        {~s, \"0.1.0\", [~s]}~n"
                "    }.~n",
                [App, App]
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
            Root = rebar_dir:root_dir(RState),
            {ok, Relative} = rebar_file_utils:path_from_ancestor(File, Root),
            abort(
                "Error rendering ~s:~nmissing template key: ~s",
                [Relative, Key]
            );
        error:{template_error, File, {include_not_found, Include}} ->
            Root = rebar_dir:root_dir(RState),
            {ok, Relative} = rebar_file_utils:path_from_ancestor(File, Root),
            abort(
                "Error rendering ~s:~nmissing include file: ~s",
                [Relative, Include]
            );
        error:{create_dir_failed, Dir, {error, Reason}} ->
            abort(
                "Error creating directory ~s: ~s",
                [Dir, file:format_error(Reason)]
            )
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

% grisp_tools Events

event_handler(Event, State) ->
    case Event of
        [deploy, package, download|_] ->
            {ok, download(Event, State)};
        _ ->
            event(Event),
            {ok, State}
    end.

event([deploy, validate, version, {mismatch, Target, Current}]) ->
    abort(
        "Current Erlang version (~p) does not match target "
        "Erlang version (~p)", [Current, Target]
    );
event([deploy, collect, {hash, Hash, Index}]) ->
    debug("GRiSP hash:~n~s~n~n~p", [Hash, Index]);
event([deploy, package, {type, {custom_build, Hash}}]) ->
    console("* Using custom OTP (~s)", [short(Hash)]);
event([deploy, package, {type, {package, Hash}}]) ->
    console("* Downloading pre-built OTP package (~s)", [short(Hash)]);
event([deploy, package, extract]) ->
    console("* Extracting package");
event([deploy, package, extract, '_skip']) ->
    io:format("    (already extracted)~n");
event([deploy, package, extract, {error, Reason}]) ->
    abort("Extraction failed: ~p", [Reason]);
event([deploy, copy, Name, {run, _Script}]) ->
    console("* Running ~p", [Name]);
event([deploy, copy, _Name, {result, Output}]) ->
    case trim(Output) of
        ""      -> ok;
        Trimmed -> console(Trimmed)
    end;
event([deploy, copy, release, {error, target_dir_missing, Target}]) ->
    abort("Target directory missing: ~s", [Target]);
event([deploy, copy, release, {copy, _Source, _Target}]) ->
    console("* Copying release...");
event([deploy, copy, files, {init, _Dest}]) ->
    console("* Copying files...");
event([deploy, copy, files, {copy, #{app := App, target := File}}]) ->
    io:format("    [~p] ~s~n", [App, File]);
event([deploy, copy, files, {error, {exists, File}}]) ->
    abort(
        "Destination ~s already exists (use --force to overwrite)",
        [File]
    );
event(Event) ->
    debug("[rebar3_grisp] ~p", [Event]).

% grisp_tools Handlers

download([deploy, package, download, {start, Size}], State) ->
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

select_release(Args, RState) ->
    Relx = rebar_state:get(RState, relx, []),

    Releases = [Release || Release <- Relx, element(1, Release) == 'release'],

    case Releases of
        [Release] ->
            {RelName, RelVsn} = element(2, Release),
            {atom_to_list(RelName), RelVsn};
        [_|_] ->
            RelName = proplists:get_value(relname, Args),
            RelVsn = proplists:get_value(relvsn, Args),

            case {RelName, RelVsn} of
                {N, V} when N == undefined; V == undefined ->
                    error({release_not_selected, Releases});
                Other ->
                    Other
            end;
        [] ->
            error(no_release_configured)
    end.

rel_args(Name, Version, Args) ->
    RelArgs = case lists:splitwith(fun("--") -> false; (_) -> true end, Args) of
        {_, ["--"|Rest]} -> Rest;
        {_, _}           -> []
    end,
    ["-n", Name, "-v", Version|RelArgs] -- ["-h", "--help", "--version"].

get_option(Arg, ConfigKey, State) ->
    get_arg_option(Arg, State, fun(Config) ->
        rebar3_grisp_util:get(ConfigKey, Config)
    end).

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
