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
                "provided. Options passed after '--' is sent to the Rebar 3 "
                "release task.\n"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    Config = rebar3_grisp_util:config(RState),
    OTPVersion = rebar3_grisp_util:otp_version(Config),
    Board = rebar3_grisp_util:board(Config),
    Destination = get_option(destination, [deploy, destination], RState),
    PreScript = get_option(pre_script, [deploy, pre_script], RState, undefined),
    PostScript = get_option(pre_script, [deploy, post_script], RState, undefined),

    {Args, _} = rebar_state:command_parsed_args(RState),
    RelName = proplists:get_value(relname, Args),
    RelVsn = proplists:get_value(relvsn, Args),
    Force = proplists:get_value(force, Args, false),

    ProjectRoot = rebar_dir:root_dir(RState),
    Apps = rebar3_grisp_util:apps(RState),

    try
        State = grisp_tools:deploy(#{
            project_root => ProjectRoot,
            otp_version => OTPVersion,
            board => Board,
            apps => Apps,
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
                shell => {fun shell_handler/2, #{}},
                release => {fun release_handler/2, RState}
            }),
            scripts => #{
                pre_script => PreScript,
                post_script => PostScript
            }
        }),
        #{release := RState2} = grisp_tools:handlers_finalize(State),
        {ok, RState2}
    catch
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
                "  - There is no pre-built package matching this version:~n"
                "      ~s",
                [Version, Hash]
            );
        error:{otp_version_mismatch, Target, Current} ->
            abort(
                "Current Erlang version (~p) does not match target "
                "Erlang version (~p)", [Current, Target]
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
            )
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

% TODO: Add state to event handler

event_handler(Event, State) ->
    case Event of
        {package, {download_progress, _Size}} ->
            ok;
        Event ->
            debug("[rebar3_grisp] ~p", [Event])
    end,
    {ok, event_handler1(Event, State)}.

event_handler1({otp_type, Hash, custom_build}, State) ->
    console("* Using custom OTP (~s)", [short(Hash)]),
    State;
event_handler1({otp_type, Hash, package}, State) ->
    console("* Downloading pre-built OTP package (~s)", [short(Hash)]),
    State;
% event_handler1({package, {download_init, _}}, State) ->
%     console("* Downloading prebuilt OTP package"),
%     State;
event_handler1({package, {download_start, Size}}, State) ->
    io:format("    0%"),
    State#{progress => {0, Size}};
event_handler1({package, {download_progress, Current}}, #{progress := {Tens, Total}} = State) ->
    NewTens = round(Current / Total * 10),
    case NewTens > Tens of
        true -> io:format(" ~p%", [NewTens * 10]);
        false -> ok
    end,
    State#{progress => {NewTens, Total}};
event_handler1({package, {download_complete, _ETag}}, State) ->
    io:format(" OK~n"),
    State;
event_handler1({package, download_cached}, State) ->
    console("* Cached file is up to date"),
    State;
event_handler1({package, {http_error, Other}}, State) ->
    warn("* Download error: ~n~p", [Other]),
    console("* Using cached file"),
    State;
event_handler1({package, http_timeout}, State) ->
    error("Download timed out"),
    State;
event_handler1({package, {extract, up_to_date}}, State) ->
    console("* Current package up to date"),
    State;
event_handler1({package, {extract, {start, _Package}}}, State) ->
    console("* Extracting package"),
    State;
event_handler1({package, {extract_failed, Reason}}, _State) ->
    abort("Tar extraction failed: ~p", [Reason]);
event_handler1({deployment, script, Name, {run, _Script}}, State) ->
    console("* Running ~p", [Name]),
    State;
event_handler1({deployment, script, _Name, {result, Output}}, State) ->
    case trim(Output) of
        ""      -> ok;
        Trimmed -> console(Trimmed)
    end,
    State;
event_handler1({deployment, release, {copy, _Source, _Target}}, State) ->
    console("* Copying release..."),
    State;
event_handler1({deployment, {files, {init, _Dest}}}, State) ->
    console("* Copying files..."),
    State;
event_handler1({deployment, files, {copy_error, {exists, File}}}, _State) ->
    abort(
        "Destination ~s already exists (use --force to overwrite)",
        [File]
    );
event_handler1({deployment, done}, #{name := Name, version := Vsn} = State) ->
    info("Deployment of ~s-~s complete", [Name, Vsn]),
    State;
event_handler1(_Event, State) ->
    State.

shell_handler(Command, State) ->
    {ok, Output} = rebar3_grisp_util:sh(Command),
    {Output, State}.

release_handler(#{name := Name, version := Version, erts := Root}, RState) ->
    OriginalArgs = rebar_state:command_args(RState),
    RelArgs = rel_args(Name, Version, OriginalArgs),
    debug("ARGS: ~p", [RelArgs]),
    debug("ROOT: ~p", [Root]),
    RState2 = rebar_state:command_args(RState, RelArgs),
    RState3 = rebar_state:set(RState2, relx, [
        {include_erts, Root},
        {system_libs, filename:join(Root, "lib")},
        {extended_start_script, false},
        {dev_mode, false}
        |rebar_state:get(RState2, relx, [])
    ]),
    {ok, RState4} = rebar_prv_release:do(RState3),
    Dir = filename:join([rebar_dir:base_dir(RState), "rel", Name]),
    {#{dir => Dir}, rebar_state:command_args(RState4, OriginalArgs)}.

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
