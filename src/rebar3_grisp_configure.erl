-module(rebar3_grisp_configure).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    console/1,
    console/2,
    info/1,
    abort/2
]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, grisp},
        {name, configure},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp configure"},
        {opts, [
                {name, $n, "name", {string, "robot"},
                 "The name of your GRiSP application"},
                {yes, $y, "yes", {boolean, false},
                 "Provides all default values"},
                {version, $v, "version", {string, "25"},
                 "The OTP version of the GRiSP app"},
                {network, $n, "network", {boolean, true},
                 "Network configuration files generation"}
               ]},
        {profiles, [default]},
        {short_desc, "Create and configure a new GRiSP app"},
        {desc,
"Create and configure a new GRiSP app using user provided values
"
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp),
    
    % TODO Erl version
    % TODO Network configuration
    
    try
        {Opts, _Rest} = rebar_state:command_parsed_args(RState),
        Config = rebar3_grisp_util:config(RState),

        CustomBuild = rebar3_grisp_util:should_build(Config),
        Board = rebar3_grisp_util:platform(Config),
        Version = rebar3_grisp_util:otp_version(Config),

        Apps = rebar3_grisp_util:apps(RState),

        ProjectRoot = rebar_dir:root_dir(RState),

        ReportDir = rebar3_grisp_util:report_dir(RState),

        Flags = maps:from_list([
                                {F, rebar3_grisp_util:get(F, Opts, D)}
                                || {F, D} <- [{name, robot}, {interactive, true}]
                               ]),
        InitState = #{
            project_root => ProjectRoot,
            report_dir => ReportDir,
            flags => Flags,
            apps => Apps,
            otp_version_requirement => Version,
            custom_build => CustomBuild,
            platform => Board,
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{}},
                shell => {fun rebar3_grisp_handler:shell/3, #{}}
            }),
            binaries => []},
        State = grisp_tools_util:weave(InitState, [fun name/1,
                                                   fun configure_version/1,
                                                   fun grisp_tools:configure/1]),
        _ = grisp_tools:handlers_finalize(State),
        {ok, RState}
    catch
        error:E ->
            abort(
                "Unexpected error: ~p",
                [E]
            )
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%--- Internal ------------------------------------------------------------------
configure_version(State) ->
    Prompt = "Erlang version",
    Default = "25",
    Version = grisp_tools_io:ask(Prompt, string, Default),
    State.

name(State) ->
    % TODO
    State.

event_handler(Event, State) ->
    event(Event),
    {ok, State}.
event(_) ->
    info("Unexpected event").
