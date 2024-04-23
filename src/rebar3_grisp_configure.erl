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
    Opts = lists:map(fun({_, Key, Type, Descr}) ->
                             [Flag | _] = atom_to_list(Key),
                             {Key, Flag, atom_to_list(Key), Type, Descr}
                     end, grisp_tools_configure:settings()),
    Provider = providers:create([
        {namespace, grisp},
        {name, configure},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp configure"},
        {opts, Opts},
        {profiles, [default]},
        {short_desc, "Create and configure a new GRiSP app"},
        {desc, "Create and configure a new GRiSP app with user provided values"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp),

    try
        {Opts, _Rest} = rebar_state:command_parsed_args(RState),
        Config = rebar3_grisp_util:config(RState),

        CustomBuild = rebar3_grisp_util:should_build(Config),
        Board = rebar3_grisp_util:platform(Config),
        Version = rebar3_grisp_util:otp_version(Config),

        Apps = rebar3_grisp_util:apps(RState),

        ProjectRoot = rebar_dir:root_dir(RState),

        ReportDir = rebar3_grisp_util:report_dir(RState),

        InitFlags = maps:from_list([
                                    {Key, rebar3_grisp_util:get(Key, Opts, D)}
                                    || {_, Key, {_, D}, _} <- 
                                       grisp_tools_configure:settings()
                                   ]),
        InitState = #{
            project_root => ProjectRoot,
            report_dir => ReportDir,
            flags => InitFlags,
            apps => Apps,
            otp_version_requirement => Version,
            custom_build => CustomBuild,
            platform => Board,
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{}},
                shell => {fun rebar3_grisp_handler:shell/3, #{}}
            }),
            binaries => []},
        State = grisp_tools:configure(InitState),
        % TODO
        % run or call rebar3 new app NAME
        %   With open port
        % render templates with grisp_tools_templates:render/2
        % overwrite files in the new project directory

        #{flags := Flags} = State,
        io:format("~p~n", [maps:to_list(Flags)]),

        case maps:get(network, Flags) of
            false -> rebar_templater:new("grispapp", maps:to_list(Flags), false, RState);
            true -> rebar_templater:new("grispnetapp", maps:to_list(Flags), false, RState)
        end,

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
event_handler(Event, State) ->
    event(Event),
    {ok, State}.
event(_) ->
    info("Unexpected event").
