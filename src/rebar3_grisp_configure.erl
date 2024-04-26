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

-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).

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

        #{flags := Flags} = State,

        Files = templater(Flags),

        {ok, Cwd} = file:get_cwd(),
        PrivDir = code:priv_dir(rebar3_grisp),
        TemplatesDir = filename:join(PrivDir, "templates"),
        lists:map(fun({From, To}) ->
                          FilePath = filename:join(TemplatesDir, From),
                          case file:read_file(FilePath) of
                              {ok, Bin} ->
                                  maybe_write_file(filename:join(Cwd, To), Bin, Flags);
                              {error, Reason} ->
                                  error(Reason)
                          end
                  end, Files),

        _ = grisp_tools:handlers_finalize(State),
        {ok, RState}
    catch
        error:E ->
            abort(
                "Unexpected error: ~p~n",
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

-spec templater(map()) -> [{Src, Dest}] when
      Src  :: string(),
      Dest :: string().
templater(#{name := Name} = Flags) ->
    [{"common/app.erl", Name ++ "/src/" ++ Name ++ ".erl"},
     {"common/sup.erl", Name ++ "/src/" ++ Name ++ "_sup.erl"},
     {"common/gitignore", Name ++ "/.gitignore"},
     {"common/LICENSE", Name ++ "/LICENSE"},
     {"common/README.md", Name ++ "/README.md"},
     {"common/otp_app.app.src", Name ++ "/src/" ++ Name ++ ".app.src"},
     {"common/sys.config", Name ++ "/config/sys.config"},
     {"common/rebar.config", Name ++ "/rebar.config"}] ++ templater_network(Flags).

templater_network(#{name := Name, network := true} = Flags) ->
    [{"network/grisp.ini.mustache", Name ++ "/grisp/grisp2/common/deploy/files/grisp.ini.mustache"}]
   ++ templater_wifi(Flags)
   ++ templater_grisp_io(Flags);
templater_network(_) ->
    [].

templater_wifi(#{name := Name, wifi := true}) ->
    [{"network/wpa_supplicant.conf", Name ++ "/grisp/default/common/deploy/files/wpa_supplicant.conf"}];
templater_wifi(_) ->
    [].

templater_grisp_io(#{name := Name, grisp_io := false}) ->
    [{"network/erl_inetrc", Name ++ "/grisp/default/common/deploy/files/erl_inetrc"}];
templater_grisp_io(_) ->
    [].

maybe_write_file(File, Bin, Params) ->
    case filelib:is_regular(File) of
        false ->
            filelib:ensure_dir(File),
            FileContent = rebar_templater:render(Bin, Params),
            file:write_file(File, FileContent);
        true -> ?INFO("File already exists: ~p~n", [File]) % TODO use the loggin from rebar3
    end.
