-module(rebar3_grisp_configure).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    console/1,
    console/2,
    info/1,
    info/2,
    debug/1,
    debug/2,
    abort/2
]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Opts = lists:map(fun({_, {Key, Short}, Type, Descr}) ->
                             Long = atom_to_list(Key),
                             {Key, Short, Long, Type, Descr}
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

        Provider = providers:get_provider(configure,
                                          rebar_state:providers(RState),
                                          grisp),
        ProviderOpts = lists:map(fun({Key, Short, Long, {Type, _}, Descr}) ->
                                         {Key, Short, Long, Type, Descr}
                                 end, providers:opts(Provider)),
        CmdArgs = rebar_state:command_args(RState),
        {ok, {UserOpts, _}} = getopt:parse(ProviderOpts,
                                           lists:join(" ", CmdArgs)),
        UserOptsMap = maps:from_list(UserOpts),

        Config = rebar3_grisp_util:config(RState),

        CustomBuild = rebar3_grisp_util:should_build(Config),
        Board = rebar3_grisp_util:platform(Config),
        Version = rebar3_grisp_util:otp_version(Config),

        Apps = rebar3_grisp_util:apps(RState),

        ProjectRoot = rebar_dir:root_dir(RState),

        ReportDir = rebar3_grisp_util:report_dir(RState),

        InitFlags = maps:from_list([
                                    {Key, rebar3_grisp_util:get(Key, Opts, D)}
                                    || {_, {Key, _}, {_, D}, _} <-
                                       grisp_tools_configure:settings()
                                   ]),
        InitState = #{
            project_root => ProjectRoot,
            report_dir => ReportDir,
            flags => InitFlags,
            user_opts => UserOptsMap,
            apps => Apps,
            otp_version_requirement => Version,
            custom_build => CustomBuild,
            platform => Board,
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{}},
                shell => {fun rebar3_grisp_handler:shell/3, #{}}
            }),
            binaries => []},


        case OptsMap = maps:from_list(Opts) of
            #{interactive := false} ->
                check_custom_params(OptsMap, UserOptsMap);
            _ -> ok
        end,

        State = grisp_tools:configure(InitState),

        #{flags := Flags} = State,

        Files = templater(Flags),

        {ok, Cwd} = file:get_cwd(),
        PrivDir = code:priv_dir(rebar3_grisp),
        TemplatesDir = filename:join(PrivDir, "templates"),
        lists:map(fun({From, To}) ->
                      FilePath = filename:join(TemplatesDir, From),
                      maybe_write_file(FilePath, filename:join(Cwd, To), Flags)
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
event({say, Prompt}) ->
    console(Prompt);
event({info, Prompt}) ->
    info(Prompt);
event(_) ->
    info("Unexpected event").

check_custom_params(OptsMap, _)
  when not map_get(network, OptsMap) andalso
       (map_get(wifi, OptsMap) orelse
        map_get(grisp_io, OptsMap) orelse
        map_get(epmd, OptsMap)) ->
    abort("The network configuration needs to be enabled with
          '--network=true' or '-n true' if you want to setup
          either wifi, grisp.io or epmd in non-interactive mode", []);
check_custom_params(OptsMap, UserOptsMap)
  when not map_get(wifi, OptsMap) andalso
       (is_map_key(ssid, UserOptsMap) orelse is_map_key(psk, UserOptsMap)) ->
    abort("The wifi configuration needs to be enabled with
          '--wifi=true' or '-w true' if you want to setup the ssid or the psk",
          []);
check_custom_params(OptsMap, UserOptsMap)
  when not map_get(grisp_io, OptsMap) andalso is_map_key(token, UserOptsMap) ->
    abort("The grisp.io configuration needs to be enabled with
          '--grisp_io=true' or '-g true' if you want to setup the token",
          []);
check_custom_params(OptsMap, UserOptsMap)
  when not map_get(epmd, OptsMap) andalso is_map_key(cookie, UserOptsMap) ->
    abort("The epmd configuration needs to be enabled with
          '--epmd=true' or '-e true' if you want to setup the cookie",
          []);
check_custom_params(_, _) ->
    ok.

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
     {"common/rebar.config", Name ++ "/rebar.config"}]
    ++ templater_network(Flags).

templater_network(#{name := Name, network := true} = Flags) ->
    [{"network/grisp.ini.mustache",
      Name ++ "/grisp/grisp2/common/deploy/files/grisp.ini.mustache"}]
   ++ templater_wifi(Flags)
   ++ templater_grisp_io(Flags);
templater_network(_) ->
    [].

templater_wifi(#{name := Name, wifi := true}) ->
    [{"network/wpa_supplicant.conf",
      Name ++ "/grisp/default/common/deploy/files/wpa_supplicant.conf"}];
templater_wifi(_) ->
    [].

templater_grisp_io(#{name := Name, grisp_io := false}) ->
    [{"network/erl_inetrc",
      Name ++ "/grisp/default/common/deploy/files/erl_inetrc"}];
templater_grisp_io(_) ->
    [].

maybe_write_file(In, Out, Params) ->
    case filelib:is_regular(Out) of
        false ->
            filelib:ensure_dir(Out),
            debug("Rendering template file: ~p", [In]),
            FileContent = grisp_tools_template:render(In, Params),
            debug("Writing output file: ~p", [Out]),
            file:write_file(Out, FileContent);
        true -> info("File already exists: ~p~n", [Out])
    end.
