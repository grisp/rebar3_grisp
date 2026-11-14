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
    abort/1,
    abort/2,
    warn/1,
    warn/2
]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Opts = lists:map(fun({Long, Short, TSpec, Desc}) ->
                         {Long, Short, atom_to_list(Long), TSpec, Desc}
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
        ProviderOpts = lists:map(fun({Key, Short, Long, {Type, _}, Desc}) ->
                                         {Key, Short, Long, Type, Desc}
                                 end, providers:opts(Provider)),
        CmdArgs = rebar_state:command_args(RState),
        {ok, {UserOpts, _}} = getopt:parse(ProviderOpts, CmdArgs),
        UserOptsMap = maps:from_list(UserOpts),

        Config = rebar3_grisp_util:config(RState),

        CustomBuild = rebar3_grisp_util:should_build(Config),
        Board = rebar3_grisp_util:platform(Config),
        Version = rebar3_grisp_util:otp_version(Config),

        Apps = rebar3_grisp_util:apps(RState),

        ProjectRoot = rebar_dir:root_dir(RState),

        ReportDir = rebar3_grisp_util:report_dir(RState),

        InitFlags = maps:from_list(
            [{Long, rebar3_grisp_util:get(Long, Opts, D)}
             || {Long, _, {_, D}, _} <- grisp_tools_configure:settings()]),
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

        #{flags := Flags, project_exists := ProjectExists} = State,

        Files = templater(Flags),

        {ok, Cwd} = file:get_cwd(),
        PrivDir = code:priv_dir(rebar3_grisp),
        TemplatesDir = filename:join(PrivDir, "templates"),
        FormatedFlags = template_flags(Flags),
        lists:map(fun({From, To}) ->
                      FilePath = filename:join(TemplatesDir, From),
                      maybe_write_file(FilePath,
                                       filename:join(Cwd, To),
                                       FormatedFlags)
          end, Files),

        case ProjectExists of
            true -> warn("Warning: The project directory already exists, "
                         ++ "and existing files were not overwritten. "
                         ++ "Please review the files to ensure all "
                         ++ "configurations are correct. ");
            _ -> ok
        end,

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
    Res = event(Event),
    {Res, State}.
event({say, Prompt}) ->
    console(Prompt);
event({info, Prompt}) ->
    info(Prompt);
event({error, Prompt}) ->
    abort(Prompt);
event({ask, Prompt}) ->
    io:get_line(Prompt);
event(Event) ->
    info("Event: ~p", [Event]),
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
  when not map_get(grisp_io, OptsMap) andalso
       is_map_key(grisp_io_linking, UserOptsMap) ->
    abort("The grisp.io integration needs to be enabled with
          '--grisp_io=true' or '-g true' if you want to activate the linking",
          []);
check_custom_params(OptsMap, UserOptsMap)
  when not map_get(grisp_io_linking, OptsMap) andalso is_map_key(token, UserOptsMap) ->
    abort("The grisp.io linking configuration needs to be enabled with
          '--grisp_io_linking=true' or '-l true' if you want to setup the token",
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
    ++ templater_network(Flags)
    ++ templater_rust(Flags)
    ++ templater_c_nif(Flags).

template_flags(Flags) ->
    Native = maps:get(rust, Flags, false) orelse maps:get(c_nif, Flags, false),
    Params = maps:map(fun(_, Value) when is_list(Value) -> unicode:characters_to_binary(Value);
                        (_, Value) -> Value end, Flags#{native => Native}),
    Params#{desc_literal => erlang_string(maps:get(desc, Params)),
            dest_literal => erlang_string(maps:get(dest, Params))}.

erlang_string(Value) ->
    unicode:characters_to_binary(io_lib:write_string(unicode:characters_to_list(Value))).

templater_c_nif(#{name := Name, c_nif := true}) ->
    [{"c_nif/nif.erl", Name ++ "/src/" ++ Name ++ "_c_nif.erl"},
     {"c_nif/nif.c", Name ++ "/grisp/default/common/build/nifs/" ++ Name ++ "_c_nif.c"}];
templater_c_nif(_) -> [].

templater_rust(#{name := Name, rust := true}) ->
    [{"rust/nif.erl", Name ++ "/src/" ++ Name ++ "_rust_nif.erl"},
     {"rust/workspace.toml", Name ++ "/native/Cargo.toml"},
     {"rust/Cargo.toml", Name ++ "/native/" ++ Name ++ "_native/Cargo.toml"},
     {"rust/lib.rs", Name ++ "/native/" ++ Name ++ "_native/src/lib.rs"},
     {"rust/config.toml", Name ++ "/.cargo/config.toml"}];
templater_rust(_) -> [].

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
        true -> warn("File already exists: ~p~n", [Out])
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

text_templates_test() ->
    Defaults = maps:from_list([{Key, Value} || {Key, _, {_, Value}, _} <- grisp_tools_configure:settings()]),
    Desc = "GRiSP \"Demo\" \\ customer's " ++ [16#E9],
    Dest = "/media/GRiSP \"card\"\\test",
    Token = "link-token_123",
    Flags = Defaults#{name => "text_demo", desc => Desc, dest => Dest,
                      grisp_io => true, grisp_io_linking => true, token => Token},
    Params = template_flags(Flags),
    TemplateRoot = filename:join(code:priv_dir(rebar3_grisp), "templates"),
    Root = filename:join("/tmp", "grisp-text-generator-" ++ integer_to_list(erlang:unique_integer([positive]))),
    try
        ok = filelib:ensure_path(Root),
        lists:foreach(fun(File) ->
            ok = file:write_file(filename:join(Root, File),
                grisp_tools_template:render(filename:join([TemplateRoot, "common", File]), Params))
        end, ["otp_app.app.src", "rebar.config", "sys.config"]),
        {ok, [{application, text_demo, App}]} = file:consult(filename:join(Root, "otp_app.app.src")),
        ?assertEqual(Desc, proplists:get_value(description, App)),
        {ok, Config} = file:consult(filename:join(Root, "rebar.config")),
        Grisp = proplists:get_value(grisp, Config),
        ?assertEqual(Dest, proplists:get_value(destination, proplists:get_value(deploy, Grisp))),
        {ok, [Sys]} = file:consult(filename:join(Root, "sys.config")),
        ?assertEqual(unicode:characters_to_binary(Token),
            proplists:get_value(device_linking_token, proplists:get_value(grisp_connect, Sys))),
        Readme = grisp_tools_template:render(filename:join([TemplateRoot, "common", "README.md"]), Params),
        ?assertNotEqual(nomatch, binary:match(Readme, unicode:characters_to_binary(Desc)))
    after file:del_dir_r(Root) end.

rust_templates_test() ->
    Defaults = maps:from_list([{Key, Value} || {Key, _, {_, Value}, _} <- grisp_tools_configure:settings()]),
    TemplateRoot = filename:join(code:priv_dir(rebar3_grisp), "templates"),
    Root = filename:join("/tmp", "grisp-rust-generator-" ++ integer_to_list(erlang:unique_integer([positive]))),
    try
        lists:foreach(fun({Rust, C}) ->
            Name = "demo_" ++ atom_to_list(Rust) ++ "_" ++ atom_to_list(C),
            Flags = Defaults#{name => Name, rust => Rust, c_nif => C},
            Params = template_flags(Flags),
            lists:foreach(fun({In, Out}) ->
                Path = filename:join(Root, Out),
                ok = filelib:ensure_dir(Path),
                ok = file:write_file(Path, grisp_tools_template:render(filename:join(TemplateRoot, In), Params))
            end, templater(Flags)),
            {ok, Config} = file:consult(filename:join([Root, Name, "rebar.config"])),
            ?assertEqual(Rust, filelib:is_regular(filename:join([Root, Name, "native", "Cargo.toml"]))),
            ?assertEqual(C, filelib:is_regular(filename:join([Root, Name, "grisp/default/common/build/nifs", Name ++ "_c_nif.c"]))),
            case C of
                true ->
                    {ok, CSource} = file:read_file(filename:join(
                        [Root, Name, "grisp/default/common/build/nifs", Name ++ "_c_nif.c"])),
                    ?assertNotEqual(nomatch, binary:match(CSource,
                        iolist_to_binary(["#define STATIC_ERLANG_NIF_LIBNAME ", Name, "_c"]))),
                    ?assertNotEqual(nomatch, binary:match(CSource,
                        iolist_to_binary(["ERL_NIF_INIT(", Name, "_c_nif"])));
                false -> ok
            end,
            ?assertEqual(Rust orelse C, proplists:is_defined(build, proplists:get_value(grisp, Config))),
            case Rust of
                true ->
                    {ok, Wrapper} = file:read_file(filename:join(
                        [Root, Name, "src", Name ++ "_rust_nif.erl"])),
                    ?assertNotEqual(nomatch, binary:match(Wrapper,
                        iolist_to_binary(["-module(", Name, "_rust_nif).\n"]))),
                    {ok, RustSource} = file:read_file(filename:join(
                        [Root, Name, "native", Name ++ "_native", "src/lib.rs"])),
                    ?assertNotEqual(nomatch, binary:match(RustSource,
                        iolist_to_binary(["rustler::init!(\"", Name, "_rust_nif\");"]))),
                    ?assertMatch([{rebar3_cargo, {git, "https://github.com/grisp/rebar3_cargo.git",
                                                       {branch, "cross_build_support_0_1_1"}}}], proplists:get_value(plugins, Config));
                false -> ?assertEqual(undefined, proplists:get_value(plugins, Config))
            end
        end, [{false, false}, {true, false}, {false, true}, {true, true}])
    after file:del_dir_r(Root) end.
-endif.
