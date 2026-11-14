-module(rebar3_grisp_rust_tests).
-include_lib("eunit/include/eunit.hrl").

optional_cargo_test_() ->
    {timeout, 30, fun optional_cargo/0}.

optional_cargo() ->
    Root = filename:join("/tmp", "grisp-no-cargo-" ++ integer_to_list(
        erlang:unique_integer([positive]))),
    %% Use a fresh VM so developer checkouts cannot hide a mandatory dependency.
    Paths = [Path || Path <- code:get_path(), not lists:any(fun(Module) ->
        filelib:is_regular(filename:join(Path, atom_to_list(Module) ++ ".beam"))
    end, [cargo_opts, rebar3_cargo_opts])],
    {Directories, Embedded} = lists:partition(fun filelib:is_dir/1, Paths),
    {ok, Peer, _} = peer:start_link(#{connection => standard_io, args => ["+S", "2"]}),
    try
        true = peer:call(Peer, code, set_path, [Directories]),
        %% Rebar's escript archive is private to the parent VM's loader.
        %% Transfer its actual beams, retaining ordinary dependency paths.
        lists:foreach(fun({Name, File, _Loaded}) ->
            case is_list(File) andalso lists:member(filename:dirname(File), Embedded) of
                true ->
                    Module = list_to_atom(Name),
                    {Module, Binary, Origin} = code:get_object_code(Module),
                    {module, Module} = peer:call(Peer, code, load_binary, [Module, Origin, Binary]);
                false -> ok
            end
        end, code:all_available()),
        ?assertEqual(non_existing, peer:call(Peer, code, which, [cargo_opts])),
        ?assertEqual(non_existing, peer:call(Peer, code, which, [rebar3_cargo_opts])),
        ok = filelib:ensure_dir(filename:join(Root, "probe")),
        {ok, App} = rebar_app_info:new(<<"probe">>, "1", Root),
        Base = rebar_state:project_apps(rebar_state:dir(rebar_state:new(), Root), [App]),
        lists:foreach(fun(Platform) -> lists:foreach(fun(Toolchain) ->
            State = rebar_state:set(Base, grisp,
                [{platform, Platform}, {build, [{toolchain, Toolchain}]}]),
            Prepared = peer:call(Peer, rebar3_grisp_rust, prepare, [State]),
            ?assertEqual([], rebar_state:get(Prepared, grisp_native_nifs)),
            ?assertEqual([App], rebar_state:project_apps(
                peer:call(Peer, rebar3_grisp_rust, restore, [Prepared])))
        end, [[{directory, filename:join(Root, "old-sdk")}],
              [{directory, filename:join(Root, "new-sdk")}],
              [{docker, "unused-image"}], []]) end, [grisp2, grisp_base, grisp_nano]),
        Empty = #{rust => #{inputs => []}, event_stack => [],
                  handlers => grisp_tools:handlers_init(#{})},
        ?assertEqual([], mapz:deep_get([build, native_nifs],
            peer:call(Peer, grisp_tools_rust, prepare, [Empty]))),
        ?assertException(error, {rust_cargo_unsupported, [_ | _]},
            peer:call(Peer, grisp_tools_rust, check_cargo, [])),
        ?assertException(error, {rust_cargo_unsupported, [_ | _]},
            peer:call(Peer, grisp_tools_rust_sdk, resolve,
                [{directory, filename:join(Root, "old-sdk")}, Root,
                 filename:join(Root, "rust"), #{}])),
        Overlay = filename:join([Root, "grisp", "grisp2", "common", "grisp.config"]),
        ok = filelib:ensure_dir(Overlay),
        ok = file:write_file(Overlay, <<"#{build => #{rust => #{supported => true}}}.\n">>),
        ok = file:write_file(filename:join(Root, "Cargo.toml"), <<"[package]\nname='probe'\n">>),
        ?assertThrow(rebar_abort, peer:call(Peer, rebar3_grisp_rust, prepare,
            [rebar_state:set(Base, grisp, [{platform, grisp2}])])),
        ?assertNot(filelib:is_dir(filename:join(Root, "_grisp")))
    after
        peer:stop(Peer),
        file:del_dir_r(Root)
    end.

native_provider_context_test() ->
    Base = rebar_state:set(rebar_state:new(), profiles, [
        {grisp, [{cargo_opts, [{features, ["target-feature"]}]}]}
    ]),
    {ok, Build} = rebar3_grisp_build:init(Base),
    {ok, State} = rebar3_grisp_deploy:init(Build),
    Context = fun(Module) ->
        Provider = providers:get_provider_by_module(Module, rebar_state:providers(State)),
        ProfileState = rebar_state:apply_profiles(State, providers:profiles(Provider)),
        Cargo = rebar3_cargo_opts:cargo(rebar3_cargo_opts:from_state(ProfileState)),
        {rebar_dir:base_dir(ProfileState), cargo_opts:features(Cargo)}
    end,
    DeployContext = Context(rebar3_grisp_deploy),
    ?assertMatch({_, [<<"target-feature">>]}, DeployContext),
    ?assertEqual(DeployContext, Context(rebar3_grisp_build)).

unsupported_platforms_test() ->
    Root = filename:join("/tmp", "grisp-rust-platform-" ++ integer_to_list(
        erlang:unique_integer([positive]))),
    try
        Manifest = filename:join(Root, "Cargo.toml"),
        ok = filelib:ensure_dir(Manifest),
        ok = file:write_file(Manifest, <<"[package]\nname = 'probe'\n">>),
        {ok, App} = rebar_app_info:new(<<"probe">>, "1", Root),
        State = rebar_state:project_apps(rebar_state:new(), [App]),
        lists:foreach(fun(Platform) ->
            case Platform of
                unknown_platform -> ok;
                _ ->
                    Config = filename:join([Root, "grisp", atom_to_list(Platform),
                                            "common", "grisp.config"]),
                    ok = filelib:ensure_dir(Config),
                    ok = file:write_file(Config, <<"#{build => #{rust => #{supported => false}}}.\n">>)
            end,
            S = rebar_state:set(State, grisp, [{platform, Platform}]),
            ?assertThrow(rebar_abort, rebar3_grisp_rust:prepare(S))
        end, [grisp_nano, grisp_base, unknown_platform]),
        Empty = rebar_state:set(rebar_state:new(), grisp, [{platform, grisp_nano}]),
        ?assertEqual([], rebar_state:get(rebar3_grisp_rust:prepare(Empty), grisp_native_nifs))
    after
        file:del_dir_r(Root)
    end.

missing_sdk_support_test() ->
    Root = filename:join("/tmp", "grisp-old-sdk-" ++ integer_to_list(erlang:unique_integer([positive]))),
    try
        Manifest = filename:join(Root, "Cargo.toml"),
        ok = filelib:ensure_dir(Manifest),
        ok = file:write_file(Manifest, <<"[package]\nname='probe'\n">>),
        Overlay = filename:join(Root, "grisp/test_board/common/grisp.config"),
        ok = filelib:ensure_dir(Overlay),
        ok = file:write_file(Overlay, <<"#{build => #{rust => #{supported => true, sdk => #{}, cargo_opts => [{target, \"test-target\"}], nm => \"target-nm\"}}}.\n">>),
        {ok, App} = rebar_app_info:new(<<"probe">>, "1", Root),
        State = rebar_state:project_apps(rebar_state:dir(rebar_state:new(), Root), [App]),
        Config = [{platform, test_board}, {otp, [{version, "29.1.2"}]},
                  {build, [{toolchain, [{directory, filename:join(Root, "old-sdk")}]}]}],
        S = rebar_state:set(State, grisp, Config),
        ?assertThrow(rebar_abort, rebar3_grisp_rust:prepare(S)),
        %% The same SDK remains valid for an application without Rust inputs.
        NoRust = rebar_state:project_apps(S, []),
        ?assertEqual([], rebar_state:get(rebar3_grisp_rust:prepare(NoRust), grisp_native_nifs))
    after file:del_dir_r(Root) end.

strip_dependency_artifacts_test() ->
    Root = filename:join("/tmp", "grisp-dependency-release-" ++
                         integer_to_list(erlang:unique_integer([positive]))),
    File = fun(App, Crate) ->
        filename:join([Root, "lib", App ++ "-1.0.0", "priv", "crates", Crate, Crate ++ ".so"])
    end,
    Files = [File(App, Crate) || App <- ["root", "dependency"],
                               Crate <- ["bundle", "first", "second", "unrelated", "build-script-build"]],
    lists:foreach(fun(Path) ->
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, <<"host library">>)
    end, Files),
    State = rebar_state:set(rebar_state:new(), grisp_native_nifs,
        [#{load_app => root, name => <<"bundle">>,
           symbols => [<<"first">>, <<"second">>],
           load_apps => #{<<"first">> => root, <<"second">> => dependency},
           host_artifacts => [#{app => App, path => filename:join(["priv", "crates", Crate])}
               || {App, Crate} <- [{root, "bundle"}, {root, "first"}, {dependency, "second"},
                                   {root, "build-script-build"}, {dependency, "build-script-build"}]]}]),
    try
        ok = rebar3_grisp_rust:strip_release(Root, State),
        Removed = [File("root", "bundle"), File("root", "first"), File("dependency", "second"),
                   File("root", "build-script-build"), File("dependency", "build-script-build")],
        lists:foreach(fun(Path) -> ?assertNot(filelib:is_file(Path)) end, Removed),
        lists:foreach(fun(Path) -> ?assert(filelib:is_file(Path)) end, Files -- Removed)
    after
        file:del_dir_r(Root)
    end.

strip_host_artifacts_test() ->
    Root = filename:join(
        "/tmp", "grisp-release-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Priv = filename:join([Root, "lib", "load_owner-1.0.0", "priv"]),
    Host = filename:join([Priv, "crates", "example", "example.so"]),
    Asset = filename:join([Priv, "assets", "data.txt"]),
    Source = filename:join(Root, "source-priv"),
    SourceHost = filename:join([Source, "crates", "example", "example.so"]),
    lists:foreach(fun(F) ->
        ok = filelib:ensure_dir(F),
        ok = file:write_file(F, <<"keep">>)
    end, [Host, Asset, SourceHost]),
    State = rebar_state:set(
        rebar_state:new(),
        grisp_native_nifs,
        [#{app => build_owner, load_app => load_owner, name => <<"example">>,
           host_artifacts => [#{app => load_owner, path => "priv/crates/example"}]}]
    ),
    try
        ok = rebar3_grisp_rust:strip_release(Root, State),
        ?assertNot(filelib:is_file(Host)),
        ?assert(filelib:is_file(Asset)),
        ?assert(filelib:is_file(SourceHost)),
        ok = file:del_dir_r(Priv),
        ok = file:make_symlink(Source, Priv),
        ?assertException(
            error,
            {native_release_symlink, _},
            rebar3_grisp_rust:strip_release(Root, State)
        ),
        ?assert(filelib:is_file(SourceHost)),
        ok = file:delete(Priv)
    after
        case file:read_link(Priv) of
            {ok, _} -> file:delete(Priv);
            _ -> ok
        end,
        file:del_dir_r(Root)
    end.
