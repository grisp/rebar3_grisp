-module(rebar3_grisp_rust).

-export([prepare/1, restore/1, strip_release/2]).

prepare(State) ->
    try do_prepare(State)
    catch
        error:Reason:Stack ->
            case format_error(Reason) of
                undefined -> erlang:raise(error, Reason, Stack);
                {Format, Args} -> rebar_api:abort(Format, Args)
            end
    end.

do_prepare(State) ->
    Apps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
    CargoApps = lists:filter(fun is_cargo_app/1, Apps),
    case CargoApps of
        [] ->
            rebar_state:set(State, grisp_native_nifs, []);
        _ ->
            Config = rebar3_grisp_util:config(State),
            Platform = rebar3_grisp_util:platform(Config),
            RustConfig = grisp_tools_rust:config(rebar3_grisp_util:apps(State), Platform),
            ok = check_frontend(),
            ok = grisp_tools_rust:check_cargo(),
            PlatformOpts = maps:get(cargo_opts, RustConfig, []),
            Defaults = PlatformOpts ++ maps:to_list(grisp_tools_rust:defaults()) ++
                [{copy_artifacts, false}],
            User = rebar3_grisp_util:get([build, rust], Config, []),
            lists:foreach(fun(Key) ->
                case proplists:is_defined(Key, User) of
                    true -> error({rust_sdk_override, Key});
                    false -> ok
                end
            end, [toolchain, target, bundler]),
            [Root | _] = rebar_state:project_apps(State),
            Project = filename:absname(rebar_dir:root_dir(State)),
            Version = unicode:characters_to_list(rebar3_grisp_util:otp_version(Config)),
            Directory = filename:join([Project, "_grisp", atom_to_list(Platform),
                                       "rust", Version]),
            Toolchain = case rebar3_grisp_util:toolchain_root(State) of
                {error, docker_not_found} -> error(docker_not_found);
                Selected -> Selected
            end,
            SDK = grisp_tools_rust_sdk:resolve(Toolchain, Project, Directory, RustConfig),
            Required = [{toolchain, undefined}, {target_dir, maps:get(target_dir, SDK)}],
            Previous = rebar_state:get(State, cargo_overrides, []),
            S0 = rebar_state:set(State, grisp_previous_cargo_overrides, Previous),
            S1 = rebar_state:set(S0, cargo_overrides, Required ++ User ++ Defaults ++ Previous),
            RootCargo = sdk_cargo_options(Root, S1, SDK),
            Inputs = [input(App, S1, SDK) || App <- CargoApps],
            Prepared = grisp_tools_rust:prepare(#{
                rust => #{app => app_name(Root), target_dir => cargo_opts:target_dir(RootCargo), inputs => Inputs,
                          directory => filename:join(Directory, "bundle"),
                          workspace_manifest => case is_cargo_app(Root) of
                              true -> filename:join(cargo_opts:path(RootCargo), "Cargo.toml");
                              false -> undefined
                          end,
                          sdk => SDK, nm => maps:get(nm, SDK), bundler => maps:get(bundler, SDK)},
                event_stack => [rust],
                handlers => grisp_tools:handlers_init(#{event => {fun event/2, #{}}})
            }),
            _ = grisp_tools:handlers_finalize(Prepared),
            Nifs = [host_artifacts(N) || N <- mapz:deep_get([build, native_nifs], Prepared)],
            HeaderState = rebar_state:set(S1, cargo_overrides,
                [{skip, true} | rebar_state:get(S1, cargo_overrides)]),
            Prepare = fun(App) -> prepare_app(App, HeaderState, Nifs) end,
            Deps = lists:map(Prepare, rebar_state:all_deps(S1)),
            Projects = lists:map(Prepare, rebar_state:project_apps(S1)),
            DepsToBuild = lists:map(Prepare, rebar_state:deps_to_build(S1)),
            WithApps = rebar_state:project_apps(rebar_state:all_deps(HeaderState, Deps), Projects),
            Built = lists:foldl(fun(App, S) -> rebar_state:deps_to_build(S, App) end, WithApps, DepsToBuild),
            rebar_state:set(Built, grisp_native_nifs, Nifs)
    end.

restore(State) ->
    Previous = rebar_state:get(
        State,
        grisp_previous_cargo_overrides,
        rebar_state:get(State, cargo_overrides, [])
    ),
    WithApps = rebar_state:project_apps(
        rebar_state:all_deps(State, lists:map(fun restore_app/1, rebar_state:all_deps(State))),
        lists:map(fun restore_app/1, rebar_state:project_apps(State))),
    Restored = lists:foldl(fun(App, S) -> rebar_state:deps_to_build(S, restore_app(App)) end,
                           WithApps, rebar_state:deps_to_build(State)),
    rebar_state:set(Restored, cargo_overrides, Previous).

restore_app(App) ->
    Opts = rebar_app_info:opts(App),
    case dict:find('$grisp_previous_cargo_overrides', Opts) of
        error -> App;
        {ok, Previous} ->
            Base = dict:erase('$grisp_previous_cargo_overrides', Opts),
            Restored = case Previous of
                error -> dict:erase(cargo_overrides, Base);
                {ok, Value} -> rebar_opts:set(Base, cargo_overrides, Value)
            end,
            rebar_app_info:opts(App, Restored)
    end.

is_cargo_app(App) ->
    Opts = rebar_opts:get(rebar_app_info:opts(App), cargo_opts, []),
    Dir = proplists:get_value(src_dir, Opts, "."),
    filelib:is_regular(filename:join([rebar_app_info:dir(App), Dir, "Cargo.toml"])).

check_frontend() ->
    Required = [
        {rebar3_cargo_opts, [{from_app, 2}, {cargo, 1}, {load_from_app, 1}]},
        {rebar3_cargo_util, [{cargo_init, 3}, {diagnostics, 1}]},
        {rebar3_cargo_compile, [{compile_app, 2}]}
    ],
    Missing = lists:flatmap(fun({Module, Functions}) ->
        _ = code:ensure_loaded(Module),
        [{Module, Function, Arity} || {Function, Arity} <- Functions,
            not erlang:function_exported(Module, Function, Arity)]
    end, Required),
    case Missing of
        [] -> ok;
        _ -> error({rust_plugin_unsupported, Missing})
    end.

cargo_options(App, State) ->
    rebar3_cargo_util:cargo_init(App, rebar3_cargo_opts:from_app(App, State), true).

sdk_cargo_options(App, State, SDK) ->
    Cargo = cargo_options(App, State),
    Env = lists:ukeysort(1, maps:get(env, SDK) ++ cargo_opts:env(Cargo)),
    Overrides = [{env, Env} | rebar_state:get(State, cargo_overrides, [])],
    cargo_options(App, rebar_state:set(State, cargo_overrides, Overrides)).

app_name(App) -> binary_to_atom(rebar_app_info:name(App), utf8).

input(App, State, SDK) ->
    Opts = rebar3_cargo_opts:from_app(App, State),
    LoadApp = case rebar3_cargo_opts:load_from_app(Opts) of
        undefined -> app_name(App);
        Other -> Other
    end,
    #{app => app_name(App), load_app => LoadApp, cargo => sdk_cargo_options(App, State, SDK)}.

prepare_app(App, State, Nifs) ->
    case is_cargo_app(App) of
        false -> App;
        true ->
            Built = rebar3_cargo_compile:compile_app(App, State),
            Name = app_name(App),
            Original = rebar_app_info:opts(Built),
            %% Rebar replaces state options with application options inside a
            %% provider hook. Carry this command's overrides through that boundary.
            Saved = case dict:is_key('$grisp_previous_cargo_overrides', Original) of
                true -> Original;
                false -> rebar_opts:set(Original, '$grisp_previous_cargo_overrides',
                                       dict:find(cargo_overrides, Original))
            end,
            Scoped = rebar_opts:set(Saved, cargo_overrides, rebar_state:get(State, cargo_overrides, [])),
            Opts = rebar_opts:set(Scoped, '$grisp_native_nifs',
                [N || N <- Nifs, maps:get(app, N) =:= Name]),
            rebar_app_info:opts(Built, Opts)
    end.

%% This adapter knows rebar3_cargo's output layout. The shared release code only
%% consumes the explicit owned paths, so other frontends can supply other layouts.
host_artifacts(Nif) ->
    LoadApps = maps:get(load_apps, Nif),
    Crates = lists:usort([maps:get(name, Nif) | maps:get(symbols, Nif)]),
    Owners = lists:usort([maps:get(load_app, Nif) | maps:values(LoadApps)]),
    Nif#{host_artifacts => [#{app => maps:get(Crate, LoadApps, maps:get(load_app, Nif)),
                              path => filename:join(["priv", "crates", binary_to_list(Crate)])}
                            || Crate <- Crates] ++
                          [#{app => Owner, path => filename:join(["priv", "crates", "build-script-build"])}
                            || Owner <- Owners]}.

%% Release descriptors already contain the paths owned by this frontend.
strip_release(Root, State) ->
    Nifs = rebar_state:get(State, grisp_native_nifs, []),
    grisp_tools_native:strip_release(Root, Nifs).

event([rust, {build, App}], State) ->
    rebar_api:info("Building Rust NIFs for ~p", [App]),
    {ok, State};
event([rust, {diagnostics, Messages}], State) ->
    rebar3_cargo_util:diagnostics(Messages),
    {ok, State};
event([rust, {archive, #{name := Name, symbols := Symbols}}], State) ->
    rebar_api:info("Static Rust NIF ~s: ~ts", [Name, lists:join(", ", Symbols)]),
    {ok, State}.

format_error({rust_cargo_unsupported, Missing}) ->
    format_error({rust_plugin_unsupported, Missing});
format_error({rust_plugin_unsupported, Missing}) ->
    {"Rust NIF inputs were found, but compatible Cargo build support is unavailable.~n"
     "Add this entry to plugins in the root project's rebar.config:~n~n"
     "  {rebar3_cargo, {git, \"https://github.com/grisp/rebar3_cargo.git\",~n"
     "                       {branch, \"cross_build_support_0_1_1\"}}}~n~n"
     "Missing required APIs: ~p", [Missing]};
format_error({rust_not_supported, Platform}) ->
    {"Rust NIFs are not supported for platform ~p", [Platform]};
format_error({rust_sdk_missing_support, Path}) ->
    {"The selected toolchain does not include Rust NIF support (~s is missing). Install a Rust-enabled GRiSP toolchain.", [Path]};
format_error({rust_sdk_missing_file, Path}) ->
    format_missing_sdk_file(filename:basename(Path), Path);
format_error({rust_input_unreadable, Path, Reason}) ->
    {"Cannot read Rust build input ~s: ~p", [Path, Reason]};
format_error({rust_sdk_invalid, Detail}) ->
    {"The selected Rust SDK has invalid or mismatched capability metadata: ~p", [Detail]};
format_error({rust_sdk_target_missing, Target}) ->
    {"The selected Rust SDK does not provide target ~s", [Target]};
format_error({rust_sdk_abi_mismatch, Key, Expected, Actual}) ->
    {"Rust SDK ABI mismatch for ~p: platform requires ~p, SDK provides ~p", [Key, Expected, Actual]};
format_error(docker_not_found) ->
    {"Cannot access Docker. Run 'docker info' in this terminal and check the Docker daemon and socket permissions.", []};
format_error(rust_sdk_not_selected) ->
    {"Rust NIFs require a directory or Docker toolchain in grisp.build.toolchain", []};
format_error({rust_sdk_override, Key}) ->
    {"Rust setting ~p is supplied by the selected SDK. Remove it from grisp.build.rust.", [Key]};
format_error({rust_sdk_read_failed, Path, Reason}) ->
    {"Cannot read Rust SDK input ~s: ~p", [Path, Reason]};
format_error({rust_sdk_command_failed, Code, Output}) ->
    {"Cannot query the selected Rust SDK (~p): ~ts", [Code, io_lib:format("~p", [Output])]};
format_error({rust_bundler_failed, Path, Code, _Output}) ->
    {"Rustler bundler ~s failed (exit ~p). See the diagnostics above.", [Path, Code]};
format_error({rust_bundle_entries_mismatch, Expected, Actual}) ->
    {"Rust bundle entries differ from its specification: expected ~p, found ~p", [Expected, Actual]};
format_error({rust_settings_mismatch, Setting}) ->
    {"Rust NIF crates need matching ~p settings for aggregation", [Setting]};
format_error(_) -> undefined.

format_missing_sdk_file("cargo", Path) ->
    {"Rust NIFs require Cargo in the selected GRiSP toolchain (~s is missing or unusable). " ++
     "Select a Rust-enabled toolchain; host Cargo is not required by the GRiSP build/deploy providers.", [Path]};
format_missing_sdk_file("rustc", Path) ->
    {"Rust NIFs require rustc in the selected GRiSP toolchain (~s is missing or unusable). " ++
     "Select a Rust-enabled toolchain; host Rust is not required by the GRiSP build/deploy providers.", [Path]};
format_missing_sdk_file(_Name, Path) ->
    {"The selected Rust SDK is incomplete: ~s is missing or unusable. Reinstall its Rust support.", [Path]}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rust_plugin_error_test() ->
    lists:foreach(fun(Tag) ->
        {Format, Args} = format_error({Tag, [{cargo_cmd, run_executable, 3}]}),
        Text = iolist_to_binary(io_lib:format(Format, Args)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"https://github.com/grisp/rebar3_cargo.git">>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"cross_build_support_0_1_1">>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"run_executable">>))
    end, [rust_cargo_unsupported, rust_plugin_unsupported]).

host_artifacts_test() ->
    Nif = #{load_app => root, name => <<"bundle">>, symbols => [<<"one">>, <<"two">>],
            load_apps => #{<<"one">> => root, <<"two">> => dependency}},
    #{host_artifacts := Artifacts} = host_artifacts(Nif),
    ?assertEqual(lists:sort([
        #{app => root, path => "priv/crates/bundle"},
        #{app => root, path => "priv/crates/one"},
        #{app => dependency, path => "priv/crates/two"},
        #{app => root, path => "priv/crates/build-script-build"},
        #{app => dependency, path => "priv/crates/build-script-build"}
    ]), lists:sort(Artifacts)).

application_hook_scope_test() ->
    Root = filename:join("/tmp", "grisp-cargo-hook-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Cargo = filename:join(Root, "bin/cargo"),
    try
        ok = filelib:ensure_dir(Cargo),
        ok = file:write_file(Cargo, <<"#!/bin/sh\nexit 99\n">>),
        ok = file:change_mode(Cargo, 8#755),
        ok = file:write_file(filename:join(Root, "Cargo.toml"), <<"[package]\nname='hook_probe'\nversion='0.1.0'\n">>),
        {ok, App0} = rebar_app_info:new(<<"hook_probe">>, "0.1.0", Root),
        App = rebar_app_info:opts(App0, dict:from_list([{cargo_opts,
            [{env, [{"PATH", filename:join(Root, "bin")}]}]}])),
        State = rebar_state:set(rebar_state:new(), cargo_overrides, [{skip, true}]),
        Built = prepare_app(App, State, []),
        %% Match rebar_hooks:run_provider_hooks/6's actual state substitution.
        HookState = rebar_state:current_app(rebar_state:opts(State, rebar_app_info:opts(Built)), Built),
        {ok, _} = rebar3_cargo_compile_prv:do(HookState),
        %% Also execute the real hook dispatcher, including app_discovery.
        ok = file:write_file(filename:join(Root, "rebar.config"), <<"{erl_opts, [debug_info]}.\n">>),
        ok = file:write_file(filename:join(Root, "src/hook_probe.app.src"),
            <<"{application,hook_probe,[{vsn,\"0.1.0\"},{applications,[kernel,stdlib]}]}.\n">>),
        WithHook = rebar_app_info:opts(Built, rebar_opts:set(rebar_app_info:opts(Built), provider_hooks,
            [{pre, [{compile, {cargo, build}}]}])),
        {ok, Discover} = rebar_prv_app_discovery:init(rebar_state:dir(State, Root)),
        {ok, Registered} = rebar3_cargo_compile_prv:init(Discover),
        _ = rebar_hooks:run_all_hooks(Root, pre, compile, rebar_state:providers(Registered),
            WithHook, rebar_state:project_apps(Registered, [WithHook])),
        Container = rebar_state:deps_to_build(rebar_state:all_deps(
            rebar_state:project_apps(State, [Built]), [Built]), [Built]),
        Restored = restore(Container),
        ?assertEqual(1, length(rebar_state:deps_to_build(Restored))),
        lists:foreach(fun(A) ->
            ?assertEqual(error, dict:find(cargo_overrides, rebar_app_info:opts(A)))
        end, rebar_state:project_apps(Restored) ++ rebar_state:all_deps(Restored) ++
             rebar_state:deps_to_build(Restored)),
        Existing = [{skip, false}],
        Configured = rebar_app_info:opts(App,
            rebar_opts:set(rebar_app_info:opts(App), cargo_overrides, Existing)),
        Twice = prepare_app(prepare_app(Configured, State, []), State, []),
        ?assertEqual({ok, Existing}, dict:find(cargo_overrides,
            rebar_app_info:opts(restore_app(Twice))))
    after file:del_dir_r(Root) end.
-endif.
