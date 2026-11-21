-module(rebar3_grisp_util_tests).
-include_lib("eunit/include/eunit.hrl").

toolchain_root_test() ->
    Root = filename:join("/tmp", "grisp-toolchain-" ++ integer_to_list(
        erlang:unique_integer([positive]))),
    Docker = filename:join(Root, "docker"),
    Previous = [{Name, os:getenv(Name)} || Name <- ["GRISP_TOOLCHAIN", "PATH"]],
    State = fun(Toolchain) ->
        rebar_state:set(rebar_state:new(), grisp, [{build, [{toolchain, Toolchain}]}])
    end,
    try
        true = os:unsetenv("GRISP_TOOLCHAIN"),
        ?assertEqual(undefined, rebar3_grisp_util:toolchain_root(State([]))),
        Directory = State([{directory, "/configured sdk"}]),
        ?assertEqual({directory, "/configured sdk"}, rebar3_grisp_util:toolchain_root(Directory)),
        Both = State([{directory, "/configured sdk"}, {docker, "test-image"}]),
        ?assertEqual({directory, "/configured sdk"}, rebar3_grisp_util:toolchain_root(Both)),
        true = os:putenv("GRISP_TOOLCHAIN", "/environment sdk"),
        ?assertEqual({directory, "/environment sdk"}, rebar3_grisp_util:toolchain_root(Both)),
        ?assertEqual({directory, "/environment sdk"}, rebar3_grisp_util:toolchain_root(State([]))),
        true = os:unsetenv("GRISP_TOOLCHAIN"),
        %% Probe a fake Docker executable; do not contact a daemon or run an image.
        ok = filelib:ensure_dir(Docker),
        ok = file:write_file(Docker, <<"#!/bin/sh\nexit 0\n">>),
        ok = file:change_mode(Docker, 8#755),
        true = os:putenv("PATH", Root ++ ":" ++ os:getenv("PATH", "")),
        Container = State([{docker, "test-image"}]),
        ?assertEqual({docker, "test-image"}, rebar3_grisp_util:toolchain_root(Container)),
        ok = file:write_file(Docker, <<"#!/bin/sh\nexit 1\n">>),
        ?assertEqual({error, docker_not_found}, rebar3_grisp_util:toolchain_root(Container))
    after
        lists:foreach(fun
            ({Name, false}) -> os:unsetenv(Name);
            ({Name, Value}) -> os:putenv(Name, Value)
        end, Previous),
        file:del_dir_r(Root)
    end.
