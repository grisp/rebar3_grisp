-module(rebar3_grisp).

% Callbacks
-export([init/1]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    lists:foldl(fun(Mod, {ok, S}) -> Mod:init(S) end, {ok, State}, [
        rebar3_grisp_deploy,
        rebar3_grisp_build,
        rebar3_grisp_version
    ]).
