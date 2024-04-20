-module(rebar3_grisp).

% Callbacks
-export([init/1]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    % Start applications, because Rebar 3 doesn't do it for us
    {ok, AllDeps} = application:get_key(?MODULE, applications),
    Deps = AllDeps -- [kernel, stdlib],
    [{ok, _} = application:ensure_all_started(A) || A <- Deps],
    % Register tasks
    lists:foldl(fun(Mod, {ok, S}) -> Mod:init(S) end, {ok, State}, [
        rebar3_grisp_deploy,
        rebar3_grisp_build,
        rebar3_grisp_configure,
        rebar3_grisp_package,
        rebar3_grisp_version,
        rebar3_grisp_report
    ]).
