-module(grisp).

% Callbacks
-export([init/1]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = grisp_prv:init(State),
    {ok, State1}.
