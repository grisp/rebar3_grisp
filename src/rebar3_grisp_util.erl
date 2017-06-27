-module(rebar3_grisp_util).

% API
-export([get/2]).
-export([get/3]).

%--- API -----------------------------------------------------------------------

get(Keys, Term) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> error({key_not_found, Keys, Term}) end);
get(Key, Term) ->
    get([Key], Term).

get(Keys, Term, Default) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> Default end);
get(Key, Term, Default) ->
    get([Key], Term, Default).

%--- Internal ------------------------------------------------------------------

deep_get([Key|Rest], Map, Default) when is_map(Map) ->
    try deep_get(Rest, maps:get(Key, Map), Default)
    catch error:{badkey, Key} -> Default()
    end;
deep_get([Key|Rest], List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> deep_get(Rest, Value, Default);
        false        -> Default()
    end;
deep_get([], Value, _Default) ->
    Value;
deep_get(Keys, _Term, Default) when is_list(Keys) ->
    Default().
