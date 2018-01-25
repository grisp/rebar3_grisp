-module(rebar3_grisp_util).

% API
-export([info/1]).
-export([info/2]).
-export([console/1]).
-export([console/2]).
-export([abort/1]).
-export([abort/2]).
-export([sh/1]).
-export([sh/2]).
-export([get/2]).
-export([get/3]).
-export([set/3]).
-export([root/1]).
-export([otp_build_root/2]).
-export([otp_install_root/2]).
-export([otp_install_release/1]).
-export([grisp_app/1]).
-export([merge_config/2]).

%--- API -----------------------------------------------------------------------

info(Msg) -> info(Msg, []).
info(Msg, Args) -> rebar_api:info(Msg, Args).

console(Msg) -> console(Msg, []).
console(Msg, Args) -> rebar_api:console(Msg, Args).

abort(Msg) -> abort(Msg, []).
abort(Msg, Args) -> rebar_api:abort(Msg, Args).

sh(Command) -> sh(Command, []).
sh(Command, Args) ->
    rebar_utils:sh(Command, Args ++ [abort_on_error]).

get(Keys, Term) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> error({key_not_found, Keys, Term}) end);
get(Key, Term) ->
    get([Key], Term).

get(Keys, Term, Default) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> Default end);
get(Key, Term, Default) ->
    get([Key], Term, Default).

set(Keys, Struct, Value) ->
    update(Keys, Struct, fun
        ([],   _S)                -> Value;
        ([K|P], S) when is_map(S) -> S#{K => set(P, #{}, Value)};
        (P, S)                    -> error({intermediate_value, P, S})
    end).

root(State) ->
    Root = rebar_dir:root_dir(State),
    filename:join(Root, "_grisp").

otp_build_root(State, Version) ->
    filename:join([root(State), "otp", Version, "build"]).

otp_install_root(State, Version) ->
    filename:join([root(State), "otp", Version, "install"]).

otp_install_release(InstallRoot) ->
    ReleaseFile = filename:join([InstallRoot, "releases", "RELEASES"]),
    case file:consult(ReleaseFile) of
        {ok, [[{release, "Erlang/OTP", RelVer, _, _, _} | _]]} -> RelVer;
        _ -> undefined
    end.

grisp_app(Apps) ->
    lists:partition(
        fun(A) -> rebar_app_info:name(A) == <<"grisp">> end,
        Apps
    ).

merge_config(New, Old) ->
    merge_config_(rebar_utils:tup_umerge(New, Old), []).

%--- Internal ------------------------------------------------------------------

deep_get([], Value, _Default) ->
    Value;
deep_get([Key|Rest], Map, Default) when is_map(Map) ->
    try deep_get(Rest, maps:get(Key, Map), Default)
    catch error:{badkey, Key} -> Default()
    end;
deep_get([Key|Rest], List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> deep_get(Rest, Value, Default);
        false        -> Default()
    end;
deep_get(Keys, _Term, Default) when is_list(Keys) ->
    Default().

update(Keys, Struct, Fun) ->
    try deep_update(Keys, Struct, Fun)
    catch throw:{return, Value} -> Value
    end.

deep_update([Key|Keys], Struct, Fun) when is_map(Struct) ->
    case Struct of
        #{Key := Value} -> Struct#{Key := deep_update(Keys, Value, Fun)};
        _               -> Fun([Key|Keys], Struct)
    end;
deep_update([], Struct, Fun) ->
    Fun([], Struct).

merge_config_([], Acc) -> lists:reverse(Acc);
merge_config_([{Key, []}, {Key, [{_, _}|_] = Val} | Rest], Acc) ->
    merge_config_(Rest, [{Key, Val} | Acc]);
merge_config_([{Key, [{_, _}|_] = Val}, {Key, []} | Rest], Acc) ->
    merge_config_(Rest, [{Key, Val} | Acc]);
merge_config_([{Key, [{_, _}|_] = New}, {Key, [{_, _}|_] = Old} | Rest], Acc) ->
    merge_config_(Rest, [{Key, merge_config(New, Old)} | Acc]);
merge_config_([{Key, Val}, {Key, _} | Rest], Acc) ->
    merge_config_(Rest, [{Key, Val} | Acc]);
merge_config_([Item | Rest], Acc) ->
    merge_config_(Rest, [Item | Acc]).
