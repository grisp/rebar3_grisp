-module(rebar3_grisp_util).

% API
-export([apps/1]).
-export([debug/1]).
-export([debug/2]).
-export([info/1]).
-export([info/2]).
-export([console/1]).
-export([console/2]).
-export([warn/1]).
-export([warn/2]).
-export([error/1]).
-export([error/2]).
-export([abort/1]).
-export([abort/2]).
-export([sh/1]).
-export([sh/2]).
-export([get/2]).
-export([get/3]).
-export([filenames_join_copy_destination/2]).
-export([root/1]).
-export([config/1]).
-export([otp_version/1]).
-export([otp_git/0]).
-export([board/1]).
-export([platform/1]).
-export([otp_build_root/2]).
-export([otp_cache_file_name/2]).
-export([otp_build_install_root/2]).
-export([otp_hash_listing_path/1]).
-export([merge_config/2]).
-export([should_build/1]).
-export([ensure_dir/1]).

-import(rebar3_grisp_tools, [event/2]).

-include("rebar3_grisp.hrl").

%--- API -----------------------------------------------------------------------

apps(RebarState) ->
    Apps = rebar_state:all_deps(RebarState) ++ rebar_state:project_apps(RebarState),
    lists:map(fun(App) ->
        Name = binary_to_atom(rebar_app_info:name(App), utf8),
        {Name, #{
            dir => rebar_app_info:dir(App),
            deps => proplists:get_value(applications, rebar_app_info:app_details(App), [])
        }}
    end, Apps).

debug(Msg) -> debug(Msg, []).
debug(Msg, Args) -> rebar_api:debug(Msg, Args).

info(Msg) -> info(Msg, []).
info(Msg, Args) -> rebar_api:info(Msg, Args).

console(Msg) -> console(Msg, []).
console(Msg, Args) -> rebar_api:console(Msg, Args).

warn(Msg) -> warn(Msg, []).
warn(Msg, Args) -> rebar_api:warn(Msg, Args).

error(Msg) -> ?MODULE:error(Msg, []).
error(Msg, Args) -> rebar_api:error(Msg, Args).

abort(Msg) -> abort(Msg, []).
abort(Msg, Args) -> rebar_api:abort(Msg, Args).

sh(Command) -> sh(Command, []).
sh(Command, Args) ->
    rebar_utils:sh(Command, Args ++ [abort_on_error]).

get(Keys, Term) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> erlang:error({key_not_found, Keys, Term}) end);
get(Key, Term) ->
    get([Key], Term).

get(Keys, Term, Default) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> Default end);
get(Key, Term, Default) ->
    get([Key], Term, Default).

filenames_join_copy_destination(FromTo, Root) ->
    maps:fold(
      fun(Target, Source, AccFromTo) ->
              maps:put(filename:join([Root, Target]), Source, AccFromTo)
      end,
      #{}, FromTo).

root(RebarState) ->
    Root = rebar_dir:root_dir(RebarState),
    filename:join(Root, "_grisp").

config(State) ->
    rebar_state:get(State, grisp, []).

otp_version(Config) ->
    get([otp, version], Config, ?DEFAULT_OTP_VSN).

otp_git() ->
    ?GIT_REMOTE_OTP.

% TODO: Deprecate!
board(Config) ->
    warn("Configuration key 'board' is deprecated, use 'platform' instead."),
    get([board], Config, ?DEFAULT_GRISP_BOARD).

platform(Config) ->
    get([platform], Config, board(Config)).

otp_build_root(RebarState, Version) ->
    filename:join([root(RebarState), "otp", Version, "build"]).

otp_build_install_root(RebarState, Version) ->
    filename:join([root(RebarState), "otp", Version, "install"]).

otp_cache_file_name(Version, Hash) when is_list(Version), is_list(Hash) ->
    "grisp_otp_build_" ++ Version ++ "_" ++ Hash ++ ".tar.gz".

otp_hash_listing_path(InstallRoot) ->
    filename:join([InstallRoot, "GRISP_PACKAGE_FILES"]).

% TODO: Remove
merge_config(New, Old) ->
    merge_config_(rebar_utils:tup_umerge(New, Old), []).

should_build(Config) ->
    try
        get([build], Config),
        true
    catch
        error:{key_not_found, _, _} ->
            false
    end.

ensure_dir(File) ->
    case filelib:ensure_dir(File) of
        ok    -> ok;
        Error -> abort("Could not create target directory: ~p", [Error])
    end.

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
