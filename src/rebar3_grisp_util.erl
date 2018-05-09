-module(rebar3_grisp_util).

% API
-export([apps/1]).
-export([info/1]).
-export([info/2]).
-export([console/1]).
-export([console/2]).
-export([abort/1]).
-export([abort/2]).
-export([sh/1]).
-export([sh/2]).
-export([format_hash/1, format_hash/2]).
-export([get/2]).
-export([get/3]).
-export([get_copy_list/3]).
-export([hash_grisp_files/1]).
-export([hash_file/2, hash_file/3]).
-export([set/3]).
-export([root/1]).
-export([otp_build_root/2]).
-export([otp_cache_file_name/2]).
-export([otp_cache_file/2]).
-export([otp_cache_file_temp/2]).
-export([otp_cache_root/0]).
-export([otp_install_root/3]).
-export([otp_install_release_version/1]).
-export([grisp_app/1]).
-export([merge_config/2]).
-export([toolchain_or_prebuilt/1]).

-define(BLOCKSIZE, 4194304). % 4MB

%--- API -----------------------------------------------------------------------

apps(State) ->
    Apps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
    {Grisp, Other} = rebar3_grisp_util:grisp_app(Apps),
    Other ++ Grisp.

info(Msg) -> info(Msg, []).
info(Msg, Args) -> rebar_api:info(Msg, Args).

console(Msg) -> console(Msg, []).
console(Msg, Args) -> rebar_api:console(Msg, Args).

abort(Msg) -> abort(Msg, []).
abort(Msg, Args) -> rebar_api:abort(Msg, Args).

sh(Command) -> sh(Command, []).
sh(Command, Args) ->
    rebar_utils:sh(Command, Args ++ [abort_on_error]).

format_hash(sha256, Hash) when is_binary(Hash) ->
    <<Int:256/big-unsigned-integer>> = Hash,
    format_hash(Int);
format_hash(md5, Hash) when is_binary(Hash) ->
    <<Int:128/big-unsigned-integer>> = Hash,
    format_hash(Int).

format_hash(Int) when is_integer(Int) ->
    io_lib:format("~.16B", [Int]).

get(Keys, Term) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> error({key_not_found, Keys, Term}) end);
get(Key, Term) ->
    get([Key], Term).

get(Keys, Term, Default) when is_list(Keys) ->
    deep_get(Keys, Term, fun() -> Default end);
get(Key, Term, Default) ->
    get([Key], Term, Default).

get_copy_list(Apps, Board, OTPRoot) ->
    rebar_api:debug("Creating copy list for apps: ~p", [Apps]),
    {_SystemFiles, _DriverFiles} = lists:foldl(
                                     fun(A, {Sys, Drivers}) ->
                                             collect_c_sources(A, Board, OTPRoot, Sys, Drivers)
                                     end,
                                     {#{}, #{}},
                                     Apps
                                    ).

hash_grisp_files(ToFrom) ->
    rebar_api:debug("Hashing ToFrom map: ~p", [ToFrom]),

    Sorted = lists:keysort(1, maps:to_list(ToFrom)),
    FileHashes = lists:map(
                   fun({Target, Source}) ->
                           rebar_api:debug("Hashing ~p for location ~p", [Source, Target]),
                           hash_file(Source, sha256, Target)
                   end,
                   Sorted
                  ),
    HashString = hashes_to_string(FileHashes),
    %%TODO: write to file
    Hash = lists:flatten(format_hash(sha256, crypto:hash(sha256, HashString))),
    {Hash, HashString}.

hash_file(File, Algorithm, Name) ->
    CryptoHandle = crypto:hash_init(Algorithm),
    HashHandle = crypto:hash_update(CryptoHandle, list_to_binary(Name)),
    case file:open(File, [binary, raw, read]) of
        {ok, FileHandle} -> hash_file_read(FileHandle, HashHandle);
        Error -> Error
    end.

hash_file(File, Algorithm) ->
    hash_file(File, Algorithm, "").

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

otp_install_root(State, Version, build) ->
    filename:join([root(State), "otp", Version, "install"]);
otp_install_root(Version, Hash, prebuilt) ->
    filename:join([rebar_dir:home_dir(), ".cache", "grisp", "packages", "otp", "build", "grisp_otp_build_" ++ Version ++ "_" ++ Hash]).

otp_install_release_version(InstallRoot) ->
    ReleaseFile = filename:join([InstallRoot, "releases", "RELEASES"]),
    case file:consult(ReleaseFile) of
        {ok, [[{release, "Erlang/OTP", RelVer, _, _, _} | _]]} -> RelVer;
        _ -> undefined
    end.

otp_cache_root() ->
    filename:join([rebar_dir:home_dir(), ".cache", "grisp", "packages", "otp", "build"]).

otp_cache_file_name(Version, Hash) when is_list(Version) and is_list(Hash) ->
    "grisp_otp_build_" ++ Version ++ "_" ++ Hash ++ ".tar.gz".

otp_cache_file(Version, Hash) ->
    filename:join([otp_cache_root(), otp_cache_file_name(Version, Hash)]).

otp_cache_file_temp(Version, Hash) ->
    otp_cache_file(Version, Hash) ++ ".temp".

grisp_app(Apps) ->
    UApps = lists:usort(Apps),
    lists:partition(
        fun(A) -> rebar_app_info:name(A) == <<"grisp">> end,
        UApps
    ).

merge_config(New, Old) ->
    merge_config_(rebar_utils:tup_umerge(New, Old), []).


toolchain_or_prebuilt(Config) ->
    try
        TcRoot = get([build, toolchain, directory], Config),
        console("* Using specified toolchain"),
        TcRoot
    catch
        error:{key_not_found, _, _} ->
            console("* Using prebuilt OTP"),
            prebuilt
    end.


%--- Internal ------------------------------------------------------------------

collect_c_sources(App, Board, OTPRoot, Sys, Drivers) ->
    Source = filename:join([rebar_app_info:dir(App), "grisp", Board]),
    {maps:merge(Sys, collect_sys(Source, OTPRoot)),  maps:merge(Drivers, collect_drivers(Source, OTPRoot))}.

collect_sys(Source, OTPRoot) ->
    maps:merge(
      collect_files(
        {Source, "sys/*.h"},
        {OTPRoot, "erts/emulator/sys/unix"}
       ),
      collect_files(
        {Source, "sys/*.c"},
        {OTPRoot, "erts/emulator/sys/unix"}
       )
     ).

collect_drivers(Source, OTPRoot) ->
    maps:merge(
      collect_files(
        {Source, "drivers/*.h"},
        {OTPRoot, "erts/emulator/drivers/unix"}
       ),
      collect_files(
        {Source, "drivers/*.c"},
        {OTPRoot, "erts/emulator/drivers/unix"}
       )
     ).

collect_files({SourceRoot, Pattern}, Target) ->
    rebar_api:debug("Collecting ~p ~p for Target ~p", [SourceRoot, Pattern, Target]),
    Files = filelib:wildcard(filename:join(SourceRoot, Pattern)),
    maps:from_list([collect_file(F, Target) || F <- Files]).

collect_file(Source, {TargetRoot, TargetDir}) ->
    Base = filename:basename(Source),
    TargetFile = filename:join(TargetDir, Base),
    Target = filename:join(TargetRoot, TargetFile),
    rebar_api:debug("Collecting Target ~p, Source ~p", [Target, Source]),
    {Target, Source}.

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

hashes_to_string(Hashes) ->
    lists:map(
      fun({Target, Hash}) ->
              io_lib:format("~s ~s~n", [Target, format_hash(sha256, Hash)]) end,
      Hashes).

hash_file_read(FileHandle, HashHandle) ->
    case file:read(FileHandle, ?BLOCKSIZE) of
        {ok, Bin} -> hash_file_read(FileHandle, crypto:hash_update(HashHandle, Bin));
        eof ->
            file:close(FileHandle),
            {ok, crypto:hash_final(HashHandle)}
    end.

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

