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
-export([err/1]).
-export([err/2]).
-export([abort/1]).
-export([abort/2]).
-export([sh/1]).
-export([sh/2]).
-export([get/2]).
-export([get/3]).
-export([files_copy_destination/2]).
-export([files_copy_destination_merged/2]).
-export([filenames_join_copy_destination/2]).
-export([get_hash/2]).
-export([hash_grisp_files/1]).
-export([set/3]).
-export([root/1]).
-export([config/1]).
-export([otp_version/1]).
-export([otp_git/0]).
-export([cdn/0]).
-export([board/1]).
-export([otp_build_root/2]).
-export([otp_cache_file_name/2]).
-export([otp_cache_file/2]).
-export([otp_cache_file_temp/2]).
-export([otp_cache_root/0]).
-export([otp_cache_install_root/2]).
-export([otp_cache_install_etag/2]).
-export([otp_build_install_root/2]).
-export([otp_install_release_version/1]).
-export([otp_hash_listing_path/1]).
-export([grisp_app/1]).
-export([merge_config/2]).
-export([should_build/1]).
-export([ensure_dir/1]).

-define(BLOCKSIZE, 4194304). % 4MB

-include("rebar3_grisp.hrl").

%--- API -----------------------------------------------------------------------

apps(State) ->
    Apps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
    {Grisp, Other} = grisp_app(Apps),
    Other ++ Grisp.

debug(Msg) -> debug(Msg, []).
debug(Msg, Args) -> rebar_api:debug(Msg, Args).

info(Msg) -> info(Msg, []).
info(Msg, Args) -> rebar_api:info(Msg, Args).

console(Msg) -> console(Msg, []).
console(Msg, Args) -> rebar_api:console(Msg, Args).

warn(Msg) -> warn(Msg, []).
warn(Msg, Args) -> rebar_api:warn(Msg, Args).

err(Msg) -> err(Msg, []).
err(Msg, Args) -> rebar_api:error(Msg, Args).

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

files_copy_destination(Apps, Board) ->
    debug("Creating copy list for apps: ~p", [Apps]),
    lists:foldl(
      fun(A, {Sys, Drivers}) ->
              collect_c_sources(A, Board, Sys, Drivers)
      end,
      {#{}, #{}},
      Apps
     ).

files_copy_destination_merged(Apps, Board) ->
    {DriverFiles, SystemFiles} = files_copy_destination(Apps, Board),
    maps:merge(DriverFiles, SystemFiles).

filenames_join_copy_destination(FromTo, Root) ->
    maps:fold(
      fun(Target, Source, AccFromTo) ->
              maps:put(filename:join([Root, Target]), Source, AccFromTo)
      end,
      #{}, FromTo).

get_hash(Apps, Board) ->
    ToFrom2 = files_copy_destination_merged(Apps, Board),
    hash_grisp_files(ToFrom2).

hash_grisp_files(ToFrom) ->
    debug("Hashing ToFrom map: ~p", [ToFrom]),

    Sorted = lists:keysort(1, maps:to_list(ToFrom)),
    FileHashes = lists:map(
                   fun({Target, Source}) ->
                           debug("Hashing ~p for location ~p", [Source, Target]),
                           {ok, Hash} = hash_file(Source, sha256),
                           {Target, Hash}
                   end,
                   Sorted
                  ),
    debug("~p", [FileHashes]),

    HashString = hashes_to_string(FileHashes),
    Hash = lists:flatten(format_hash(sha256, crypto:hash(sha256, HashString))),
    {Hash, HashString}.

set(Keys, Struct, Value) ->
    update(Keys, Struct, fun
        ([],   _S)                -> Value;
        ([K|P], S) when is_map(S) -> S#{K => set(P, #{}, Value)};
        (P, S)                    -> error({intermediate_value, P, S})
    end).

root(State) ->
    Root = rebar_dir:root_dir(State),
    filename:join(Root, "_grisp").

config(State) ->
    rebar_state:get(State, grisp, []).

otp_version(Config) ->
    get([otp, version], Config, ?DEFAULT_OTP_VSN).

otp_git() ->
    ?GIT_REMOTE_OTP.

cdn() ->
    ?DOWNLOAD_CDN_URI.

board(Config) ->
    get([board], Config, ?DEFAULT_GRISP_BOARD).

otp_build_root(State, Version) ->
    filename:join([root(State), "otp", Version, "build"]).

otp_build_install_root(State, Version) ->
    filename:join([root(State), "otp", Version, "install"]).

otp_cache_install_root(Version, Hash) ->
    filename:join([otp_cache_root(),
                   "grisp_otp_build_" ++ Version ++ "_" ++ Hash]).

otp_cache_install_etag(Version, Hash) ->
    {ok, [{etag, ETag}]} = file:consult(
      filename:join([otp_cache_install_root(Version, Hash), "ETag"])),
     ETag.

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

otp_hash_listing_path(InstallRoot) ->
    filename:join([InstallRoot, "GRISP_PACKAGE_FILES"]).

grisp_app(Apps) ->
    UApps = lists:usort(Apps),
    lists:partition(
        fun(A) -> rebar_app_info:name(A) == <<"grisp">> end,
        UApps
    ).

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

collect_c_sources(App, Board, Sys, Drivers) ->
    Source = filename:join([rebar_app_info:dir(App), "grisp", Board]),
    {maps:merge(Sys, collect_sys(Source)),
     maps:merge(Drivers, collect_drivers(Source))}.

collect_sys(Source) ->
    maps:merge(
      collect_files(
        {Source, "sys/*.h"},
        "erts/emulator/sys/unix"
       ),
      collect_files(
        {Source, "sys/*.c"},
        "erts/emulator/sys/unix"
       )
     ).

collect_drivers(Source) ->
    maps:merge(
      collect_files(
        {Source, "drivers/*.h"},
        "erts/emulator/drivers/unix"
       ),
      collect_files(
        {Source, "drivers/*.c"},
        "erts/emulator/drivers/unix"
       )
     ).

collect_files({SourceRoot, Pattern}, Target) ->
    debug("Collecting ~p ~p for Target ~p", [SourceRoot, Pattern, Target]),
    Files = filelib:wildcard(filename:join(SourceRoot, Pattern)),
    maps:from_list([collect_file(F, Target) || F <- Files]).

collect_file(Source, TargetDir) ->
    Base = filename:basename(Source),
    TargetFile = filename:join([TargetDir, Base]),
    debug("Collecting Target ~p, Source ~p", [TargetFile, Source]),
    {TargetFile, Source}.

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

hash_file(File, Algorithm) ->
    CryptoHandle = crypto:hash_init(Algorithm),
    case file:open(File, [binary, raw, read]) of
        {ok, FileHandle} -> hash_file_read(FileHandle, CryptoHandle);
        Error -> Error
    end.

hash_file_read(FileHandle, HashHandle) ->
    case file:read(FileHandle, ?BLOCKSIZE) of
        {ok, Bin} -> hash_file_read(FileHandle, crypto:hash_update(HashHandle, Bin));
        eof ->
            file:close(FileHandle),
            {ok, crypto:hash_final(HashHandle)}
    end.

hashes_to_string(Hashes) ->
    lists:map(
      fun({Target, Hash}) ->
              io_lib:format("~s ~s~n", [Target, format_hash(sha256, Hash)]) end,
      Hashes).

format_hash(sha256, Hash) when is_binary(Hash) ->
    <<Int:256/big-unsigned-integer>> = Hash,
    format_hash(Int);
format_hash(md5, Hash) when is_binary(Hash) ->
    <<Int:128/big-unsigned-integer>> = Hash,
    format_hash(Int).

format_hash(Int) when is_integer(Int) ->
    io_lib:format("~.16b", [Int]).

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

