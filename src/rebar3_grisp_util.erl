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
-export([platform/1]).
-export([otp_build_root/2]).
-export([otp_cache_file_name/2]).
-export([otp_build_install_root/2]).
-export([otp_hash_listing_path/1]).
-export([report_dir/1]).
-export([deploy_dir/1]).
-export([merge_config/2]).
-export([should_build/1]).
-export([ensure_dir/1]).
-export([select_release/3]).
-export([bundle_file_name/3]).
-export([bundle_file_path/3]).
-export([firmware_dir/1]).
-export([firmware_file_name/4]).
-export([firmware_file_path/4]).
-export([update_dir/1]).
-export([update_file_name/3]).
-export([update_file_path/3]).
-export([toolchain_root/1]).

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
    rebar_utils:sh(Command, Args).

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

platform(Config) ->
    try
        get([platform], Config)
    catch
        error:{key_not_found, _, _} ->
            try
                Board = get([board], Config),
                warn(
                    "Configuration key 'board' is deprecated, use 'platform' "
                    " instead."
                ),
                Board
            catch
                error:{key_not_found, _, _} ->
                    ?DEFAULT_GRISP_BOARD
            end
    end.

otp_build_root(RebarState, Version) ->
    filename:join([root(RebarState), "otp", Version, "build"]).

otp_build_install_root(RebarState, Version) ->
    filename:join([root(RebarState), "otp", Version, "install"]).

otp_cache_file_name(Version, Hash) when is_list(Version), is_list(Hash) ->
    "grisp_otp_build_" ++ Version ++ "_" ++ Hash ++ ".tar.gz".

otp_hash_listing_path(InstallRoot) ->
    filename:join([InstallRoot, "GRISP_PACKAGE_FILES"]).

report_dir(RebarState) ->
        filename:join([root(RebarState), "report"]).

deploy_dir(RebarState) ->
        filename:join([root(RebarState), "deploy"]).

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

select_release(RebarState, RelName0, RelVsn)
  when is_atom(RelName0) orelse is_list(RelName0),
       RelVsn =:= undefined orelse is_list(RelVsn) ->
    RelName = case is_list(RelName0) of
        true -> list_to_atom(RelName0);
        false -> RelName0
    end,
    Relx = rebar_state:get(RebarState, relx, []),
    Releases = [element(2, R) || R <- Relx, element(1, R) == 'release'],
    [erlang:error(no_release_configured) || length(Releases) == 0],
    Indexed = index_releases(Releases),

    case {{RelName, RelVsn}, lists:keyfind(RelName, 1, Indexed)} of
        {{undefined, _}, _} when length(Indexed) > 1 ->
            erlang:error({release_not_selected, Indexed});
        {{undefined, undefined}, _} when length(Indexed) == 1 ->
            [{Name, [Version|_]}] = Indexed,
            {Name, Version};
        {{RelName, undefined}, {RelName, [Version|_]}} ->
            {RelName, Version};
        {{RelName, RelVsn} = Release, {RelName, Versions}} ->
            case lists:member(RelVsn, Versions) of
                true -> {RelName, RelVsn};
                false ->
                    erlang:error({unknown_release_version, Release, Versions})
            end;
        {Release, false} ->
            erlang:error({unknown_release_name, Release,
                          lists:map(fun({N, _}) -> N end, Indexed)})
    end.

bundle_file_name(RebarState, RelName, RelVsn) ->
    Config = config(RebarState),
    Board = platform(Config),
    Profile = profile_postfix(RebarState),
    iolist_to_binary(io_lib:format("~s.~s.~s~s.tar.gz",
                                   [Board, RelName, RelVsn, Profile])).

bundle_file_path(RebarState, RelName, RelVsn) ->
    BundleDir = rebar3_grisp_util:deploy_dir(RebarState),
    BundleName = bundle_file_name(RebarState, RelName, RelVsn),
    filename:join(BundleDir, BundleName).

firmware_dir(RebarState) ->
    filename:join([root(RebarState), "firmware"]).

firmware_file_name(RebarState, image, RelName, RelVsn) ->
    Config = config(RebarState),
    Board = platform(Config),
    Profile = profile_postfix(RebarState),
    iolist_to_binary(io_lib:format("~s.~s.~s~s.emmc.gz",
                                   [Board, RelName, RelVsn, Profile]));
firmware_file_name(RebarState, system, RelName, RelVsn) ->
    Config = config(RebarState),
    Board = platform(Config),
    Profile = profile_postfix(RebarState),
    iolist_to_binary(io_lib:format("~s.~s.~s~s.sys.gz",
                                   [Board, RelName, RelVsn, Profile]));
firmware_file_name(RebarState, boot, RelName, RelVsn) ->
    Config = config(RebarState),
    Board = platform(Config),
    Profile = profile_postfix(RebarState),
    iolist_to_binary(io_lib:format("~s.~s.~s~s.boot.gz",
                                   [Board, RelName, RelVsn, Profile])).

firmware_file_path(RebarState, Type, RelName, RelVsn) ->
    BundleDir = rebar3_grisp_util:firmware_dir(RebarState),
    BundleName = firmware_file_name(RebarState, Type, RelName, RelVsn),
    filename:join(BundleDir, BundleName).

update_dir(RebarState) ->
    filename:join([root(RebarState), "update"]).

update_file_name(RebarState, RelName, RelVsn) ->
    Config = config(RebarState),
    Board = platform(Config),
    Profile = profile_postfix(RebarState),
    iolist_to_binary(io_lib:format("~s.~s.~s~s.tar",
                                   [Board, RelName, RelVsn, Profile])).

update_file_path(RebarState, RelName, RelVsn) ->
    UpdateDir = rebar3_grisp_util:update_dir(RebarState),
    UpdateName = update_file_name(RebarState, RelName, RelVsn),
    filename:join(UpdateDir, UpdateName).

toolchain_root(RebarState) ->
    Config = rebar3_grisp_util:config(RebarState),
    DockerImg = rebar3_grisp_util:get([build, toolchain, docker], Config, error),
    TCDir = rebar3_grisp_util:get([build, toolchain, directory], Config, error),
    case os:getenv("GRISP_TOOLCHAIN", TCDir) of
        error -> case DockerImg of
            error -> undefined;
            DockerImage ->
                case rebar3_grisp_util:sh("docker info",[return_on_error]) of
                    {error, _} -> {error, docker_not_found};
                    {ok, _} -> {docker, DockerImage}
                end
            end;
        Directory ->
            {directory, Directory}
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

index_releases(Releases) ->
    Index = lists:foldl(fun({Name, Version}, Acc) ->
        Versions = proplists:get_value(Name, Acc, []),
        lists:keystore(Name, 1, Acc, {Name, [Version|Versions]})
        % maps:update_with(Name, fun(L) -> [Version|L] end, [Version], Acc)
    end, [], Releases),
    lists:map(fun({Name, Versions}) ->
        {Name, lists:usort(fun(V1, V2) ->
            rlx_util:parsed_vsn_lte(
                rlx_util:parse_vsn(V2), % Highest version first
                rlx_util:parse_vsn(V1)
            )
        end, Versions)}
    end, Index).

profile_postfix(RebarState) ->
    AllProfiles = rebar_state:current_profiles(RebarState),
    case [atom_to_binary(P) || P <- AllProfiles, P =/= default, P =/= grisp] of
        [] -> <<"">>;
        Profiles -> iolist_to_binary([".", lists:join($+, Profiles)])
    end.
