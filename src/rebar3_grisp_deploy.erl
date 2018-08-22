-module(rebar3_grisp_deploy).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    debug/1,
    debug/2,
    info/1,
    info/2,
    console/1,
    console/2,
    warn/1,
    warn/2,
    abort/1,
    abort/2,
    sh/1,
    set/3
]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, deploy},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}]},
            {example, "rebar3 grisp deploy"},
            {opts, [
                {relname, $n, "relname", string, "Specify the name for the release that will be deployed"},
                {relvsn, $v, "relvsn", string, "Specify the version of the release"},
                {destination, $d, "destination", string, "Path to put deployed release in"},
                {force, $f, "force", {boolean, false}, "Replace existing files"},
                {pre_script, undefined, "pre-script", string, "Shell script to run before deploying begins"},
                {post_script, undefined, "post-script", string, "Shell script to run after deploying has finished"}
            ]},
            {profiles, [grisp]},
            {short_desc, "Deploy a GRiSP release to a destination"},
            {desc,
"Deploys a GRiSP application.

The command requires the release name and version to be provided.
"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    info("~p", [Args]),
    Config = rebar3_grisp_util:config(State),
    RelName = proplists:get_value(relname, Args),
    RelVsn = proplists:get_value(relvsn, Args),
    OTPVersion = rebar3_grisp_util:otp_version(Config),
    Board = rebar3_grisp_util:board(Config),
    Apps = rebar3_grisp_util:apps(State),

    InstallRoot = case rebar3_grisp_util:should_build(Config) of
                      false ->
                          try_get_package(Apps, Board, OTPVersion);
                      true ->
                          console("* Using custom OTP"),
                          rebar3_grisp_util:otp_build_install_root(State, OTPVersion)
                  end,
    InstallRelVer = rebar3_grisp_util:otp_install_release_version(InstallRoot),
    check_otp_release(InstallRelVer),
    State3 = make_release(State, RelName, RelVsn, InstallRoot),
    Force = proplists:get_value(force, Args),
    Dest = get_option(destination, [deploy, destination], State),
    info("Deploying ~s-~s to ~s", [RelName, RelVsn, Dest]),
    run_script(pre_script, State),
    % FIXME: Resolve ERTS version
    ERTSPath = filelib:wildcard(filename:join(InstallRoot, "erts-*")),
    "erts-" ++ ERTSVsn = filename:basename(ERTSPath),
    rebar3_grisp_util:ensure_dir(filename:join(Dest, "PLACEHOLDER")),

    try
        copy_files(State3, RelName, RelVsn, Board, ERTSVsn, Dest, Force)
    catch
        throw:{template_error, File, {missing_key, Key}} ->
            Root = rebar_dir:root_dir(State),
            {ok, Relative} = rebar_file_utils:path_from_ancestor(File, Root),
            rebar3_grisp_util:abort(
                "Error rendering ~s:~nmissing template key: ~s",
                [Relative, Key]
            );
        throw:{template_error, File, {include_not_found, Include}} ->
            Root = rebar_dir:root_dir(State),
            {ok, Relative} = rebar_file_utils:path_from_ancestor(File, Root),
            rebar3_grisp_util:abort(
                "Error rendering ~s:~nmissing include file: ~s",
                [Relative, Include]
            )
    end,

    copy_release(State3, RelName, RelVsn, Dest, Force),
    run_script(post_script, State),
    {ok, State3}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

check_otp_release(InstallRelVer) ->
    case {InstallRelVer, erlang:system_info(otp_release)} of
        {Target, Target} -> ok;
        {Target, Current} ->
            warn(
                "Current Erlang version (~p) does not match target "
                "Erlang version (~p). It is not guaranteed that the "
                "deployed release will work!", [Current, Target]
            )
    end.

make_release(_State, Name, Version, _InstallRoot) when
  Name == undefined; Version == undefined ->
    abort("Release name and/or version not specified", []);
make_release(State, Name, Version, InstallRoot) ->
    State2 = rebar_state:set(State, relx, [
        {include_erts, InstallRoot},
        {system_libs, InstallRoot},
        {extended_start_script, false},
        {dev_mode, false}
        |rebar_state:get(State, relx, [])
    ]),
    {ok, State3} = rebar_prv_do:do_tasks(
        [{"release", ["-n", Name, "-v", Version]}],
        rebar_state:namespace(State2, default)
    ),
    rebar_state:namespace(State3, grisp).

run_script(Name, State) ->
    case get_option(Name, [deploy, Name], State, undefined) of
        undefined -> ok;
        Script ->
            console("* Running ~p", [Name]),
            {ok, Output} = sh(Script),
            case trim(Output) of
                ""      -> ok;
                Trimmed -> console(Trimmed)
            end
    end.

copy_files(State, RelName, RelVsn, Board, ERTSVsn, Dest, Force) ->
    console("* Copying files..."),
    Tree = find_replacement_files(State, Board, "files"),
    Context = #{
        release_name    => RelName,
        release_version => RelVsn,
        erts_vsn        => ERTSVsn
    },
    maps:map(
        fun(Target, Source) ->
            write_file(Dest, Target, Source, Force, Context)
        end,
        Tree
    ).

find_replacement_files(State, Board, Subdir) ->
    AllApps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
    case rebar3_grisp_util:grisp_app(AllApps) of
        {[], _} ->
            grisp_files(rebar_state:dir(State), Board, Subdir);
        {[Grisp|_], _} ->
            [GrispFiles, ProjectFiles] = lists:map(
                fun(Dir) -> grisp_files(Dir, Board, Subdir) end,
                [rebar_app_info:dir(Grisp), rebar_state:dir(State)]
            ),
            maps:merge(GrispFiles, ProjectFiles)
    end.

grisp_files(Dir, Board, Subdir) ->
    Path = filename:join([Dir, "grisp", Board, Subdir]),
    resolve_files(find_files(Path), Path).

write_file(Dest, Target, Source, Force, Context) ->
    Path = filename:join(Dest, Target),
    debug("Creating ~p from ~p", [Path, Source]),
    Content = load_file(Source, Context),
    force_execute(Path, Force,
        fun(F) ->
            rebar3_grisp_util:ensure_dir(F),
            ok = file:write_file(F, Content)
        end
    ).

find_files(Dir) ->
    [F || F <- filelib:wildcard(Dir ++ "/**"), filelib:is_regular(F)].

resolve_files(Files, Root) -> resolve_files(Files, Root, #{}).

resolve_files([File|Files], Root, Resolved) ->
    Relative = prefix(File, Root ++ "/"),
    Name = filename:rootname(Relative, ".mustache"),
    resolve_files(Files, Root, maps:put(
        Name,
        resolve_file(Root, Relative, Name, maps:find(Name, Resolved)),
        Resolved
    ));
resolve_files([], _Root, Resolved) ->
    Resolved.

prefix(String, Prefix) ->
    case lists:split(length(Prefix), String) of
        {Prefix, Rest} -> Rest;
        _              -> String
    end.

resolve_file(Root, Source, Source, error) ->
    filename:join(Root, Source);
resolve_file(Root, Source, _Target, _) ->
    {template, filename:join(Root, Source)}.

load_file({template, Source}, Context) ->
    rebar3_grisp_template:render(Source, Context);
load_file(Source, _Context) ->
    {ok, Binary} = file:read_file(Source),
    Binary.

copy_release(State, Name, _Version, Dest, Force) ->
    console("* Copying release..."),
    Source = filename:join([rebar_dir:base_dir(State), "rel", Name]),
    Target = filename:join(Dest, Name),
    Command = case Force of
        true  -> "cp -Rf";
        false -> "cp -R"
    end,
    sh(string:join([Command, qoute(Source ++ "/"), qoute(Target)], " ")).

qoute(String) -> "\"" ++ String ++ "\"".

force_execute(File, Force, Fun) ->
    case {filelib:is_file(File), Force} of
        {true, false} ->
            abort(
                "Destination ~s already exists (use --force to overwrite)",
                [File]
            );
        _ ->
            ok
    end,
    Fun(File).

get_option(Arg, ConfigKey, State) ->
    get_arg_option(Arg, State, fun(Config) ->
        rebar3_grisp_util:get(ConfigKey, Config)
    end).

get_option(Arg, ConfigKey, State, Default) ->
    get_arg_option(Arg, State, fun(Config) ->
        rebar3_grisp_util:get(ConfigKey, Config, Default)
    end).

get_arg_option(Arg, State, Fun) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, grisp, []),
    case proplists:get_value(Arg, Args) of
        undefined -> Fun(Config);
        Value     -> Value
    end.

trim(String) ->
    re:replace(String, "(^[\s\n\t]+|[\s\n\t]+$)", "", [global, {return, list}]).

try_get_package(Apps, Board, OTPVersion) ->
    {Hash, _HashString} = rebar3_grisp_util:get_hash(Apps, Board),
    debug("Version ~p, Hash ~p", [OTPVersion, Hash]),
    try obtain_prebuilt(OTPVersion, Hash)
    catch
        error:nomatch -> abort("Package for OTP ~p not found in our repository "++
                                   "Either C source files have been modified or the "++
                                   "OTP version does not have a pre-built package. "++
                                   "Please build OTP manually using:~n~n"++
                                   "rebar3 grisp build", [OTPVersion])
    end,
    rebar3_grisp_util:otp_cache_install_root(OTPVersion, Hash).

obtain_prebuilt(Version, ExpectedHash) ->
    Tarball = rebar3_grisp_util:otp_cache_file(Version, ExpectedHash),
    case filelib:is_regular(Tarball) of
        true ->
            ETag = rebar3_grisp_util:otp_cache_install_etag(Version, ExpectedHash),
            debug("Found file with ETag ~p", [ETag]),
            download_and_unpack(Version, ExpectedHash, ETag);
        false ->
            download_and_unpack(Version, ExpectedHash, false)
    end.

download_and_unpack(Version, Hash, ETag) ->
    rebar3_grisp_util:ensure_dir(rebar3_grisp_util:otp_cache_file(Version, Hash)),
    case file:delete(rebar3_grisp_util:otp_cache_file_temp(Version, Hash)) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, FileReason} -> abort("Error ~p", [FileReason])
    end,
    case try_download(Version, Hash, ETag) of
        {etag, ServerETag} -> ServerETag;
        _Other -> ServerETag = ""
    end,

    case filelib:is_regular(rebar3_grisp_util:otp_cache_file(Version, Hash)) of
        true ->
            maybe_unpack(Version, Hash, ServerETag);
        false -> 
            abort(
                "Could not find a cached package matching your " ++ 
                "configuration!~n" ++
                "Possible causes:~n" ++
                "  - You have custom C sources and need to do a manual " ++
                "build: rebar3 grisp build~n" ++
                "  - The package is missing in the repository and the local " ++
                "cache"
            )
    end.

try_download(Version, Hash, ETag) ->
    ssl:start(),
    {ok, InetsPid} = inets:start(httpc, [{profile, rebar3_grisp}], stand_alone),
    HTTPOptions = [{connect_timeout, 5000}],
    Options = [{stream, self},
               {body_format, binary}, {sync, false}],
    Url = rebar3_grisp_util:cdn() ++ rebar3_grisp_util:otp_cache_file_name(Version, Hash),
    Filename = rebar3_grisp_util:otp_cache_file_temp(Version, Hash),
    case ETag of
        false -> Headers = [];
        _ -> Headers = [{"If-None-Match", ETag}]
    end,
    {ok, RequestId} = httpc:request(get, {Url, Headers}, HTTPOptions, Options, InetsPid),
    console("* Downloading prebuilt OTP package"),
    download_loop(Filename, RequestId, Version, Hash, ETag).


download_loop(Filename, RequestId, Version, Hash, ETag) ->
    try
        {ok, FileHandle} = file:open(Filename, [append, raw, binary]),
        download_loop(Filename, RequestId, Version, Hash, FileHandle, ETag)
    after
        file:close(Filename)
    end.

download_loop(Filename, RequestId, Version, Hash, FileHandle, ETag) ->
    receive
        {http, {RequestId, stream_start, _Headers}} ->
            debug("Starting download", []),
            download_loop(Filename, RequestId, Version, Hash, FileHandle, ETag);
        {http, {RequestId, stream, BinBodyPart}} ->
            ok = file:write(FileHandle, BinBodyPart),
            download_loop(Filename, RequestId, Version, Hash, FileHandle, ETag);
        {http, {RequestId, stream_end, Headers}} ->
            debug("Stream ended", []),
            case lists:keyfind("etag", 1, Headers) of
                {"etag", ServerETag} ->
                    debug("Downloaded file with ETag ~p", [ServerETag]),
                    finalize_download(FileHandle, Version, Hash, ServerETag);
                false ->
                    finalize_download(FileHandle, Version, Hash, ETag)
            end;
        {http, {RequestId, {{_HTTPVersion, 304, "Not Modified"}, _Headers, _Body}}} ->
            console("* Cached file is up to date"),
            {etag, ETag};
        {http, {RequestId, {{_HTTPVersion, 404, "Not Found"}, _Headers, _Body}}} ->
            warn("* Server does not have OTP ~p Hash ~p", [Version, Hash]),
            {etag, ETag};
        {http, Other} ->
            warn("* Download error: ~n~p", [Other]),
            console("* Using cached file"),
            debug("HTTPC error: ~p, RequestId ~p", [Other, RequestId]),
            {etag, ETag}
    after
        120000 ->
            warn("* Download timed out")
    end.

finalize_download(FileHandler, Version, Hash, ETag) ->
    ok = file:close(FileHandler),
    move_file(rebar3_grisp_util:otp_cache_file_temp(Version, Hash),
              rebar3_grisp_util:otp_cache_file(Version, Hash)),
    console("* Download completed"),
    {etag, ETag}.

move_file(From, To) ->
    case file:delete(To) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> abort("Error ~p", [Reason])
    end,
    file:rename(From, To),
    file:delete(From).

maybe_unpack(Version, Hash, ETag) ->
    case should_unpack(Version, Hash, ETag) of
        yes ->
            OTPCacheInstallRoot = rebar3_grisp_util:otp_cache_install_root(Version, Hash),
            console("* Extracting package"),
            case erl_tar:extract(
                   rebar3_grisp_util:otp_cache_file(Version, Hash),
                   [compressed, {cwd, OTPCacheInstallRoot}])
            of
                ok -> ok;
                {error, Reason} -> abort("Tar extraction failed: ~p", [Reason])
            end,
            if
                is_list(ETag) ->
                    ok = file:write_file(filename:join([OTPCacheInstallRoot, "ETag"]),
                                         io_lib:format("~p.~n", [{etag, ETag}]));
                true -> ok
            end;
        no -> console("* Current package up to date")
    end.

should_unpack(Version, Hash, ETag) ->
    debug("Checking for ETag ~p", [ETag]),
    case file:consult(filename:join([rebar3_grisp_util:otp_cache_install_root(Version, Hash), "ETag"])) of
        {error, enoent} -> yes;
        {ok, [{etag, ETag}]} -> no; % not modified
        _Other  -> yes
    end.
