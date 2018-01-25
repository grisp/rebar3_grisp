-module(rebar3_grisp_deploy).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-include("rebar3_grisp.hrl").

-import(rebar3_grisp_util, [
    info/2,
    console/1,
    console/2,
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
            {deps, [{default, app_discovery}]},
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
    Config = rebar_state:get(State, grisp, []),
    RelName = proplists:get_value(relname, Args),
    RelVsn = proplists:get_value(relvsn, Args),
    OTPVersion = rebar3_grisp_util:get([otp, version], Config,
        ?DEFAULT_OTP_VSN
    ),
    InstallRoot = rebar3_grisp_util:otp_install_root(State, OTPVersion),
    InstallRelVer = rebar3_grisp_util:otp_install_release(InstallRoot),
    check_otp_release(InstallRelVer),
    State3 = make_release(State, RelName, RelVsn, InstallRoot),
    Force = proplists:get_value(force, Args),
    Dest = get_option(destination, [deploy, destination], State),
    info("Deploying ~s-~s to ~s", [RelName, RelVsn, Dest]),
    run_script(pre_script, State),
    % FIXME: Resolve ERTS version
    ERTSPath = filelib:wildcard(filename:join(InstallRoot, "erts-*")),
    "erts-" ++ ERTSVsn = filename:basename(ERTSPath),
    Board = rebar3_grisp_util:get([board], Config, ?DEFAULT_GRISP_BOARD),
    copy_files(State3, RelName, RelVsn, Board, ERTSVsn, Dest, Force),
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
            rebar_api:warn(
                "Current Erlang version (~p) does not match target "
                "Erlang version (~p). It is not guaranteed that the "
                "deployed release will work!", [Current, Target]
            )
    end.

make_release(_State, Name, Version, _InstallRoot) when
  Name == undefined; Version == undefined ->
    rebar_api:abort("Release name and/or version not specified", []);
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
    AllApps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
    Tree = case rebar3_grisp_util:grisp_app(AllApps) of
        {[], _} -> grisp_files(rebar_state:dir(State), Board);
        {[Grisp], _} ->
            [GrispFiles, ProjectFiles] = lists:map(
                fun(Dir) -> grisp_files(Dir, Board) end,
                [rebar_app_info:dir(Grisp), rebar_state:dir(State)]
            ),
            maps:merge(GrispFiles, ProjectFiles)
    end,
    Context = [
        {release_name, RelName},
        {release_version, RelVsn},
        {erts_vsn, ERTSVsn}
    ],
    maps:map(
        fun(Target, Source) ->
            write_file(Dest, Target, Source, Force, Context)
        end,
        Tree
    ).

grisp_files(Dir, Board) ->
    Path = filename:join([Dir, "grisp", Board, "files"]),
    resolve_files(find_files(Path), Path).

write_file(Dest, Target, Source, Force, Context) ->
    Path = filename:join(Dest, Target),
    rebar_api:debug("Creating ~p from ~p", [Path, Source]),
    Content = load_file(Source, Context),
    force_execute(Path, Force,
        fun(F) ->
            ensure_dir(F),
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
    Parsed = bbmustache:parse_file(Source),
    bbmustache:compile(Parsed, Context, [{key_type, atom}]);
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
    ensure_dir(Target),
    sh(string:join([Command, Source ++ "/", Target], " ")).

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

ensure_dir(File) ->
    case filelib:ensure_dir(File) of
        ok    -> ok;
        Error -> abort("Could not create target directory: ~p", [Error])
    end.

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
