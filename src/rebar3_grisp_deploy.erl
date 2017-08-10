-module(rebar3_grisp_deploy).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [info/2, console/1, console/2, abort/1, abort/2, sh/1]).

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
                {force, $f, "force", {boolean, false}, "Delete existing release before deploying"}
            ]},
            {profiles, [grisp]},
            {short_desc, "Deploy a GRiSP release to a destination"},
            {desc,
"Deploys a GRiSP application.
"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    info("~p", [Args]),
    Config = rebar_state:get(State, grisp, []),
    check_otp_release(Config),
    Name = proplists:get_value(relname, Args),
    Version = proplists:get_value(relvsn, Args),
    ErlangVersion = "19.3.6",
    State3 = make_release(State, Name, Version, ErlangVersion),
    Force = proplists:get_value(force, Args),
    Dest = case proplists:get_value(destination, Args) of
        undefined -> rebar3_grisp_util:get([deploy, destination], Config);
        Value     -> Value
    end,
    info("Deploying ~s-~s to ~s", [Name, Version, Dest]),
    copy_release(State3, Name, Version, Dest, Force),
    {ok, State3}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

check_otp_release(Config) ->
    try
        OTPRelease = rebar3_grisp_util:get(otp_release, Config, undefined),
        case {OTPRelease, erlang:system_info(otp_release)} of
            {Target, Target} -> ok;
            {Target, Current} ->
                rebar_api:warn(
                    "Current Erlang version (~p) does not match target "
                    "Erlang version (~p). It is not guaranteed that the "
                    "deployed release will work!", [Current, Target]
                )
        end
    catch
        {key_not_found, [otp_release], _} ->
            abort(
                "GRiSP OTP release in rebar.config not configured:"
                "~n~n{grisp, [{otp_release, \"<VERSION>\"}]}"
            )
    end.

make_release(_State, Name, Version, _ErlangVersion) when
  Name == undefined; Version == undefined ->
    rebar_api:abort("Release name and/or version not specified", []);
make_release(State, Name, Version, ErlangVersion) ->
    InstallRoot = rebar3_grisp_util:otp_install_root(State, ErlangVersion),
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

copy_release(State, Name, Version, Dest, Force) ->
    Source = filename:join([rebar_dir:base_dir(State), "rel", Name]),
    Target = filename:join(Dest, Name),
    case {filelib:is_file(Target), Force} of
        {true, true} ->
            console("* Removing old release..."),
            sh("rm -rf " ++ Target);
        {true, _} ->
            abort("Destination ~s already exists (use --force to overwrite)", [Target]);
        _ ->
            ok
    end,
    case filelib:ensure_dir(Target) of
        ok    -> ok;
        Error -> abort("Could not create target directory: ~p", [Error])
    end,
    console("* Copying release..."),
    sh("cp -R " ++ Source ++ " " ++ Target).
