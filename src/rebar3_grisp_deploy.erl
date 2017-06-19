-module(rebar3_grisp_deploy).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, deploy},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, release}]},
            {example, "rebar3 grisp deploy"},
            {opts, [
                {dest, $d, "dest", string, "destination path"}
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
    Config = rebar_state:get(State, grisp, []),
    check_otp_release(Config),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

check_otp_release(Config) ->
    case proplists:get_value(otp_release, Config) of
        undefined ->
            rebar_api:abort(
                "GRiSP OTP release in rebar.config not configured:"
                "~n~n{grisp, [{otp_release, \"<VERSION>\"}]}", []
            );
        OTPRelease ->
            case {OTPRelease, erlang:system_info(otp_release)} of
                {Target, Target} -> ok;
                {Target, Current} ->
                    rebar_api:warn(
                        "Current Erlang version (~p) does not match target"
                        "Erlang version (~p). It is not guaranteed that the"
                        "deployed release will work!", [Current, Target]
                    )
                end
    end.
