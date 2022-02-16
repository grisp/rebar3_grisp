-module(rebar3_grisp_package).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    debug/2,
    info/1,
    info/2,
    console/1,
    console/2,
    warn/1,
    warn/2,
    abort/1,
    abort/2
]).

%--- Macros --------------------------------------------------------------------

-define(NAMESPACE, grisp).
-define(TASK, package).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, ?NAMESPACE},
            {name, ?TASK},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}]},
            {example, "rebar3 grisp " ++ atom_to_list(?TASK)},
            {opts, [
                {platform, $p, "platform", string, "Platform to list packages for"},
                {hash, $x, "hash", {boolean, false}, "List package hashes"}
            ]},
            {profiles, [grisp]},
            {short_desc, "Pre-built package tasks"},
            {desc,
                "Pre-built packages tasks~n"
                "~n"
                "Commands:~n"
                "  list\t\tList packages~n"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {Args, _Rest} = ParsedArgs = rebar_state:command_parsed_args(RState),
    RState2 = case proplists:get_value(help, Args) of
        true ->
            task_help(proplists:get_value(task, Args), ParsedArgs, RState);
        undefined ->
            task_run(proplists:get_value(task, Args), ParsedArgs, RState)
    end,
    {ok, RState2}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

task_help("list", _Args, RState) ->
    console(
        "List packages~n"
        "~n"
        "Usage: rebar3 grisp packages list [--platform <platform>]~n"
        "~n"
        "  -p, --platform  Platform to list packages for~n"
        "  -x, --hash      List package hashes [default: false]~n"
    ),
    RState;
task_help(undefined, _Args, RState) ->
    Args = [atom_to_list(A) || A <- [?NAMESPACE, ?TASK]],
    {ok, RState2} = rebar_prv_help:do(rebar_state:command_args(RState, Args)),
    RState2.

task_run("list", {Args, _Rest}, RState) ->
    Config = rebar3_grisp_util:config(RState),
    Platform = to_atom(proplists:get_value(platform, Args, rebar3_grisp_util:platform(Config))),
    Versions = grisp_tools:list_packages(#{
        type => otp,
        platform => Platform
    }),
    info("GRiSP pre-built OTP versions for platform '~p'", [Platform]),
    case proplists:get_bool(hash, Args) of
        false ->
            [console("~s", [VS]) || VS <- lists:usort([V || #{version := V} <- Versions])];
        true ->
            [console("~s\t~s", [V, H]) || #{version := V, hash := H} <- Versions]
    end,
    RState;
task_run(Task, _Args, _State) ->
    abort("~p: unknown task: ~s", [?TASK, Task]).

to_atom(Term) when is_atom(Term) -> Term;
to_atom(Term) when is_list(Term) -> list_to_atom(Term).
