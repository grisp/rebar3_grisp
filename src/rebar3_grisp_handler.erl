-module(rebar3_grisp_handler).

% API
-export([shell/3]).

-import(rebar3_grisp_util, [debug/2]).

%--- API -----------------------------------------------------------------------

shell(RawCommand, Opts, State) ->
    Command = binary_to_list(iolist_to_binary(RawCommand)),
    debug("sh command:~n~s", [Command]),
    Result = case lists:member(return_on_error, Opts) of
        true -> rebar3_grisp_util:sh(Command, Opts);
        false -> rebar3_grisp_util:sh(Command, [ abort_on_error | Opts])
    end,
    shell_output(Result),
    {Result, State}.

shell_output({ok, Output}) ->
    debug("sh output:~n~s", [Output]);
shell_output({error, {Code, Output}}) ->
    debug("sh error (exit code ~p):~n~s", [Code, Output]).
