-module(rebar3_grisp_version).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, grisp},
        {name, version},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp version"},
        {opts, []},
        {profiles, [default]},
        {short_desc, "Print version of plug-in"},
        {desc,
"Print version of plug-in.
"
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar3_grisp_util:console(rebar3_grisp_util:version()),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
