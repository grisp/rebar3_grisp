-module(rebar3_grisp_version).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    console/1,
    console/2
]).

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
    {ok, _} = application:ensure_all_started(rebar3_grisp),
    lists:foreach(fun(A) ->
        console("~p: ~s (~s)", [A, version(A), path(A)])
    end, [rebar3_grisp] ++ deps(rebar3_grisp)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

version(App) ->
    case application:get_key(App, vsn) of
        {ok, Version} -> Version;
        undefined     -> error({version_missing, App})
    end.

path(App) ->
    case code:priv_dir(App) of
        {error, bad_name} = Error -> error(Error);
        Path -> filename:dirname(Path)
    end.

deps(App) ->
    {ok, Deps} = application:get_key(App, applications),
    Deps -- [kernel, stdlib].
