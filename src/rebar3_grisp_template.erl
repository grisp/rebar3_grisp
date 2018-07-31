-module(rebar3_grisp_template).

% API
-export([render/2]).

%--- API -----------------------------------------------------------------------

render(File, Context) ->
    Parsed = bbmustache:parse_file(File),
    bbmustache:compile(Parsed, default(Context), [
        {key_type, atom},
        raise_on_context_miss
    ]).

%--- Internal ------------------------------------------------------------------

default(Context) -> maps:merge(env(), Context).

env() -> #{env => maps:from_list([parse_env(E) || E <- os:getenv()])}.

parse_env(E) ->
    {match, [Name, Value]} = re:run(E, "([^=]+)=(.*)", [
        {capture, all_but_first, binary}
    ]),
    {binary_to_atom(Name, utf8), Value}.
