% @doc {{{name}}} public API.
-module({{{name}}}).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> {{{name}}}_sup:start_link().

% @private
stop(_State) -> ok.
