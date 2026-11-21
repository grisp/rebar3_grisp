-module({{{name}}}_c_nif).
-export([hello/0]).
-on_load(init/0).

init() -> erlang:load_nif(atom_to_list(?MODULE), 0).
hello() -> erlang:nif_error(nif_not_loaded).
