-module({{{name}}}_rust_nif).
-export([hello/0]).
-on_load(init/0).
-include("cargo.hrl").

init() -> ?load_nif_from_crate("{{{name}}}_native", 0).
hello() -> erlang:nif_error(nif_not_loaded).
