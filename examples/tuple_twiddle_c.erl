-module(tuple_twiddle_c).
-export([twiddle/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./tuple_twiddle_c", 0).

twiddle(_T) ->
    exit(nif_library_not_loaded).
