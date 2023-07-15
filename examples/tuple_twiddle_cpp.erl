-module(tuple_twiddle_cpp).
-export([twiddle/1, test/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./tuple_twiddle_cpp", 0).

twiddle(_T) ->
    exit(nif_library_not_loaded).

test() ->
    twiddle({{1, 2}, 3, atom}).
