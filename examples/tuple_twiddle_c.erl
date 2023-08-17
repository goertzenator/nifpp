-module(tuple_twiddle_c).
-export([twiddle/1, test/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./tuple_twiddle_c", 0).

%% Argument must be in the form {{_, _}, _}
twiddle(_T) ->
    exit(nif_library_not_loaded).

test() ->
    {3, {2, 1}} = twiddle({{1, 2}, 3}).
