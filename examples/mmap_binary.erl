-module(mmap_binary).
-export([open/1]).
-on_load(init/0).
-include_lib("eunit/include/eunit.hrl").

init() ->
    ok = erlang:load_nif("./mmap_binary", 0).

open(_Filename) ->
    exit(nif_library_not_loaded).

