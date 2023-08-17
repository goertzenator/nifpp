//
// tuple_twiddle_cpp.cpp - Demonstrate nifpp tuple manipulation
//
#include "nifpp.h"
#include <functional>

using std::make_tuple;
using std::ref;

extern "C" {

//
// Convert tuple of form {{1,2},3} to {3,{2,1}}.  Fully decode and recode ints.
//
static ERL_NIF_TERM twiddle_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    int a,b,c;
    auto tup_in  = make_tuple( make_tuple(ref(a), ref(b)), ref(c) );
    return nifpp::get(env, argv[0], tup_in)
         ? nifpp::make(env, make_tuple( c, make_tuple(b, a)))
         : enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = { {"twiddle", 1, twiddle_nif, 0} };

ERL_NIF_INIT(tuple_twiddle_cpp, nif_funcs, NULL, NULL, NULL, NULL)

} //extern C


