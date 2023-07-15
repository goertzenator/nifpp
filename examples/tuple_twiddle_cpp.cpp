//
// tuple_twiddle_cpp.cpp - Demonstrate nifpp tuple manipulation
//
#include "nifpp.h"
#include <functional>

using std::make_tuple;
using std::ref;

extern "C" {

//
// Convert tuple of form {{1,2},3,myatom} to {myatom, 3,{2,1}}.  Fully decode and recode ints.
//
static ERL_NIF_TERM twiddle_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        int a,b,c;
        nifpp::atom atom;
        auto tup_in  = make_tuple( make_tuple(ref(a), ref(b)), ref(c), ref(atom) );
        nifpp::get_throws(env, argv[0], tup_in);
        return nifpp::make(env, make_tuple( atom, c, make_tuple(b, a)));
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = { {"twiddle", 1, twiddle_nif} };

ERL_NIF_INIT(tuple_twiddle_cpp, nif_funcs, NULL, NULL, NULL, NULL)

} //extern C


