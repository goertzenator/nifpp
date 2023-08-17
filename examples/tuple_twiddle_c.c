/*
   tuple_twiddle_c.c - C reference version of tuple manipulation
*/
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <erl_nif.h>
#pragma GCC diagnostic pop

/*
  Convert tuple of form {{1,2},3} to {3,{2,1}}.  Fully decode and recode ints.
*/
static ERL_NIF_TERM twiddle_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int a,b,c;
    const ERL_NIF_TERM *outer, *inner;
    ERL_NIF_TERM tup[2];
    int arity;
    (void)argc;

    if(!enif_get_tuple(env, argv[0], &arity, &outer)) goto error;
    if(arity!=2) goto error;
    if(!enif_get_tuple(env, outer[0], &arity, &inner)) goto error;
    if(arity!=2) goto error;
    if(!enif_get_int(env, inner[0], &a)) goto error;
    if(!enif_get_int(env, inner[1], &b)) goto error;
    if(!enif_get_int(env, outer[1], &c)) goto error;
    tup[0] = enif_make_int(env,c);
    tup[1] = enif_make_tuple2(env, enif_make_int(env, b), enif_make_int(env,a));
    return enif_make_tuple_from_array(env, tup, 2);
error:
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = { {"twiddle", 1, twiddle_nif, 0} };

ERL_NIF_INIT(tuple_twiddle_c, nif_funcs, NULL, NULL, NULL, NULL)


