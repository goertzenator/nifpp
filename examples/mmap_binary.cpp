//
// mmap_binary.cpp - Memory map a file and return as a resource binary to Erlang.
// Requires the Boost library and linkage with libboost_iostreams-mt
//
#include "nifpp.h"
#include <boost/iostreams/device/mapped_file.hpp>

using boost::iostreams::mapped_file_source; // encapsulates read-only memory-mapped file

extern "C" {

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    nifpp::register_resource<mapped_file_source>(env, nullptr, "mapped_file_source");
    return 0;
}

static ERL_NIF_TERM open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        auto map_ptr = nifpp::construct_resource<mapped_file_source>( nifpp::get<std::string>(env, argv[0]) );
        return nifpp::make_resource_binary(env, map_ptr, (const void *)(map_ptr->data()), map_ptr->size());
    }
    catch(nifpp::badarg) {}
    catch(std::ios_base::failure) {}
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = { {"open", 1, open_nif} };

ERL_NIF_INIT(mmap_binary, nif_funcs, load, NULL, NULL, NULL)

} //extern C

