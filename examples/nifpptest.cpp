
#include <iostream>
using std::cout;
using std::cerr;
using std::endl;

#include <functional>

using std::ref;

#define NIFPP_INTRUSIVE_UNIT_TEST
#include "nifpp.h"


using namespace nifpp;

// list test abstracted over container type and contained element type
template<typename T> // supported container type
ERL_NIF_TERM list2_test(ErlNifEnv* env, ERL_NIF_TERM term)
{
    T container;
    get_throws(env, term, container);

    // append reversed copy of container to itself
    T dup = container;
    for(auto i=dup.rbegin(); i!=dup.rend(); i++)
    {
        //container.push_back(*i);
        container.insert(container.end(), *i);
    }

    return make(env, container);
}


class tracetype
{
public:
    static int ctor_cnt;
    static int dtor_cnt;
    tracetype() { ctor_cnt++; }
    ~tracetype() { dtor_cnt++; }
    static void reset()
    {
        ctor_cnt = 0;
        dtor_cnt = 0;
    }

    int x;
};

int tracetype::ctor_cnt;
int tracetype::dtor_cnt;

ERL_NIF_TERM nif_main(ErlNifEnv* env, ERL_NIF_TERM term)
{
    
    str_atom cmd;
    ERL_NIF_TERM cmddata;
    auto cmdtup=std::tie(cmd,cmddata);
    get_throws(env, term, cmdtup);
    cout << "cmd = " << cmd << endl;
    if(cmd=="atom2")
    {
        str_atom in;
        str_atom out;
        get_throws(env, cmddata, in);
        out = in;
        out += in;
        return make(env,out);
    }
    else if(cmd=="string2")
    {
        std::string in;
        std::string out;
        get_throws(env, cmddata, in);
        out = in;
        out += in;
        return make(env,out);
    }
    else if(cmd=="double2")
    {
        double in;
        double out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="int2")
    {
        int in;
        int out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="uint2")
    {
        unsigned int in;
        unsigned int out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="long2")
    {
        long in;
        long out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="booln")
    {
        bool in;
        bool out;
        get_throws(env, cmddata, in);
        out = !in;
        return make(env,out);
    }
    else if(cmd=="tuple2a")
    {
        // test complex tie() coding
        // double each member of {123, "abc", {xyz, 456}, 500.0}}
        int a;
        std::string b;
        str_atom c;
        int d;
        double e;

        auto tup = make_tuple(ref(a),ref(b),std::tie(c,d),ref(e));
        get_throws(env, cmddata, tup);

        a*=2;
        b = b + b;
        c = c + c;
        d*=2;
        e*=2;

        return make(env, tup);
    }
    else if(cmd=="tuple2b")
    {
        // test simpler tie() coding
        // double each member of {123, "abc", 500.0}
        int a;
        std::string b;
        double e;

        auto tup = tie(a,b,e);
        get_throws(env, cmddata, tup);

        a*=2;
        b = b + b;
        e*=2;

        return make(env, tup);
    }
    else if(cmd=="tuple2c")
    {
        // non-ref tuple test
        // double each member of {123, "abc", {xyz, 456}, 500.0}}

        std::tuple<int, std::string, std::tuple<str_atom, int>, double>  tup;
        get_throws(env, cmddata, tup);

        int &a = std::get<0>(tup);
        std::string &b = std::get<1>(tup);
        str_atom &c = std::get<0>(std::get<2>(tup));
        int &d =  std::get<1>(std::get<2>(tup));
        double &e = std::get<3>(tup);

        a*=2;
        b = b + b;
        c = c + c;
        d*=2;
        e*=2;

        return make(env, tup);
    }
//    else if(cmd=="tuple2d")
//    {
//        // test multi-param get/make
//        // double each member of {123, "abc", 500.0}
//        int a;
//        std::string b;
//        double e;

//        get_throws(env, cmddata, a, b, e);

//        a*=2;
//        b = b + b;
//        e*=2;

//        return make(env, a, b, e);
//    }
//    else if(cmd=="tuple2e")
//    {
//        // test multi-param get/make (nested)
//        // double each member of {123, "abc", {xyz, 456}, 500.0}}
//        int a;
//        std::string b;
//        str_atom c;
//        int d;
//        double e;

//        auto inner_tup = std::tie(c,d);
//        get_throws(env, cmddata, a, b, inner_tup, e);

//        a*=2;
//        b = b + b;
//        c.name = c.name + c.name;
//        d*=2;
//        e*=2;

//        return make(env, a, b, inner_tup, e);
//    }
    else if(cmd=="list2aa") { return list2_test<std::vector   <int> >(env, cmddata); }
    else if(cmd=="list2ab") { return list2_test<std::list     <int> >(env, cmddata); }
    else if(cmd=="list2ac") { return list2_test<std::deque    <int> >(env, cmddata); }
    else if(cmd=="list2ad") { return list2_test<std::set      <int> >(env, cmddata); }
    else if(cmd=="list2ae") { return list2_test<std::multiset <int> >(env, cmddata); }

    else if(cmd=="list2ba") { return list2_test<std::vector   <ERL_NIF_TERM> >(env, cmddata); }
    else if(cmd=="list2bb") { return list2_test<std::list     <ERL_NIF_TERM> >(env, cmddata); }
    else if(cmd=="list2bc") { return list2_test<std::deque    <ERL_NIF_TERM> >(env, cmddata); }
    else if(cmd=="list2bd") { return list2_test<std::set      <ERL_NIF_TERM> >(env, cmddata); }
    else if(cmd=="list2be") { return list2_test<std::multiset <ERL_NIF_TERM> >(env, cmddata); }

    else if(cmd=="list2ca") { return list2_test<std::vector   <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2cb") { return list2_test<std::list     <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2cc") { return list2_test<std::deque    <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2cd") { return list2_test<std::set      <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2ce") { return list2_test<std::multiset <std::tuple<int, std::string> > >(env, cmddata); }

    // increment 2nd element of std::array<int, 5>
    else if(cmd=="stdarray_inc2")
    {
        std::array<int, 5> array;
        get_throws(env, cmddata, array);
        array[2]++;
        return make(env, array);
    }

    // copy 3rd elemend to 2nd  of std::array<ERL_NIF_TERM, 5>
    else if(cmd=="stdarray_cp32")
    {
        std::array<ERL_NIF_TERM, 5> array;
        get_throws(env, cmddata, array);
        array[2]=array[3];
        return make(env, array);
    }

    // basic resource testing
    else if(cmd=="makeresint")
    {
        int num;
        get_throws(env, cmddata, num);

        auto ptr = construct_resource<int>(num);
        return make(env, ptr); // make resource term
    }
    else if(cmd=="incresint")
    {
        int *rawptr;
        get_throws(env, cmddata, rawptr);
        (*rawptr)++;
        return make(env, *rawptr); // return value of int
    }
    else if(cmd=="decresint")
    {
        resource_ptr<int> ptr;
        get_throws(env, cmddata, ptr);
        (*ptr)--;
        return make(env, *ptr); // return value of int
    }

    // verify resource destruction
    else if(cmd=="tracetype_reset")
    {
        tracetype::reset();
        return make(env,str_atom("ok"));
    }
    else if(cmd=="tracetype_create")
    {
        return make(env, construct_resource<tracetype>());
    }
    else if(cmd=="tracetype_getcnts")
    {
        return make(env, std::make_tuple(tracetype::ctor_cnt, tracetype::dtor_cnt));
    }

    // test binaries
    else if(cmd=="bin2")
    {
        ErlNifBinary ebin;
        get_throws(env, cmddata, ebin);
        binary newbin(ebin.size*2);
        std::memcpy(newbin.data,           ebin.data, ebin.size);
        std::memcpy(newbin.data+ebin.size, ebin.data, ebin.size);

        // make sure these give compile errors:
        //binary bincopy = newbin; //error
        //binary bincopy2(newbin); //error
        //binary bincopy3(std::move(newbin)); //error

        return make(env, newbin);
    }

    // verify binary destruction
    else if(cmd=="binary_release_counter_reset")
    {
        binary::release_counter=0;
        return make(env,str_atom("ok"));
    }
    else if(cmd=="binary_release_counter_get")
    {
        return make(env,binary::release_counter);
    }
    else if(cmd=="bina")
    {
        binary a(10);
        binary b(20);
        binary c(20);
        binary d(20);
        make(env, b);
        return make(env,str_atom("ok"));
        // expect 3 release callss
    }



#if SIZEOF_LONG != 8
    else if(cmd=="longlongint2")
    {
        long long int in;
        long long int out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
#endif
    cout << "cmd no match" << endl;
    return enif_make_badarg(env);
}


extern "C" {

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    register_resource<std::string>(env, nullptr, "std::string");
    register_resource<int>(env, nullptr, "int");
    register_resource<tracetype>(env, nullptr, "tracetype");
    return 0;
}

static ERL_NIF_TERM invoke_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        return nif_main(env, argv[0]);
    }
    catch(nifpp::badarg)
    {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }
    ret = x*2;
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    if (!enif_get_int(env, argv[0], &y)) {
        return enif_make_badarg(env);
    }
    ret = y*3;
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"invoke_nif", 1, invoke_nif},
};

ERL_NIF_INIT(nifpptest, nif_funcs, load, NULL, NULL, NULL)
} //extern C
