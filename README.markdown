# nifpp (v2): C++11 Wrapper for Erlang NIF API

## Introduction

Nifpp enhances the Erlang NIF API for C++ by providing: 

- Overloaded get()/make() wrappers for the enif_get_xxx()/enif_make_xxx() C API.
- get()/make() support for STL containers tuple, vector, array, list, deque, set, unordered_set, multiset, map, and unordered_map.
- get()/make() support for nested containers.
- A resource pointer type so that any type can be easily used as a NIF resource.  Think of it as a std::shared_ptr that the emulator can hold references to.

### Changes for version 2
- Patch for erl_nif.h is no longer needed.  The new type nifpp::TERM replaces patch functionality.
- Additional container types.
- Support for v17 maps API.

See github branch v1 for the old version.

## Installation

Nifpp is provided as a single header file.  Copy `nifpp.h` into your nif source directory.  Wherever you would write


	#include <erl_nif.h>

instead write

	#include “nifpp.h”

All nifpp functions are available in the `nifpp` namespace.  The C API remains available in the global namespace, and it may be mixed with nifpp functions.


## C++11 compatibility
Nifpp was tested with gcc-4.8, but should also work with 4.6 and 4.7.  Activate c++11 support with "--std=c++11".

I did not tested with clang, but I have no reason to think that it will not work.  Please tell me if there are issues.

I understand that c++11 support in MSVC 2013 is still marginal, so I do not have high hopes of success with that.  Do tell me if it works.

## unsigned long, ERL_NIF_TERM, and nifpp::TERM

The header file "erl_nif.h" defines ERL_NIF_TERM as an integer (usually unsigned long.)  This is trouble for the overloaded functions in this library, for example...

    ERL_NIF_TERM  a = ...;
    unsigned long b = ...;
    auto t = nifpp::make(env, std::make_tuple(a, b));  // Are a and b terms or integers?? :(

To work around this problem, this library introduces the type nifpp::TERM which is a simple struct{} wrapper for ERL_NIF_TERM.  nifpp::TERM can be used in overloaded functions.

    nifpp::TERM   a = ...;
    unsigned long b = ...;
    auto t = nifpp::make(env, std::make_tuple(a, b));  // Ahhh, a is term, and b is ulong. :)

nifpp::TERM implicitly casts to ERL_NIF_TERM, so it can be used wherever a ERL_NIF_TERM is required.  Casts from ERL_NIF_TERM to nifpp::TERM must be explicit.  Example.



    void my_support_function(nifpp::TERM t1)
    {...}

    static ERL_NIF_TERM my_nif_function(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
    {
        ERL_NIF_TERM a;
        nifpp::TERM  b;

        // don't do this....
        my_support_function(a);       // can't convert ERL_NIF_TERM to nifpp::TERM.
        auto t = nifpp::make(env, a); // a interpreted as unsigned long

        // but do this...
        my_support_function(nifpp::TERM(a));   // explicit cast
        my_support_function(b);                // no cast needed
        auto t = nifpp::make(env, b);          // b recognized as term

        enif_send(..., b);     // implicit conversion to ERL_NIF_TERM
        return b;              // implicit conversion to ERL_NIF_TERM

    }




## get() and make()

Nifpp provides overloaded wrappers for most of the `enif_get_XXX()` and `enif_make_XXX()` functions.  The prototypes are:

	bool nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, T &var);
	nifpp::TERM nifpp::make(ErlNifEnv *env, const T &var);
	
`get()` will return true on success, false on failure.

There are some additional template wrappers for `get()`:

	void nifpp::get_throws(ErlNifEnv *env, ERL_NIF_TERM term, T &var);
	T nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term);
		
Both forms will throw `nifpp::badarg` upon failure.  Note that for the last form, the type must be explicitly specified since it cannot be inferred, for example:

	int i = nifpp::get(env, term);       // compile error
	int i = nifpp::get<int>(env, term);  // success!

### Plain-Old-Data Types

The following POD types are supported:

- `double`
- `int`
- `unsigned int`
- `long`
- `unsigned long`
- `ErlNifSInt64`
- `ErlNifUInt64`
- `bool`
- `nifpp::TERM`

Note that `nifpp::get()`/`nifpp::make()` for `nifpp::TERM` simply outputs the input term without conversion.  This is useful in conjunction with tuple and list `nifpp::get()`/`nifpp::make()` (see below).

Examples:

	// get() example 1
	long a;
	if(nifpp::get(env, term, a))
	{ … do something with a...}
	
	// get() example 2
	try {
	  double b;
	  nifpp::get_throws(env, term, b);
	  … do something with b...
	}
	except(nifpp::badarg) {}
	
	// get() example 3  
	try {
	  auto c = nifpp::get<ErlNifPid>(env, term);
	  … do something with c...
	}
	except(nifpp::badarg) {}
	
	// make() example 1
	nifpp::TERM output;
	output = nifpp::make(env, a);
	return output;
	
	// make() example 2
	auto output = nifpp::make(env, b);
	return output;
	
	// make() example 3
	return nifpp::make(env, c);
	
  
### Strings

String are represented by `std::string`.  Examples:

	// get() example:
	std::string a;
	nifpp::get_throws(env, term, a);
	… do something with a...
	
	// make() example 1:
	nifpp::TERM term = nifpp::make(env, “hello world”);
	
	// make() example 2:
	std::string a(“hello world”);
	nifpp::TERM term = nifpp::make(env, a);

### Atoms

nifpp represents atoms by the type `nifpp::str_atom` or type which is a thin
wrapper around `std::string` or `nifpp::atom`, which is a wrapper around
`ERL_NIF_TERM`.


	// get() example:
	nifpp::str_atom a;
	nifpp::get_throws(env, term, a);
	… do something with a...
	
	// make() example 1:
	nifpp::TERM term = nifpp::make(env, nifpp::str_atom(“hello world”));
	
	// make() example 2:
	nifpp::str_atom a(“hello world”);
	nifpp::TERM term = nifpp::make(env, a);

    // make() example 3:
	nifpp::atom a;
	nifpp::get_throws(env, term, a);
	… do something with a...

    // make() example 4:
    nifpp::TERM term = nifpp::atom(env, "abc");

    // make() example 5:
    nifpp::atom a;
    a.init(env, "abc");
    if (nifpp::atom(env, "abc") == a)
        … do something with a...

    ... do something ...

### Binaries

ErlNifBinary is directly supported by get()/make(), for Example:
    
    // inspect binary
    ErlNifBinary ebin;
    nifpp::get_throws(env, term, ebin);
    ...inspect contents of ebin...
    
    // create binary
    ErlNifBinary ebin;
    enif_alloc_binary(2000, &ebin);
    ...copy data into ebin...
    nifpp::TERM term = nifpp::make(env, ebin);
    
The type `nifpp::binary` is also supplied to assist in safe creation of binaries.  `nifpp::binary` is derived from ErlNifBinary
and will automatically release the allocated memory if it was never made into a term.  For example:
    
    // create binary using "binary" type
    try
    {
        nifpp::binary nbin(2000);
        ...copy data into nbin, maybe throw...
        ERL_NIF_TERM term = nifpp::make(env, nbin);
    }
    catch(...)
    {} // nbin released here if nifpp::make() was not called on it.
    
    
    


### Tuples

Tuples are represented by the C++11 type `std::tuple`.  Tuples-of-references are a powerful method for cracking and packing Erlang tuple terms.  Examples:

	// crack simple tuple {hello, 14} using tuple-of-references
	nifpp::str_atom a;
	int b;
	auto tup = std::make_tuple( std::ref(a), std::ref(b) );
	nifpp::get_throws(env, term, tup);
	
	// crack nested tuple {hello, 14, {10,4}} using tuple-of-references
	nifpp::str_atom a;
	int b;
	int c;
	int d;
	auto tup = std::make_tuple( std::ref(a), std::ref(b),
	     std::make_tuple( std::ref(c), std::ref(d) ));
	nifpp::get_throws(env, term, tup);
	

`std::tie()` offers syntactic sugar for composing a tuple-of-references.  Note that the result of `std::tie()` is not a reference, so it cannot be used in the top-level tuple in the nested tuple example.  Examples:

	// crack simple tuple {hello, 14} using tuple-of-references [using std::tie()]
	nifpp::str_atom a;
	int b;
	auto tup = std::tie( a, b );
	nifpp::get_throws(env, term, tup);
	
	// crack nested tuple {hello, 14, {10,4}} using tuple-of-references [using std::tie()]
	nifpp::str_atom a;
	int b;
	int c;
	int d;
	auto tup = std::make_tuple( std::ref(a), std::ref(b), std::tie( c, d) );
	nifpp::get_throws(env, term, tup);


	`nifpp::TERM` can be used to defer decoding of tuple elements.  Example:
	
	// partial crack
	nifpp::str_atom type;
	nifpp::TERM value;
	auto tup = std::tie( type, value );
	nifpp::get_throws(env, term, tup);
	… decode value based on type


If you want to use `std::tuple` for the C++ side of your code, you can use regular value tuples too.  Example:

	// crack plain tuple
	std::tuple<str_atom, int, ERL_NIF_TERM> tup;
	nifpp::get_throws(env, term, tup);

And here are some examples of tuple packing...

	ERL_NIF_TERM term;
	nifpp::str_atom a(“hello”);
	int b(4);
	term = nifpp::make(env, std::make_tuple(a,b));
	
	ERL_NIF_TERM term;
	auto tup = std::make_tuple(nifpp::str_atom(“hello”), 4);
	term = nifpp::make(env, tup);

	ERL_NIF_TERM term = nifpp::make(env, std::make_tuple(nifpp::str_atom(“hello”), 4));
	
	



### Lists

Lists can be represented by a number of types:

- `std::vector`
- `std::list`
- `std::deque`
- `std::set`
- `std::multiset`

`nifpp::get()` and `nifpp::make()` may be used on all of them to decode/create an Erlang list.  Examples:

	std::vector<int> myintlist;
	nifpp::get_throws(env, term, myintlist);
	
	std::vector<ERL_NIF_TERM> mylist;
	nifpp::get_throws(env, term, mylist);
	
	std::deque<int> mydeque;
	mydeque.push_back(44);
	...
	nifpp::TERM term = nifpp::make(env, mydeque);


There is also a special Erlang list iteration function:

	bool nifpp::list_foreach(ErlNifEnv *env, ERL_NIF_TERM list_term, std::function<void (ErlNifEnv *, ERL_NIF_TERM item)>);

This is useful if you want to iterate through a list without copying the entire thing.  The following `nifpp::get()` function is a good example of `nifpp::list_foreach` usage:

[list for each example]

### Maps

Erlang maps can be represented by `std::map` and `std::unordered_map`.  Maps are only available when compiling NIFs on Erlang v17 or later.

`nifpp::get()` and `nifpp::make()` are used to decode/create Erlang maps.  Examples:

    std::map<nifpp::str_atom, nifpp::TERM> map1;
    nifpp::get_throws(env, term, map1);

    std::map<nifpp::str_atom, int> map2;
    nifpp::get_throws(env, term, map2);

    std::map<nifpp::str_atom, int> map3;
    map3["abc"] = 123;
    map3["pqr"] = 456;
    nifpp::TERM term = nifpp::make(env, map3);

### Resources

The following forms are for working with resources:

	bool nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, T * &var);
	bool nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, resource_ptr<T> &var);
	nifpp::TERM nifpp::make(ErlNifEnv *env, const resource_ptr<T> &var)
	
See section below for details on nifpp resources.

### Resource Binaries

TODO: write this section.

See "Memory Mapped Binary Resource" example below.

## Resources

Nifpp allows any C++ type to be used as a resource.  All resource types must be registered using:

	template<typename T>
	int nifpp::register_resource(
		ErlNifEnv* env,
		const char* module_str,
		const char* name,
		ErlNifResourceFlags flags = ErlNifResourceFlags(ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER),
		ErlNifResourceFlags* tried = nullptr);
			


This function does a number of things:

1. Create static storage for `ErlNifResourceType` pointer.
2. Create a resource destructor that invokes the object’s C++ destructor.
3. Call `enif_open_resource_type()` and save result.

Registrations must appear in the nif module’s `load()` function, for example:

	static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
	{
	   nifpp::register_resource<std::string>(env, nullptr, "std::string");
	   nifpp::register_resource<int>(env, nullptr, "peanutbutter toast, yum");
	   nifpp::register_resource<MyClass>(env, nullptr, "MyClass");
	   return 0;
	}


Objects are created with:

	nifpp::resource_ptr<T> nifpp::construct_resource<T>(Args...);

Where `Args` is a set of parameters accepted by any of `T`’s constructors.  Internally this function calls `enif_allocate_resource()` and then performs a placement new on the allocated memory.  Exceptions in the constructor are properly handled; the memory will immediately be released and the C++ destructor will not be invoked.

`resource_ptr` is a reference counting pointer similar to `std::shared_ptr`.  Creating copies of a valid `nifpp::resource_ptr<>` will invoke `enif_keep_resource()`, and destroying instances will invoke `enif_release_resource()`.  `resource_ptr<>` may be safely kept around after the NIF function call returns.

Resource terms are created with `nifpp::make()`:

	nifpp::TERM nifpp::make(ErlNifEnv *env, const resource_ptr<T> &var);

The object pointer can be retrieved from a resource term with

	bool nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, T * &var);
	bool nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, nifpp::resource_ptr<T> &var);
	
The `T*` version does not affect reference counts, but should not be kept around after the NIF call returns.  The `nifpp::resource_ptr<T>` version does increase reference count as described above, but does not have the same restrictions.

Examples of resource construction:

	nifpp::resource_ptr<int> ptr = nifpp::construct_resource<int>();     //default ctor
	nifpp::resource_ptr<int> ptr = nifpp::construct_resource<int>(123);
	auto ptr = nifpp::construct_resource<int>(123);
	auto ptr = nifpp::construct_resource<std::string>(“cupcakes”);
	auto ptr = nifpp::construct_resource<vector<std::string>>(5000, “many cupcakes”);
	auto ptr = nifpp::construct_resource<MyClass>(p1, p2, p3, p4);
	auto ptr = nifpp::construct_resource<std::shared_ptr<MyClass>>(new MyClass(p1, p2, p3, p4));
	
Any of the above resources can be made into a term with:

	nifpp::TERM term = nifpp::make(env, ptr);

Also, any of the pointers may be kept around after the NIF call returns.  Be sure to use C++11 copy constructor semantics to prevent superfluous `keep()`/`release(`) calls:

	auto ptr = nifpp::construct_resource<std::string>(“cupcakes”);
	bakery.front_window = std::move(ptr);
	

Examples of getting objects from resource terms:


	// these are for use within this nif call only.
	int *ptr;
	std::string *ptr;
	vector<std::string>> *ptr;
	MyClass *ptr;
	std::shared_ptr<MyClass> *ptr;
	
	// these may be stored in between nif calls.
	nifpp::resource_ptr<int> ptr;
	nifpp::resource_ptr<std::string> ptr;
	nifpp::resource_ptr<vector<std::string>>> ptr;
	nifpp::resource_ptr<MyClass> ptr;
	nifpp::resource_ptr<std::shared_ptr<MyClass>> ptr;
	
	// all pointer types retrieved with...
	nifpp::get(env, term, ptr);



## Complete examples

### Tuple Twiddle

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
    static ERL_NIF_TERM twiddle_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
    {
        try
        {
            int a,b,c;
            auto tup_in  = make_tuple( make_tuple(ref(a), ref(b)), ref(c) );
            nifpp::get_throws(env, argv[0], tup_in);
            return nifpp::make(env, make_tuple( c, make_tuple(b, a)));
        }
        catch(nifpp::badarg) {}
        return enif_make_badarg(env);
    }

    static ErlNifFunc nif_funcs[] = { {"twiddle", 1, twiddle_nif} };

    ERL_NIF_INIT(tuple_twiddle_cpp, nif_funcs, NULL, NULL, NULL, NULL)

    } //extern C


### Memory Mapped Binary Resource

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

Disclaimer: Reading memory mapped files will cause scheduler havoc while waiting for memory pages to load.  If you have real time work in other Erlang processes that needs to get done, then expect problems.

## Performance

The example programs `tuple_twiddle_cpp.cpp` and `tuple_twiddle_c.c` where benchmarked from `nifpptest.erl`.  When compiled under gcc-4.8.1 with the `-O3` option, the median results were...

- `tuple_twiddle_cpp`: 1744 uSec
- `tuple_twiddle_c`: 1756 uSec


Without the `-O3` switch the results are...

- `tuple_twiddle_cpp`: 8347 uSec
- `tuple_twiddle_c`: 1904 uSec

This disparity is typical of template-heavy C++.  The moral here is to always use `-O3` with nifpp (`-O2` does pretty good too).
