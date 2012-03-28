# nifpp: C++11 Wrapper for Erlang NIF API

## Introduction

Nifpp enhances the Erlang NIF API for C++ by providing: 

- Conversions to/from C++ container types, namely tuple, vector, list, deque, set, and multiset.
- A safe resource wrapper so that any type can be created as a resource.

## Installation

Nifpp is provided as a single header file.  Copy `nifpp.h` into your nif source directory.  Wherever you would write


	#include <erl_nif.h>

instead write

	#include “nifpp.h”

All nifpp functions are available in the `nifpp` namespace.  The C API remains available in the global namespace, and it may be mixed with nifpp functions.


### erl_nif.h patch
nifpp requires a small patch to `erl_nif.h`.  Locate your `erl_nif.h` and patch it with a command like...

	patch -p4 /path/to/erl_nif.h <erl_nif.h.patch

Nifpp relies on overloaded functions.  The type `ERL_NIF_TERM` in erl_nif.h is defined as an integer, and this creates conflicts when nifpp wants to deal with actual integers.  The patch changes the type of ERL_NIF_TERM to a C++11 enum to avoid the conflict.  The new type is activated by a #define in nifpp.h, so the patched erl_nif.h remains compatible with non-nifpp users.


## C++11 compatibility
Nifpp was tested with gcc-4.6, but may work with older versions.  Activate c++11 support with the switch “--std=c++0x”.

MSVC 2011 and older lack (at least) variadic templates and will not work with nifpp.

## get() and make()

Nifpp provides overloaded wrappers for most of the `enif_get_XXX()` and `enif_make_XXX()` functions.  The prototypes are:

	bool get(ErlNifEnv *env, ERL_NIF_TERM term, T &var);
	ERL_NIF_TERM make(ErlNifEnv *env, const T &var);
	
`get()` will return true on success, false on failure.

There are some additional template wrappers for `get()`:

	void get_throws(ErlNifEnv *env, ERL_NIF_TERM term, T &var);
	T get(ErlNifEnv *env, ERL_NIF_TERM term);
		
Both forms will throw `nifpp::badarg` upon failure.  Note that for the last form, the type must be explicitly specified since it cannot be inferred, for example:

	int i = get<int>(env, term);

### Plain-Old-Data Types

The following POD types are supported:

- `double`
- `int`
- `unsigned int`
- `long`
- `unsigned long`
- `ErlNifSInt64`
- `ErlNifUInt64`
- `ERL_NIF_TERM`

Note that `get()`/`make()` for `ERL_NIF_TERM` simply outputs the input term without conversion.  This is useful in conjunction with tuple and list `get()`/`make()` (see below).

Examples:

	// get() example 1
	long a;
	if(get(env, term, a))
	{ … do something with a...}
	
	// get() example 2
	try {
	  double b;
	  get_throws(env, term, b);
	  … do something with b...
	}
	except(nifpp::badarg) {}
	
	// get() example 3  
	try {
	  auto c = get<ErlNifPid>(env, term);
	  … do something with c...
	}
	except(nifpp::badarg) {}
	
	// make() example 1
	ERL_NIF_TERM output;
	output = make(env, a);
	return output;
	
	// make() example 2
	auto output = make(env, b);
	return output;
	
	// make() example 3
	return make(env, c);
	
  
### Strings

String are represented by `std::string`.  Examples:

	// get() example:
	std::string a;
	get_throws(env, term, a);
	… do something with a...
	
	// make() example 1:
	ERL_NIF_TERM term = make(env, “hello world”);
	
	// make() example 2:
	std::string a(“hello world”);
	ERL_NIF_TERM term = make(env, a);

### Atoms

nifpp represents atoms by the type `str_atom`, which is a thin wrapper around `std::string`.  Other more complex and more efficient representations may be added in the future.


	// get() example:
	str_atom a;
	get_throws(env, term, a);
	… do something with a...
	
	// make() example 1:
	ERL_NIF_TERM term = make(env, str_atom(“hello world”));
	
	// make() example 2:
	str_atom a(“hello world”);
	ERL_NIF_TERM term = make(env, a);



### Tuples

Tuples are represented by the C++11 type `std::tuple`.  Tuples-of-references are a powerful method for cracking and packing Erlang tuple terms.  Examples:

	// crack simple tuple {hello, 14} using tuple-of-references
	str_atom a;
	int b;
	auto tup = std::make_tuple( std::ref(a), std::ref(b) );
	get_throws(env, term, tup);
	
	// crack nested tuple {hello, 14, {10,4}} using tuple-of-references
	str_atom a;
	int b;
	int c;
	int d;
	auto tup = std::make_tuple( std::ref(a), std::ref(b),
	     std::make_tuple( std::ref(c), std::ref(d) ));
	get_throws(env, term, tup);
	

`std::tie()` offers syntactic sugar for composing a tuple-of-references.  Note that the result of `std::tie()` is not a reference, so it cannot be used in the top-level tuple in the nested tuple example.  Examples:

	// crack simple tuple {hello, 14} using tuple-of-references [using std::tie()]
	str_atom a;
	int b;
	auto tup = std::tie( a, b );
	get_throws(env, term, tup);
	
	// crack nested tuple {hello, 14, {10,4}} using tuple-of-references [using std::tie()]
	str_atom a;
	int b;
	int c;
	int d;
	auto tup = std::make_tuple( std::ref(a), std::ref(b), std::tie( c, d) );
	get_throws(env, term, tup);


	`ERL_NIF_TERM` can be used to defer decoding of tuple elements.  Example:
	
	// partial crack
	str_atom type;
	ERL_NIF_TERM value;
	auto tup = std::tie( type, value );
	get_throws(env, term, tup);
	… decode value based on type


If you want to use `std::tuple` for the C++ side of your code, you can use regular value tuples too.  Example:

	// crack plain tuple
	std::tuple<str_atom, int, ERL_NIF_TERM> tup;
	get_throws(env, term, tup);

And here are some examples of tuple packing...

	ERL_NIF_TERM term;
	str_atom a(“hello”);
	int b(4);
	term = make(env, std::make_tuple(a,b));
	
	ERL_NIF_TERM term;
	auto tup = std::make_tuple(str_atom(“hello”), 4);
	term = make(env, tup);

	ERL_NIF_TERM term = make(env, std::make_tuple(str_atom(“hello”), 4));
	
	



### Lists

Lists can be represented by a number of types:

- `std::vector`
- `std::list`
- `std::deque`
- `std::set`
- `std::multiset`

`get()` and `make()` may be used on all of them to decode/create an Erlang list.  Examples:

	std::vector<int> myintlist;
	get_throws(env, term, myintlist);
	
	std::vector<ERL_NIF_TERM> mylist;
	get_throws(env, term, mylist);
	
	std::deque<int> mydeque;
	mydeque.push_back(44);
	...
	ERL_NIF_TERM term = make(env, mydeque);


There is also a special Erlang list iteration function:

	bool list_foreach(ErlNifEnv *env, ERL_NIF_TERM list_term, std::function<void (ErlNifEnv *, ERL_NIF_TERM item)>);

This is useful if you want to iterate through a list without copying the entire thing.  The following `get()` function is a good example of `list_foreach` usage:

[list for each example]

### Resources

The following forms are for working with resources:

	bool get(ErlNifEnv *env, ERL_NIF_TERM term, T * &var);
	bool get(ErlNifEnv *env, ERL_NIF_TERM term, resource_ptr<T> &var);
	ERL_NIF_TERM make(ErlNifEnv *env, const resource_ptr<T> &var)
	
See section below for details on nifpp resources.

### Resource Binaries

TBA

### Extending get()/make()

Under Construction (there issues with function definition order that need to be worked out)

You can define `get()` and `make()` for any additional type that you wish in your own code.  That type may then be used in other composite types like `std::tuple` and `std::vector`.  Wrappers `void get_throws()` and `void get()` will automatically wrap it.  To reiterate, the following functions should be implemented for your type:

	bool get(ErlNifEnv *env, ERL_NIF_TERM term, T &var);
	ERL_NIF_TERM make(ErlNifEnv *env, const T &var);
	
## Resources

Nifpp allows any C++ type to be used as a resource.  All resource types must be registered using:

	template<typename T>
	int register_resource(
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
	   register_resource<std::string>(env, nullptr, "std::string");
	   register_resource<int>(env, nullptr, "int");
	   register_resource<MyClass>(env, nullptr, "MyClass");
	   return 0;
	}


Objects are created with:

	resource_ptr<T> construct_resource<T>(Args...);

Where `Args` is a set of parameters accepted by any of `T`’s constructors.  Internally this function calls `enif_allocate_resource()` and then performs a placement new on the allocated memory.  Exceptions in the constructor are properly handled; the memory will immediately be released and the C++ destructor will not be invoked.

`resource_ptr` is a reference counting pointer similar to `std::shared_ptr`.  Creating copies of a valid `resource_ptr<>` will invoke `enif_keep_resource()`, and destroying instances will invoke `enif_release_resource()`.  `resource_ptr<>` may be safely kept around after the NIF function call returns.

Resource terms are created with `make()`:

	ERL_NIF_TERM make(ErlNifEnv *env, const resource_ptr<T> &var);

The object pointer can be retrieved from a resource term with

	bool get(ErlNifEnv *env, ERL_NIF_TERM term, T * &var);
	bool get(ErlNifEnv *env, ERL_NIF_TERM term, resource_ptr<T> &var);
	
The `T*` version does not affect reference counts, but should not be kept around after the NIF call returns.  The `resource_ptr<T>` version does increase reference count as described above, but does not have the same restrictions.

Examples of resource construction:

	resource_ptr<int> ptr = construct_resource<int>();  //default ctor
	resource_ptr<int> ptr = construct_resource<int>(123);
	auto ptr = construct_resource<int>(123);  //save typing with c++11 type inference
	auto ptr = construct_resource<std::string>(“cupcakes”);
	auto ptr = construct_resource<vector<std::string>>(5000, “many cupcakes”);
	auto ptr = construct_resource<MyClass>(p1, p2, p3, p4);
	auto ptr = construct_resource<std::shared_ptr<MyClass>>(new MyClass(p1, p2, p3, p4));
	
Any of the above resources can be made into a term with:

	ERL_NIF_TERM term = make(env, ptr);

Also, any of the pointers may be kept around after the NIF call returns.  Be sure to use C++11 copy constructor semantics to prevent superfluous `keep()`/`release(`) calls:

	auto ptr = construct_resource<std::string>(“cupcakes”);
	bakery.front_window = std::move(ptr);
	

Examples of getting objects from resource terms:


	// these are for use within this nif call only.
	int *ptr;
	std::string *ptr;
	vector<std::string>> *ptr;
	MyClass *ptr;
	std::shared_ptr<MyClass> *ptr;
	
	// these may be stored in between nif calls.
	resource_ptr<int> ptr;
	resource_ptr<std::string> ptr;
	resource_ptr<vector<std::string>>> ptr;
	resource_ptr<MyClass> ptr;
	resource_ptr<std::shared_ptr<MyClass>> ptr;
	
	// all pointer types retrieved with...
	get(env, term, ptr);



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


## Performance

The example programs `tuple_twiddle_cpp.cpp` and `tuple_twiddle_c.c` where benchmarked from `nifpptest.erl`.  When compiled under gcc-4.6.2 with the `-O2` option, the median results were...

- `tuple_twiddle_cpp`: 1803 uSec
- `tuple_twiddle_c`: 1887 uSec

The nifpp version was marginally faster on most runs of the benchmark.

Without the `-O2` switch the results are...

- `tuple_twiddle_cpp`: 7902 uSec
- `tuple_twiddle_c`: 1912 uSec

This disparity is typical of template-heavy C++.  The moral here is to always use `-O2` with nifpp.