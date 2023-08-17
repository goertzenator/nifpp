ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
NIF_INCLUDE:=-I../ -I$(ERLANG_PATH)

CFLAGS = -g -O3 -ansi -pedantic -Wall -Wextra $(NIF_INCLUDE)

ifneq ($(OS),Windows_NT)
	CFLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	else
		LDFLAGS += -shared
	endif
endif

all: nifpptest.so tuple_twiddle_cpp.so tuple_twiddle_c.so nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam

CPPFLAGS = -std=c++17

nifpptest.o nifpptest_2tu.o: %.o: %.cpp ../nifpp.h
	$(CXX) $(CFLAGS) $(LDFLAGS) $(CPPFLAGS) -c $<

nifpptest.so: nifpptest.o nifpptest_2tu.o
	$(CXX) $(CFLAGS) $(LDFLAGS) $(CPPFLAGS) -o $@ $^

mmap_binary.so: mmap_binary.cpp ../nifpp.h
	$(CXX) $(CFLAGS) $(LDFLAGS) $(CPPFLAGS) -o $@ -lboost_iostreams-mt $<

tuple_twiddle_c.so: tuple_twiddle_c.c ../nifpp.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

tuple_twiddle_cpp.so: tuple_twiddle_cpp.cpp ../nifpp.h
	$(CXX) $(CFLAGS) $(LDFLAGS) $(CPPFLAGS) -o $@ $<

%.beam: %.erl
	erlc $<

test: all
	erl -noshell -noinput -s nifpptest

clean:
	rm -f *.so *.beam *.o
	rm -rf *.so.dSYM

