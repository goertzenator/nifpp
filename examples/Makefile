

NIF_INCLUDE:=-I../ -I/usr/lib/erlang/usr/include

all: nifpptest.so tuple_twiddle_cpp.so tuple_twiddle_c.so nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam 

nifpptest.so: nifpptest.cpp ../nifpp.h
	g++ -O2 -std=c++0x $(NIF_INCLUDE) -o $@ -fpic -shared $<

mmap_binary.so: mmap_binary.cpp ../nifpp.h
	g++ -O2 -std=c++0x $(NIF_INCLUDE) -o $@ -fpic -shared -lboost_iostreams-mt $<

tuple_twiddle_c.so: tuple_twiddle_c.c ../nifpp.h
	gcc -O2 $(NIF_INCLUDE) -o $@ -fpic -shared $<
	
tuple_twiddle_cpp.so: tuple_twiddle_cpp.cpp ../nifpp.h
	g++ -O2 -std=c++0x $(NIF_INCLUDE) -o $@ -fpic -shared $<
	
%.beam: %.erl
	erlc $<

test: all
	erl -s nifpptest
	
clean:
	rm -f *.so *.beam
	