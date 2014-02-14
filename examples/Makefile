

NIF_INCLUDE:=-I../ -I/usr/lib/erlang/usr/include

all: nifpptest.so tuple_twiddle_cpp.so tuple_twiddle_c.so nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam 

nifpptest.o nifpptest_2tu.o: %.o: %.cpp ../nifpp.h
	g++ -O3 -g -std=c++11 $(NIF_INCLUDE) -fpic -c $<

nifpptest.so: nifpptest.o nifpptest_2tu.o
	g++ -O3 -g -o $@ -fpic -shared $^

mmap_binary.so: mmap_binary.cpp ../nifpp.h
	g++ -O3 -std=c++11 $(NIF_INCLUDE) -o $@ -fpic -shared -lboost_iostreams-mt $<

tuple_twiddle_c.so: tuple_twiddle_c.c ../nifpp.h
	gcc -O3 $(NIF_INCLUDE) -o $@ -fpic -shared $<
	
tuple_twiddle_cpp.so: tuple_twiddle_cpp.cpp ../nifpp.h
	g++ -O3 -std=c++11 $(NIF_INCLUDE) -o $@ -fpic -shared $<
	
%.beam: %.erl
	erlc $<

test: all
	erl -s nifpptest
	
clean:
	rm -f *.so *.beam
	
