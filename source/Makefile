path_bin_file=/tmp

mk_init: clean_init init

init:
	dune build init.exe
	cp _build/default/init.exe init
	./init
	make mk_mg

clean_init:
	dune clean
	rm -f init

mk_mg: clean_mg mg cmd

mg:
	dune build main.exe
	cp _build/default/main.exe mg

clean_mg:
	dune clean
	rm -f mg

cmd:
	cp -f mg $(path_bin_file)/mg

clean: clean_init clean_mg

clean_all: clean call_clean

call_clean:
	dune build clean.exe
	cp _build/default/clean.exe clean
	./clean
	dune clean
	rm -f clean

.PHONY:
	mk_mg mk_init
