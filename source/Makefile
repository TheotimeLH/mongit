path_bin_file=/home/theotime/Documents/Projets/bin

all: mk_init mk_mg cmd

mk_init: clean_init init

init:
	dune build init.exe
	cp _build/default/init.exe init
	./init

clean_init:
	dune clean
	rm -f init

mk_mg: clean_mg mg

mg:
	dune build main.exe
	cp _build/default/main.exe mg

clean_mg:
	dune clean
	rm -f mg

cmd: mk_mg
	cp -f mg $(path_bin_file)/mg

.PHONY:
	mk_mg mk_init
