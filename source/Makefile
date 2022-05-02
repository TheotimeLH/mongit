mk_mongit: clean mongit

mongit:
	dune build main.exe
	cp _build/default/main.exe mongit

clean:
	dune clean
	rm -f mongit

.PHONY:
	mk_mongit
