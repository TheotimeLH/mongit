mk_mongit: clean mongit

mongit:
	dune build main.exe
	cp _build/default/main.exe mongit

clean:
	dune clean
	rm -f mongit

cmd: mk_mongit
	cp -f mongit $$HOME/Documents/Projets/bin/mg

.PHONY:
	mk_mongit
