comptest: comptest.ml
	ocamlc -o comptest -I .. -dllpath .. mathlib.cma comptest.ml 

comptest.opt: comptest.ml
	ocamlopt -o comptest.opt -I .. mathlib.cmxa comptest.ml

clean:
	rm -f comptest comptest.opt *.cm? *.o