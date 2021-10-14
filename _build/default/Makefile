.PHONY: test check
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
clean:
	dune clean
	rm -f adventure.zip
build:
	dune build