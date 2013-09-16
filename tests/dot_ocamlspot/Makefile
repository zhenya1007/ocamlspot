test: build/hello.cmo
	../../ocamlspot hello.ml:l5c8
	../../ocamlspot dir/dir2/bye.ml:l2c12

build/dir/dir2/bye.cmo: dir/dir2/bye.ml
	ocamlc -c -bin-annot -I build/dir/dir2 -o build/dir/dir2/bye.cmo $<

build/hello.cmo: hello.ml build/dir/dir2/bye.cmo
	ocamlc -c -bin-annot -I build/dir/dir2 -o $@ $<

clean:
	rm build/hello.* build/dir/dir2/bye.*


