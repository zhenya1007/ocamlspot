==========================================
OCamlSpotter - OCaml source browsing
==========================================

OCamlSpotter is a tool for OCaml source code browsing. You can search the type and definitions of values, expressions and modules.

OCamlSpotter 2.0.0 uses \*.cmt and \*.cmti files created by OCaml compiler 4.00.0 or newer with -bin-annot option.

Unlike OCamlSpotter 1.x, OCamlSpotter 2.0.0 is a standalone application. You NO LONGER need compiler patching. Just make, make install, and configure ocamlspot.el.

Dependency
=====================

OCamlSpotter strongly depends on OCaml compiler implementation and its compiler-libs library.
You need use the correct pairs of compiler and OCamlSpotter.

https://camlspotter@bitbucket.org/camlspotter/ocamlspot provides OCamlSpotter branches 
for each OCaml versions:

* ocaml-version-name : compilable against the given OCaml version
* default : For the latest OCaml development version or something around. Probably not for you.
* dev : default + something experimental. Absolutely not for you unless you are explicitly instructed to use this.
