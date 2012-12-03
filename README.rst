==========================================
OCamlSpotter - OCaml source browsing
==========================================

OCamlSpotter is a tool for OCaml source code browsing. 

* You can search the definitions of names of values, functions, data types and modules.
* Emacs and Vim helpers help your browsing via editors.
* Definition search traverses module aliases and functor applications: if module M = N, OCamlSpotter automatically seeks the definition of M.x in N. Very helpful in the modern OCaml programming with lots of modules.

OCamlSpotter 2.x uses \*.cmt and \*.cmti files created by OCaml compiler 4.00.0 or newer with -bin-annot option.

Unlike OCamlSpotter 1.x, OCamlSpotter 2.x is a standalone application. You NO LONGER need compiler patching. Just make, make install, and configure ocamlspot.el.

Dependency
=====================

OCamlSpotter strongly depends on OCaml compiler implementation and its compiler-libs library.
You need use the correct pairs of compiler and OCamlSpotter.

https://bitbucket.org/camlspotter/ocamlspot provides OCamlSpotter branches for each OCaml versions:

* ocaml-<version-name> : compilable against the given OCaml version
    * ocaml-4.00.0 : the latest "stable" version
    * ocaml-4.00.1 : the latest "stable" version
* default : Development version. Sometimes not compilable. Not for you.

Versions
================

OCamlSpotter is always under development and there is no clear release versions.
If you want to use the latest stable version of OCamlSpotter, choose the tip of the branch 
with the name of your OCaml compiler version. 
When you report bugs, please note the revision hash with your issue description please.

To work with OCamlSpotter
==========================

To browse modules correctly, 

* Compile them with -bin-annot ocaml compiler option.
* Keep the source code and produced cmt/cmti files.
* Install cmt/cmti files along with cmi/cma/cmxa files.

Otherwise OCamlSpotter complains that it cannot find required cmt/cmti files.

How to do it?
---------------------------

Normally this requires little modifications to the build script (Makefile/OMakefile/...) of each library.
Basically, you need:

* Add -bin-annot to the compiler switch. For example OCAMLCFLAGS += -bin-annot
* Copy cmt and cmti files at installation. For example::

     install::
        cp \*.mli \*.cmi \*.cma \*.cmt \*.cmti \*.cmxa $(INSTALLDIR)

This means that you need to recompile all the OCaml libraries you use with -bin-annot.

If you want to browse OCaml's standard library (stdlib and otherlibs), 
you must also recompile those modules with -bin-annot option to create cmt/cmti files.

Some automation
--------------------------

To facilitate these you may want to use SpotInstall( https://bitbucket.org/camlspotter/spotinstall ). SpotInstall provides:

* A small OCaml compiler patch to automatically enable -bin-annot by the existence of OCAML_ANNOT environment variable; no need to fix build scripts.
* An automatic cmt/cmti post installation command, spotinstall.
