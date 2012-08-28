==========================================
OCamlSpotter - OCaml source browsing
==========================================

OCamlSpotter is a tool for OCaml source code browsing. You can search the type and definitions of values, expressions and modules.

OCamlSpotter 2.x uses \*.cmt and \*.cmti files created by OCaml compiler 4.00.0 or newer with -bin-annot option.

Unlike OCamlSpotter 1.x, OCamlSpotter 2.x is a standalone application. You NO LONGER need compiler patching. Just make, make install, and configure ocamlspot.el.

Dependency
=====================

OCamlSpotter strongly depends on OCaml compiler implementation and its compiler-libs library.
You need use the correct pairs of compiler and OCamlSpotter.

https://bitbucket.org/camlspotter/ocamlspot provides OCamlSpotter branches for each OCaml versions:

* ocaml-<version-name> : compilable against the given OCaml version
    * ocaml-4.00.0 : the latest "stable" version
    * ocaml-4.00.0-rc1 : no longer maintained
    * ocaml-4.00.0-beta2 : no longer maintained
* default : Development version. Probably not for you.

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

* Add -bin-annot to the compiler switch (for example OCAMLCFLAGS += -bin-annot)
* Copy cmt and cmti files at installation. For example::

     install::
        cp \*.mli \*.cmi \*.cma \*.cmt \*.cmti \*.cmxa $(INSTALLDIR)


Some automation
--------------------------

To facilitate this you may want to use SpotInstall( https://bitbucket.org/camlspotter/spotinstall ). SpotInstall provides:

* No need to fix build scripts, since the compiler can produce annot/cmt/cmti files by default, if OCAML_ANNOT env var is non empty
* Automatic cmt/cmti post installation
