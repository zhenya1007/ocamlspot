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

Installation and Usage
============================

To compile OCamlSpotter, just type ``make`` and ``make install``.
 
If you are Emacs user, see ``ocamlspot.el``. It explains how to set up
and use it:

* Set ``load-path``.
* Set your ``ocamlspot`` binary in your emacs setting (``M-x customize-group => ocamlspot``)
* Add caml or tuareg mode hook.
* See the buffer ``*ocamlspot-process*`` if you thing something goes wrong.

If you have done properly, move your cursor to OCaml identifier and
press ``C-c ;``. Emacs should display the definition of the identifier.
Otherwise... you made mistake or a bug of OCamlSpotter. Yes, it has bugs.

I have also written Vim script ``ocamlspot.vim``, but it is not tested at all.
Sorry but I do not use Vim.


If something goes wrong...
--------------------------------

To browse modules correctly, 

* Use the correct ``ocamlspot`` matching with your OCaml compiler version.
* Compile OCaml modules with ``-bin-annot`` ocaml compiler option.
* Keep the source code and produced cmt/cmti files.
* Install cmt/cmti files along with cmi/cma/cmxa files.

Otherwise OCamlSpotter complains that it cannot find required cmt/cmti files.

Browsing libraries, packages and OCaml stdlibs 
=======================================================

Libraries and packages
-----------------------------

Normally OCaml libraries and packages are not always compiled with -bin-annot option
and do not always install the annotation files.
Therefore, if you want to use OCamlSpotter with installed libraries and packages,
you must rebuild them with -bin-annot compiler option.
This requires little modifications to their build script (Makefile/OMakefile/...).
Basically, you need:

* Add -bin-annot to the compiler switch. For example OCAMLCFLAGS += -bin-annot
* Copy cmt and cmti files at installation. For example::

     install::
        cp \*.mli \*.cmi \*.cma \*.cmt \*.cmti \*.cmxa $(INSTALLDIR)

* Do not remove the original source files, otherwise browsing cannot work.

Browsing OCaml stdlib and otherlibs
------------------------------------------

If you want to browse OCaml's standard library (stdlib and otherlibs), 
you must recompile those modules with -bin-annot option to create cmt/cmti files.

Automation
------------------------------------

To facilitate these you may want to use SpotInstall( https://bitbucket.org/camlspotter/spotinstall ). SpotInstall provides:

* A small OCaml compiler patch to automatically enable -bin-annot by the existence of OCAML_ANNOT environment variable; no need to fix build scripts.
* An automatic cmt/cmti post installation command, spotinstall.
