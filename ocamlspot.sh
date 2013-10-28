#!/bin/sh

# This is a sample shell script which tries to call the corresponding OCamlSpotter
# with the current OPAM switch.

DIR=`opam config var bin`

if [ -x $DIR/ocamlspot.opt ]; then 
  $DIR/ocamlspot.opt $*
else 
  if [ -x $DIR/ocamlspot ]; then 
    $DIR/ocamlspot $*
  else 
    echo "ERROR: No ocamlspot.opt or ocamlspot found at $DIR"
  fi
fi
