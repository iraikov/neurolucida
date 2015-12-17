#!/bin/sh

$HOME/bin/chicken/bin/csc -deploy neurolucida.scm
$HOME/bin/chicken/bin/chicken-install -deploy -p $PWD/neurolucida matchable input-parse sxml-transforms sxpath ssax getopt-long typeclass kd-tree cis datatype iset digraph  dyn-vector vector-lib format-graph

