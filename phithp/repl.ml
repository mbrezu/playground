
(* 

This file contains ocaml repl commands useful for loading Phithp in
the REPL so I can try things out interactively.

*)

#use "topfind"
#thread
#require "core"
#directory "src"
#load "utils.cmo"
#load "sexp.cmo"
#load "parser.cmo"
#load "formatter.cmo"
#load "eval.cmo"
#load "compiler.cmo"
