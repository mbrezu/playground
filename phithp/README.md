
# Phithp

A Lisp to PHP compiler.

## Rationale

Since PHP support is so wide spread, it would be nice to be able to
deploy web applications using hosting that provides PHP, but use a
better language.

Weirdly, compiling Lisp to PHP has not been attempted yet
publicly. Well, at least not proeminently enough to be easily found by
Google.

So I'm doing this as an exercise to learn F# and the discipline of
TDD. This means this is an educational toy project.

What needs to be done:

 * a Lisp interpreter (because I want to have macros, which will run
   at compile time, therefore I need a way to run the code).
 * a compiler to PHP source (this includes a pretty printer to render
   the PHP code).
 * a convenient way to interface the Phithp with the underlying PHP
   libraries.

## Prerequisites

I'm working on Ubuntu 10.04 alpha, and the commands used to install
prerequisites are

 * Python, but that's already installed almost everywhere;
 * the OCaml compiler (<http://caml.inria.fr/download.en.html> or
   `sudo apt-get install ocaml`);
 * OUnit, the unit testing framework for OCaml -
   (<http://www.xs4all.nl/~mmzeeman/ocaml/>);
 * `ocamlfind` from `apt-get install ocaml-findlib`;
 * the 'Core' library from Jane Street Capital - installable by using
   `sudo apt-get install libcore-extended-ocaml-dev libcore-ocaml-dev`.

## Status

I'm still writing the Lisp interpreter.

I switched from F# to OCaml since I'm working on this project on Linux
and the Mono-F# combo is really, really slow (compiling my ~1000 lines
and running the tests takes about 15 seconds, which doesn't work well
with the quick edit-compile-run cycle of TDD).

Getting better at OCaml should help me with F# too. Hopefully.

## Advice

Don't try to pronounce this project's name :-)
