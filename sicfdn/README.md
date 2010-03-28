
# Studies in Compilation For .NET

## Introduction

This is a set of notes and code that I will use to learn more about the
common intermediate language (CIL) used in the .NET platform.

I will try to implement a subset of JavaScript,

 * first as a compiler to a plain CIL file (to be assembled with `ilasm.exe`)
 * then I will try to generate the code using `System.Reflection.Emit`
 * and finally I will try to use DLR.

## Plan

 * DONE write a snippets extractor to help including the code in the
   text in a reliable way
 * DONE decide on the subset of JavaScript to implement
 * DONE extract a grammar for the subset
 * DONE write a lexer
 * DONE write a parser
 * DONE write an interpreter to test the grammar/parser/AST API
 * TODO write a code generator that will output a file usable by
   `ilasm.exe`

