
(* This file contains the PLSQL to abstract interpreter intermediate
   representation. *)

open Utils;;
open Absint.Types;;
open PlsqlParser.Ast;;
open Acm;;

let rec compile ast =
  match ast with
    | Program(stmts), _ ->
        compile_stmt_list stmts
    | Block(declarations, statements), _ ->
        add_ir AddFrame
        <+> compile_stmt_list declarations
        <+> compile_stmt_list statements
        <+> add_ir DeleteFrame
    | _ ->
        failwith "Unknown ast type."

and compile_stmt_list (stmts: plsql_ast_with_pos list) : (ir list, unit) compiler_monad =
  match stmts with
    | hd :: tl ->
        compile hd
        <+> compile_stmt_list tl
    | [] ->
        result ();;

let compile_for_absint ast =
  let irs, _ = run_compiler (compile ast) [] in
    List.rev irs;;
