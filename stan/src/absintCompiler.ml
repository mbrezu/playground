
(* This file contains the PLSQL to abstract interpreter intermediate
   representation. *)

open Utils;;
open Absint.Ir;;
open PlsqlParser.Ast;;
open Acm;;
open Printf;;

module Gensym : sig
  val gensym: string -> string;;
  val reset_gensym: unit -> unit;;
end = struct
  let gensym_counter = ref 0;;

  let gensym prefix =
    gensym_counter := !gensym_counter + 1;
    sprintf "%s_%d" prefix !gensym_counter;;

  let reset_gensym () =
    gensym_counter := 0;;

end;;

open Gensym;;

let rec compile ast =
  match ast with
    | Program(stmts), _ ->
        compile_stmt_list stmts
    | Block(declarations, statements), _ ->
        add_ir AddFrame
        <+> compile_stmt_list declarations
        <+> compile_stmt_list statements
        <+> add_ir DeleteFrame
    | StmtNull, _ ->
        result ()
    | VarDecl(var_name, var_type), _ ->
        add_ir <| Declare(var_name, var_type)
    | StmtAssignment(target, expr), _ ->
        add_ir <| Assignment(target, expr)
    | StmtCall(subprogram, arguments), _ ->
        add_ir <| Absint.Ir.Call(subprogram, arguments)
    | StmtIf(cond, then_clause, else_clause), _ ->
        compile_if cond then_clause else_clause
    | _ ->
        failwith "Unknown ast type."

and compile_if cond then_clause else_clause =
  let then_label = gensym "Then" in
  let else_label = gensym "Else" in
  let after_if_label = gensym "AfterIf" in
    (add_ir <| GotoIf(cond, then_label, else_label))
    <+> (add_ir <| Label then_label)
    <+> (compile_stmt_list then_clause)
    <+> (add_ir <| Goto(after_if_label, None))
    <+> (add_ir <| Label else_label)
    <+> (match else_clause with
           | NoElse ->
               result ()
           | Else(stmts) ->
               compile_stmt_list stmts
           | ElsIf(cond, then_clause, else_clause) ->
               compile_if cond then_clause else_clause)
    <+> (add_ir <| Label after_if_label)

and compile_stmt_list stmts =
  match stmts with
    | hd :: tl ->
        compile hd
        <+> compile_stmt_list tl
    | [] ->
        result ();;

let compile_for_absint ast =
  reset_gensym ();
  let (_, irs, _), _ = run_compiler (compile ast) Acm.empty_state in
    List.rev irs;;

let compile_helper str =
  let _, result = PlsqlParser.parse2 str in
    match result with
      | Some (_, ast) ->
          compile_for_absint ast
      | None ->
          failwith "Parse failed.";;

