open OUnit;;
open PlsqlParser.Ast;;
open Utils;;
open AbsintCompiler;;
open Absint.Types;;
open PlsqlParser;;

let test_simple_program () = ();;
  (* let program = "DECLARE N NUMBER; BEGIN NULL; END;" in *)
  (* let expected_ir = [AddFrame; *)
  (*                    Declare("N", "NUMBER"); *)
  (*                   ] in *)
  (* let parse_errors, ast = parse2 program in *)
  (*   assert_equal [] parse_errors; *)
  (*   let ir = compile_for_absint ast in *)
  (*     assert_equal expected_ir ir;; *)

let suite = "Absint tests" >::: [
  "test_simple_program" >:: test_simple_program;
];;
