
open OUnit

open Utils
open Sexp
open Parser
open Eval

let test_eval_nil () =
  let sexp, pos = parse "nil" 0 in
  let result = eval sexp in
    assert_equal Nil result

let test_eval_string () =
  let sexp, pos = parse "\"foo\"" 0 in
  let result = eval sexp in
    assert_equal (String "foo") result

let test_eval_number () =
  let sexp, pos = parse "10" 0 in
  let result = eval sexp in
    assert_equal (Number 10) result

let test_eval_builtin_plus () =
  let sexp, pos = parse "(+ 1 1 2)" 0 in
  let result = eval sexp in
    assert_equal (Number 4) result

let test_eval_builtin_times () =
  let sexp, pos = parse "(* 2 2)" 0 in
  let result = eval sexp in
    assert_equal (Number 4) result

let test_eval_builtin_reverse () =
  let expected, _ = parse "(3 2 1)" 0 in
  let sexp, _ = parse "(reverse '(1 2 3))" 0 in
  let result = eval sexp in
    assert_equal expected result

let test_eval_arity_check () =
  let evalFunc () =
    let sexp, _ = parse "(reverse '(4 5 6) 1)" 0 in
      eval sexp |> ignore
  in
  let message = "Needed exactly 1 arguments, but got 2." in
    assert_raises (Failure message) evalFunc

let test_eval_lambda_creation () =
  let sexp, _ = parse "((fn (x) (+ x x)) 1)" 0 in
  let result = eval sexp in
    assert_equal (Number 2) result

let test_eval_definition_inside_lambda () =
  let sexp, _ = parse "((fn (x) (def k 2) (+ x k)) 2)" 0 in
  let result = eval sexp in
    assert_equal (Number 4) result

let suite = "Eval tests" >::: [ "test_eval_nil" >:: test_eval_nil;
                                "test_eval_number" >:: test_eval_number;
                                "test_eval_builtin_plus" >:: test_eval_builtin_plus;
                                "test_eval_builtin_times" >:: test_eval_builtin_times;
                                "test_eval_builtin_reverse" >:: test_eval_builtin_reverse;
                                "test_eval_arity_check" >:: test_eval_arity_check;
                                "test_eval_lambda_creation" >:: test_eval_lambda_creation;
                                "test_eval_definition_inside_lambda" >::
                                  test_eval_definition_inside_lambda;
                                "test_eval_string" >:: test_eval_string]

