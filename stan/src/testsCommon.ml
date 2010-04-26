
open OUnit;;
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;
open Ast;;

let parse_helper parse_function str expected_warnings expected =
  let tokens, _ = tokenize str 0 in
  let warnings, result_option = parse_function tokens in
    (match result_option with
       | Some (_, ast) -> assert_equal expected ast
       | None -> assert_failure "Parse failed.");
    assert_equal expected_warnings (List.rev warnings);;

