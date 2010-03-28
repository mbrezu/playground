
open Utils
open Sexp
open Formatter
open Parser
open OUnit

let test_no_break () =
  assert_equal "123" <| format 100 (NoBreak [Str "1"; Str "2"; Str "3"])

let test_always_break () =
  assert_equal "1\n2\n3" <| format 100 (AlwaysBreak [Str "1"; Str "2"; Str "3"])

let test_always_break_with_indent () =
  let input = (NoBreak [Str "1"; (AlwaysBreak [Str "2"; Str "3"]); Str "4"]) in
  let actual = format 100 input in
    assert_equal "12\n 34" actual

let test_maybe_break () =
  let input = (NoBreak [Str "1"; (MaybeBreak [Str "2"; Str "3"]); Str "4"]) in
  let actual = format 100 input in
    assert_equal "1234" actual

let test_maybe_break_small_col () =
  let input = (NoBreak [Str "1"; (MaybeBreak [Str "2"; Str "3"]); Str "4"]) in
  let actual = format 3 input in
    assert_equal "12\n 34" actual

let test_maybe_break_validation () =
  let input = (MaybeBreak [Str "1"; (AlwaysBreak [Str "2"; Str "3"]); Str "4"]) in
  let message = "Cannot nest `AlwaysBreak` inside `MaybeBreak`." in
  let evalFunc () = format 3 input |> ignore in
    assert_raises (Failure message) evalFunc

let test_decorate_string () =
  let actual = String "test" |> decorate_for_format |> format 100 in
    assert_equal "\"test\"" actual

let test_decorate_symbol () =
  let actual = Symbol "test" |> decorate_for_format |> format 100 in
    assert_equal "test" actual

let test_decorate_special_symbol () =
  let actual_nil = Nil |> decorate_for_format |> format 100 in
  let actual_t = T |> decorate_for_format |> format 100 in
    assert_equal "nil" actual_nil;
    assert_equal "t" actual_t

let test_decorate_number () =
  let actual = Number 123 |> decorate_for_format |> format 100 in
    assert_equal "123" actual

let test_decorate_list () =
  let result, _ = parse "(1 2 3)" 0 in
  let actual = result |> decorate_for_format |> format 100 in
    assert_equal "(1 2 3)" actual

let test_decorate_list_small_col () =
  let result, _ = parse "(1 2 3)" 0 in
  let actual = result |> decorate_for_format |> format 4 in
    assert_equal "(1\n 2\n 3)" actual

let test_decorate_complex_list_small_col () =
  let result, _ = parse "((1) (1 2) (1 2 3))" 0 in
  let actual = result |> decorate_for_format |> format 10 in
    assert_equal  "((1)\n (1 2)\n (1 2 3))" actual

let suite = "Formatter tests" >::: [ "test_no_break" >:: test_no_break;
                                     "test_always_break_with_indent" >::
                                       test_always_break_with_indent;
                                     "test_maybe_break" >:: test_maybe_break;
                                     "test_maybe_break_small_col" >:: test_maybe_break_small_col;
                                     "test_maybe_break_validation" >:: test_maybe_break_validation;
                                     "test_decorate_string" >:: test_decorate_string;
                                     "test_decorate_symbol" >:: test_decorate_symbol;
                                     "test_decorate_number" >:: test_decorate_number;
                                     "test_decorate_list" >:: test_decorate_list;
                                     "test_decorate_list_small_col" >::
                                       test_decorate_list_small_col;
                                     "test_decorate_complex_list_small_col" >::
                                       test_decorate_complex_list_small_col;
                                     "test_decorate_special_symbol" >::
                                       test_decorate_special_symbol;
                                     "test_always_break" >:: test_always_break ]
