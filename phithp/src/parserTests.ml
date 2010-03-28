
open Utils
open Sexp
open Parser
open OUnit

let test_parse_nil () =
  let result1, pos1 = parse "()" 0 in
  let result2, pos2 = parse "nil" 0 in
    assert_equal Nil result1;
    assert_equal 2 pos1;
    assert_equal Nil result2;
    assert_equal 3 pos2

let test_parse_symbol () =
  let result, pos = parse "foo" 0 in
    assert_equal (Symbol "foo") result;
    assert_equal 3 pos

let test_parse_string () =
  let result, pos = parse "\"foo\"" 0 in
    assert_equal (String "foo") result;
    assert_equal 5 pos

let test_parse_number () =
  let result, pos = parse "10" 0 in
    assert_equal (Number 10) result;
    assert_equal 2 pos

let test_parse_list () =
  let result, pos = parse "(10)" 0 in
  let expected_sexp = Pair (Number 10, Nil) in
    assert_equal expected_sexp result;
    assert_equal 4 pos

let test_parse_complex_list () =
  let result, pos = parse "(10 ())" 0 in
  let expected_sexp = Pair (Number 10, Pair (Nil, Nil)) in
    assert_equal expected_sexp result;
    assert_equal 7 pos

let test_parse_t () =
  let result, pos = parse "t" 0 in
    assert_equal T result;
    assert_equal 1 pos

let test_parse_quote () =
  let result, pos = parse "'(1 2)" 0 in
  let expected = Pair (Symbol "quote", Pair (Pair (Number 1, Pair (Number 2, Nil)), Nil)) in
    assert_equal expected result;
    assert_equal 6 pos

let suite = "Parser tests" >::: [ "test_parse_nil" >:: test_parse_nil;
                                  "test_parse_t" >:: test_parse_t;
                                  "test_parse_string" >:: test_parse_string;
                                  "test_parse_number" >:: test_parse_number;
                                  "test_parse_list" >:: test_parse_list;
                                  "test_parse_complex_list" >:: test_parse_complex_list;
                                  "test_parse_quote" >:: test_parse_quote;
                                  "test_parse_symbol" >:: test_parse_symbol ]
