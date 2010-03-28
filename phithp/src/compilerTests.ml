
open OUnit

open Utils
open Sexp
open Parser
open Eval
open Compiler

let test_compile_number () =
  let options = { max_column = 100 } in
  let ast = compile_to_ast options "1" in
  let php_code = generate_code_from_ast options ast in
    assert_equal (Number 1) ast;
    assert_equal "<?php\nprint_r (1);\n?>" php_code

let test_compile_string () =
  let options = { max_column = 100 } in
  let ast = compile_to_ast options "\"1\"" in
  let php_code = generate_code_from_ast options ast in
    assert_equal (String "1") ast;
    assert_equal "<?php\nprint_r (\"1\");\n?>" php_code

let test_compile_symbol () =
  let options = { max_column = 100 } in
  let ast = compile_to_ast options "foo" in
  let php_code = generate_code_from_ast options ast in
    assert_equal (Identifier "foo") ast;
    assert_equal "<?php\nprint_r (foo);\n?>" php_code

let test_compile_simple_expression () =
  let options = { max_column = 100 } in
  let ast = compile_to_ast options "(+ 1 1)" in
  let php_code = generate_code_from_ast options ast in
    assert_equal (BinaryOperator ("+", [Number 1; Number 1])) ast;
    assert_equal "<?php\nprint_r (1 + 1);\n?>" php_code

let test_compile_almost_simple_expression () =
  let options = { max_column = 100 } in
  let ast = compile_to_ast options "(+ 1 1 1)" in
  let php_code = generate_code_from_ast options ast in
  let expected_ast = BinaryOperator ("+", [ BinaryOperator ("+", [ Number 1;
                                                               Number 1 ]);
                                        Number 1 ])
  in
    assert_equal expected_ast ast;
    assert_equal "<?php\nprint_r (1 + 1 + 1);\n?>" php_code

let test_compile_almost_simple_expression_with_small_column () =
  let options = { max_column = 10 } in
  let ast = compile_to_ast options "(+ 1 1 1)" in
  let php_code = generate_code_from_ast options ast in
  let expected_ast = BinaryOperator ("+", [ BinaryOperator ("+", [ Number 1;
                                                               Number 1 ]);
                                        Number 1 ])
  in
    assert_equal expected_ast ast;
    assert_equal "<?php\nprint_r (1\n         + 1\n         + 1);\n?>" php_code

let test_compile_with_precedence () =
  let options = { max_column = 100 } in
  let ast = compile_to_ast options "(* (+ 2 3) 4)" in
  let php_code = generate_code_from_ast options ast in
  let expected_ast = BinaryOperator ("*", [ BinaryOperator ("+", [ Number 2;
                                                                   Number 3 ]);
                                            Number 4 ]) in
    assert_equal expected_ast ast;
    assert_equal "<?php\nprint_r ((2 + 3) * 4);\n?>" php_code

let test_compile_with_precedence_and_small_column () =
  let options = { max_column = 30 } in
  let ast = compile_to_ast options  "(* (+ 2 3) (+ 4 5 6))" in
  let php_code = generate_code_from_ast options ast in
  let expected_ast =
    BinaryOperator ("*", [ BinaryOperator ("+", [ Number 2;
                                                  Number 3 ]);
                           BinaryOperator ("+", [ BinaryOperator ("+", [ Number 4;
                                                                         Number 5 ]);
                                                  Number 6 ]) ]) 
  in
    assert_equal expected_ast ast;
    assert_equal "<?php\nprint_r ((2 + 3)\n         * (4 + 5 + 6));\n?>" php_code

let suite = "Compiler tests" >::: [ "test_compile_number" >:: test_compile_number;
                                    "test_compile_symbol" >:: test_compile_symbol;
                                    "test_compile_simple_expression" >::
                                      test_compile_simple_expression;
                                    "test_compile_almost_simple_expression" >::
                                      test_compile_almost_simple_expression;
                                    "test_compile_almost_simple_expression_with_small_column" >::
                                      test_compile_almost_simple_expression_with_small_column;
                                    "test_compile_with_precedence" >:: test_compile_with_precedence;
                                    "test_compile_with_precedence_and_small_column" >::
                                      test_compile_with_precedence_and_small_column;
                                    "test_compile_string" >:: test_compile_string ]

