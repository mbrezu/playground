
open OUnit;;
open ParserTypes;;
open PlsqlParser;;
open Lexer;;
open Pwm;;

let test_lex_helper str expected =
  let tokens, _ = tokenize str 0 in
    assert_equal expected tokens;;

let test_lex_begin_end () =
  test_lex_helper
    "BEGIN END;"
    [Token ("BEGIN", Pos (0, 4));
     Token ("END", Pos (6, 8));
     Token (";", Pos (9, 9))];;

let test_lex_simple_select () =
  test_lex_helper
    "SELECT Field1 FROM TestTable;"
    [Token ("SELECT", Pos (0, 5));
     Token ("FIELD1", Pos (7, 12));
     Token ("FROM", Pos (14, 17));
     Token ("TESTTABLE", Pos (19, 27));
     Token (";", Pos (28, 28))];;

let test_pwm_lookahead () =
  let tokens, _ = tokenize "" 0 in
  let result = run_parser_helper lookahead tokens in
    assert_equal ([], Some (Stream (None, []), None)) result;;

let test_pwm_eoi () =
  let result_1 = run_parser eoi (Stream(None, []), []) in
  let tokens, _ = tokenize "BEGIN" 0 in
  let result_2 = run_parser eoi (Stream(None, tokens), []) in
    assert_equal ([], Some (Stream (None, []), true)) result_1;
    assert_equal ([], Some (Stream (None, tokens), false)) result_2;;

let test_pwm_consume_1 () =
  let tokens, _ = tokenize "" 0 in
  let result = run_parser (consume "BEGIN") (Stream(None, tokens), []) in
    match result with
      | [warning], None ->
          assert_equal (Warning("Expected 'BEGIN' but reached end of input.", 0)) warning
      | _ ->
          assert_failure "Expected a warning.";;

let test_pwm_consume_2 () =
  let tokens, _ = tokenize "BEGIN END" 0 in
  let result = run_parser
    (consume "BEGIN" <+> consume "BEGIN")
    (Stream(None, tokens), [])
  in
    match result with
      | [warning], None ->
          assert_equal (Warning ("Expected 'BEGIN' but got 'END'.", 6)) warning
      | _ ->
          assert_failure "Expected a warning.";;

let test_pwm_until_eoi () =
  let tokens, _ = tokenize "BEGIN END BEGIN" 0 in
  let result =
    run_parser (until_eoi (consume "BEGIN" <+> consume "END")) (Stream(None, tokens), [])
  in
    match result with
      | [warning], None ->
          assert_equal (Warning ("Expected 'END' but reached end of input.", 15)) warning
      | _ ->
          assert_failure "Expected a warning.";;

let test_pwm_consume_or_fake () =
  let tokens, _ = tokenize "BEGIN END" 0 in
  let parser = consume "BEGIN" <+> consume "END" <+> consume_or_fake ";" in
  let result = run_parser parser (Stream(None, tokens), []) in
    match result with
      | [warning], _ ->
          assert_equal (Warning ("Expected ';'.", 8)) warning
      | _ ->
          assert_failure "Expected a warning.";;

let parse_helper parse_function str expected_warnings expected =
  let tokens, _ = tokenize str 0 in
  let warnings, result_option = parse_function tokens in
    (match result_option with
       | Some (_, ast) -> assert_equal expected ast
       | None -> assert_failure "Parse failed.");
    assert_equal expected_warnings (List.rev warnings);;

let test_parse_helper = parse_helper parse;;

let test_parse_begin_end () =
  test_parse_helper
    "BEGIN END;"
    []
    (Program([Block([], []), Pos(0, 9)]), Pos(0, 9));;

let test_parse_declare_begin_end () =
  test_parse_helper
    "DECLARE BEGIN END;"
    []
    (Program([Block([], []), Pos(0, 17)]), Pos(0, 17));;

let test_parse_empty_block_with_decl () =
  test_parse_helper
    "DECLARE var INTEGER; BEGIN END;"
    []
    (Program([Block([VarDecl("VAR", "INTEGER"), Pos(8, 19)], []), Pos(0, 30)]),
     Pos(0, 30));;

let test_parse_begin_end_no_semicolon () =
  test_parse_helper
    "BEGIN END"
    [Warning("Expected ';'.", 8)]
    (Program([Block([], []), Pos(0, 8)]), Pos(0, 8));
  test_parse_helper
    "DECLARE BEGIN END"
    [Warning("Expected ';'.", 16)]
    (Program([Block([], []), Pos(0, 16)]), Pos(0, 16));
  test_parse_helper
    "DECLARE var int BEGIN END"
    [Warning("Expected ';'.", 14);
     Warning("Expected ';'.", 24)]
    (Program
       [(Block ([(VarDecl ("VAR", "INT"), Pos (8, 14))], []), Pos (0, 24))],
     Pos (0, 24));;

let test_parse_simple_complete_block_1 () =
  test_parse_helper
    "DECLARE var INTEGER; BEGIN var := 0; END;"
    []
    (Program([(Block ([(VarDecl ("VAR", "INTEGER"), Pos (8, 19))],
                      [(StmtAssignment ("VAR", (NumericLiteral "0", Pos (34, 34))),
                        Pos (27, 35))]),
               Pos (0, 40))]),
     Pos (0, 40));;

let test_parse_simple_complete_block_2 () =
  test_parse_helper
    "DECLARE var INTEGER; BEGIN var := a; END;"
    []
    (Program([(Block ([(VarDecl ("VAR", "INTEGER"), Pos (8, 19))],
                      [(StmtAssignment ("VAR", (Identifier "A", Pos (34, 34))),
                        Pos (27, 35))]),
               Pos (0, 40))]),
     Pos(0, 40));;

let test_parse_expr_helper = parse_helper parse_expr;;

let test_parse_expression_simple_1 () =
  let expected = (BinaryOp("+",
                           (NumericLiteral "1", Pos(0, 0)),
                           (NumericLiteral "2", Pos(4, 4))),
                  Pos(0, 4)) in
    test_parse_expr_helper "1 + 2" [] expected

let test_parse_expression_simple_2 () =
  let mul_tree = (BinaryOp ("*",
                                (NumericLiteral "2", Pos(4, 4)),
                                (NumericLiteral "3", Pos(8, 8))),
                  Pos(4, 8)) in
  let expected = (BinaryOp("+",
                               (NumericLiteral "1", Pos(0, 0)),
                               mul_tree),
                  Pos(0, 8)) in
    test_parse_expr_helper "1 + 2 * 3" []expected

let test_parse_expression_simple_3 () =
  let second_sum = (BinaryOp ("+", (NumericLiteral "1", Pos (0, 0)),
                                 (NumericLiteral "2", Pos (4, 4))),
                    Pos (0, 4)) in
  let expected = (BinaryOp ("+",
                                second_sum,
                                (NumericLiteral "3", Pos (8, 8))),
                  Pos (0, 8))
  in
    test_parse_expr_helper "1 + 2 + 3" [] expected

let test_parse_expression_simple_4 () =
  let expected = (BinaryOp ("-",
                                (BinaryOp ("+", (NumericLiteral "1", Pos (0, 0)),
                                               (NumericLiteral "2", Pos (4, 4))),
                                 Pos (0, 4)),
                                (NumericLiteral "3", Pos (8, 8))),
                  Pos (0, 8))
  in
    test_parse_expr_helper "1 + 2 - 3" [] expected

let test_parse_expression_simple_5 () =
  let expected = (BinaryOp ("+",
                                (BinaryOp ("/", (NumericLiteral "1", Pos (0, 0)),
                                               (NumericLiteral "2", Pos (4, 4))),
                                 Pos (0, 4)),
                                (BinaryOp ("*", (NumericLiteral "2", Pos (8, 8)),
                                               (NumericLiteral "3", Pos (12, 12))),
                                 Pos (8, 12))),
                  Pos (0, 12))
  in
    test_parse_expr_helper "1 / 2 + 2 * 3" [] expected

let test_parse_select_helper = parse_helper parse_select_helper;;

let test_parse_simple_select_1 () =
  let expected = (Select ({fields = [(Column (Identifier "*", Pos (7, 7)), Pos(7, 7))];
                           clauses =
                              [SelectFromClause [(TableName "TABLE", Pos (14, 18))],
                               Pos (9, 18)]}),
                  Pos (0, 18))
  in
    test_parse_select_helper "SELECT * FROM table" [] expected;
    test_parse_select_helper "select * from table" [] expected;;

let test_parse_simple_select_2 () =
  let expected = (Select ({fields =
                              [Column (Identifier "*", Pos (7, 7)), Pos(7, 7);
                               Column (Identifier "ORDERID", Pos (10, 16)), Pos(10, 16)];
                           clauses =
                              [SelectFromClause
                                 [(TableName "TABLE", Pos (23, 27));
                                  (TableName "ORDERS", Pos (30, 35))],
                               Pos (18, 35)]}),
                  Pos (0, 35))
  in
    test_parse_select_helper "SELECT *, OrderId FROM table, orders" [] expected

let test_parse_simple_select_3 () =
  let expected = (Select ({fields =
                              [Column (BinaryOp (".",
                                              (Identifier "O", Pos (7, 7)),
                                              (Identifier "ORDERID", Pos (9, 15))),
                                       Pos (7, 15)), Pos(7, 15)];
                           clauses =
                              [SelectFromClause
                                 [(TableAlias ("ORDERS", "O"), Pos (22, 29))],
                               Pos (17, 29)]}),
                  Pos (0, 29))
  in
    test_parse_select_helper "SELECT o.OrderId FROM orders o" [] expected

let test_parse_simple_select_4 () =
  let expected = (Select ({fields =
                              [Column (BinaryOp ("+",
                                          (NumericLiteral "1", Pos (7, 7)),
                                          (NumericLiteral "2", Pos (11, 11))),
                                Pos (7, 11)), Pos(7, 11)];
                           clauses =
                              [SelectFromClause [(TableName "DUAL", Pos (18, 21))],
                               Pos (13, 21)]}),
                  Pos (0, 21))
  in
    test_parse_select_helper "SELECT 1 + 2 FROM dual" [] expected

let test_parse_alias_1 () =
  let expected = (Select
                    {fields =
                        [(ColumnAlias ("AS",
                                       (BinaryOp ("+", (NumericLiteral "1", Pos (7, 7)),
                                                      (NumericLiteral "2", Pos (11, 11))),
                                        Pos (7, 11)),
                                       "SUM"),
                          Pos (7, 18))];
                     clauses =
                        [SelectFromClause
                           [(TableName "DUAL", Pos (25, 28))],
                         Pos (20, 28)]},
                  Pos (0, 28))
  in
    test_parse_select_helper "SELECT 1 + 2 as sum FROM dual" [] expected

let test_parse_alias_2 () =
  let expected = (Select
                    {fields =
                        [(ColumnAlias ("",
                                       (BinaryOp ("+", (NumericLiteral "1", Pos (7, 7)),
                                                      (NumericLiteral "2", Pos (11, 11))),
                                        Pos (7, 11)),
                                       "SUM"),
                          Pos (7, 15))];
                     clauses =
                        [SelectFromClause
                           [(TableName "DUAL", Pos (22, 25))],
                         Pos (17, 25)]},
                  Pos (0, 25))
  in
    test_parse_select_helper "SELECT 1 + 2 sum FROM dual" [] expected

let suite = "Parser tests" >::: ["test_lex_begin_end" >:: test_lex_begin_end;
                                 "test_lex_simple_select" >:: test_lex_simple_select;
                                 "test_parse_begin_end" >:: test_parse_begin_end;

                                 "test_pwm_lookahead" >:: test_pwm_lookahead;
                                 "test_pwm_eoi" >:: test_pwm_eoi;
                                 "test_pwm_consume_1" >:: test_pwm_consume_1;
                                 "test_pwm_consume_2" >:: test_pwm_consume_2;
                                 "test_pwm_until_eoi" >:: test_pwm_until_eoi;
                                 "test_pwm_consume_or_fake" >:: test_pwm_consume_or_fake;

                                 "test_parse_declare_begin_end" >::
                                   test_parse_declare_begin_end;
                                 "test_parse_empty_block_with_decl" >::
                                   test_parse_empty_block_with_decl;
                                 "test_parse_begin_end_no_semicolon" >::
                                   test_parse_begin_end_no_semicolon;
                                 "test_parse_simple_complete_block_1" >::
                                   test_parse_simple_complete_block_1;
                                 "test_parse_simple_complete_block_2" >::
                                   test_parse_simple_complete_block_2;

                                 "test_parse_expression_simple_1" >::
                                   test_parse_expression_simple_1;
                                 "test_parse_expression_simple_2" >::
                                   test_parse_expression_simple_2;
                                 "test_parse_expression_simple_3" >::
                                   test_parse_expression_simple_3;
                                 "test_parse_expression_simple_4" >::
                                   test_parse_expression_simple_4;
                                 "test_parse_expression_simple_5" >::
                                   test_parse_expression_simple_5;

                                 "test_parse_simple_select_1" >::
                                   test_parse_simple_select_1;
                                 "test_parse_simple_select_2" >::
                                   test_parse_simple_select_2;
                                 "test_parse_simple_select_3" >::
                                   test_parse_simple_select_3;
                                 "test_parse_simple_select_4" >::
                                   test_parse_simple_select_4;
                                 "test_parse_alias_1" >:: test_parse_alias_1;
                                 "test_parse_alias_2" >:: test_parse_alias_2;
                                ]

let _ =
  run_test_tt_main suite
