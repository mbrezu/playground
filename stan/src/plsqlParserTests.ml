
open OUnit;;
open ParserTypes;;
open PlsqlParser;;
open PlsqlParser.Expression;;
open PlsqlParser.Select;;
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

let test_lex_string_literal () =
  test_lex_helper
    "\'Test\' 1 \'\'\'Test2\'\'\' \'Test3"
    [Token ("'Test'", Pos (0, 5));
     Token ("1", Pos (7, 7));
     Token ("'''Test2'''", Pos (9, 19));
     Token ("'Test3", Pos (21, 26))];;

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

let test_parse_expression_logical_1 () =
  let expected = (BinaryOp ("OR",
                            (BinaryOp ("<",
                                       (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("AND",
                                       (BinaryOp (">",
                                                  (BinaryOp ("+",
                                                             (NumericLiteral "2",
                                                              Pos (9, 9)),
                                                             (NumericLiteral "2",
                                                              Pos (13, 13))),
                                                   Pos (9, 13)),
                                                  (NumericLiteral "3", Pos (17, 17))),
                                        Pos (9, 17)),
                                       (BinaryOp ("<",
                                                  (NumericLiteral "2", Pos (23, 23)),
                                                  (NumericLiteral "3", Pos (27, 27))),
                                        Pos (23, 27))),
                             Pos (9, 27))),
                  Pos (0, 27))
  in
    test_parse_expr_helper "1 < 2 OR 2 + 2 > 3 AND 2 < 3" [] expected

let test_parse_expression_logical_2 () =
  let expected = (BinaryOp ("OR",
                            (BinaryOp ("<",
                                       (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("AND",
                                       (UnaryOp ("NOT",
                                                 (BinaryOp ("<",
                                                            (BinaryOp ("+",
                                                                       (NumericLiteral "2",
                                                                        Pos (13, 13)),
                                                                       (NumericLiteral "2",
                                                                        Pos (17, 17))),
                                                             Pos (13, 17)),
                                                            (NumericLiteral "3",
                                                             Pos (21, 21))),
                                                  Pos (13, 21))),
                                        Pos (9, 21)),
                                       (BinaryOp ("<",
                                                  (NumericLiteral "2", Pos (27, 27)),
                                                  (NumericLiteral "3", Pos (31, 31))),
                                        Pos (27, 31))),
                             Pos (9, 31))),
                  Pos (0, 31))
  in
    test_parse_expr_helper "1 < 2 OR NOT 2 + 2 < 3 AND 2 < 3" [] expected

let test_parse_expression_logical_3 () =
  let expected = (BinaryOp ("OR",
                            (BinaryOp ("=",
                                       (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("AND",
                                       (UnaryOp ("NOT",
                                                 (BinaryOp ("<>",
                                                            (BinaryOp ("+",
                                                                       (NumericLiteral "2",
                                                                        Pos (13, 13)),
                                                                       (NumericLiteral "2",
                                                                        Pos (17, 17))),
                                                             Pos (13, 17)),
                                                            (NumericLiteral "3",
                                                             Pos (22, 22))),
                                                  Pos (13, 22))),
                                        Pos (9, 22)),
                                       (BinaryOp ("<",
                                                  (NumericLiteral "2", Pos (28, 28)),
                                                  (NumericLiteral "3", Pos (32, 32))),
                                        Pos (28, 32))),
                             Pos (9, 32))),
                  Pos (0, 32))
  in
    test_parse_expr_helper "1 = 2 OR NOT 2 + 2 <> 3 AND 2 < 3" [] expected

let test_parse_expression_parens_1 () =
  let expected = (BinaryOp ("*",
                            (BinaryOp ("+",
                                       (NumericLiteral "1", Pos (1, 1)),
                                       (NumericLiteral "2", Pos (5, 5))),
                             Pos (0, 6)),
                            (NumericLiteral "3", Pos (10, 10))),
                  Pos (0, 10))
  in
    test_parse_expr_helper "(1 + 2) * 3" [] expected

let test_parse_expression_parens_2 () =
  let expected = (BinaryOp ("*",
                            (BinaryOp ("+",
                                       (NumericLiteral "1", Pos (1, 1)),
                                       (NumericLiteral "2", Pos (5, 5))),
                             Pos (0, 6)),
                            (BinaryOp ("-",
                                       (NumericLiteral "3", Pos (11, 11)),
                                       (NumericLiteral "4", Pos (15, 15))),
                             Pos (10, 16))),
                  Pos (0, 16))
  in
    test_parse_expr_helper "(1 + 2) * (3 - 4)" [] expected

let test_parse_expression_function_1 () =
  let expected = (BinaryOp ("+",
                            (Call
                               ((Identifier "NVL", Pos (0, 2)),
                                [(Identifier "A", Pos (4, 4));
                                 (BinaryOp ("+",
                                            (NumericLiteral "1", Pos (7, 7)),
                                            (NumericLiteral "2", Pos (11, 11))),
                                  Pos (7, 11))]),
                             Pos (0, 12)),
                            (Call
                               ((Identifier "MAX", Pos (16, 18)),
                                [(NumericLiteral "3", Pos (20, 20));
                                 (NumericLiteral "4", Pos (23, 23));
                                 (Identifier "B", Pos (26, 26))]),
                             Pos (16, 27))),
                  Pos (0, 27))
  in
    test_parse_expr_helper "NVL(a, 1 + 2) + MAX(3, 4, b)" [] expected

let test_parse_expression_is_null () =
  let expected_1 = (BinaryOp ("AND",
                              (IsNull
                                 (Identifier "A", Pos (0, 0)),
                               Pos (0, 8)),
                              (BinaryOp ("=",
                                         (Identifier "A", Pos (14, 14)),
                                         (Identifier "C", Pos (18, 18))),
                               Pos (14, 18))),
                    Pos (0, 18))
  in
  let expected_2 = (BinaryOp ("AND",
                              (IsNotNull
                                 (Identifier "B", Pos (0, 0)),
                               Pos (0, 12)),
                              (BinaryOp ("=",
                                         (Identifier "A", Pos (18, 18)),
                                         (Identifier "C", Pos (22, 22))),
                               Pos (18, 22))),
                    Pos (0, 22))
  in
    test_parse_expr_helper "a IS NULL AND a = c" [] expected_1;
    test_parse_expr_helper "b IS NOT NULL AND a = c" [] expected_2;;

let test_parse_expression_like () =
  let expected = (BinaryOp ("AND",
                            (Like ((Identifier "A", Pos (0, 0)), "'Test%'"), Pos (0, 13)),
                            (BinaryOp ("=",
                                       (Identifier "B", Pos (19, 19)),
                                       (BinaryOp ("+",
                                                  (Identifier "C", Pos (23, 23)),
                                                  (NumericLiteral "2", Pos (27, 27))),
                                        Pos (23, 27))),
                             Pos (19, 27))),
                  Pos (0, 27))
  in
    test_parse_expr_helper "A LIKE \'Test%\' AND B = C + 2" [] expected;;

let test_parse_select_helper = parse_helper parse_select_helper;;

let test_parse_simple_select_1 () =
  let expected = (Select ({fields = [(Column (Identifier "*", Pos (7, 7)), Pos(7, 7))];
                           clauses =
                              [FromClause [(TableName "TABLE", Pos (14, 18))],
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
                              [FromClause
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
                              [FromClause
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
                              [FromClause [(TableName "DUAL", Pos (18, 21))],
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
                        [FromClause
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
                        [FromClause
                           [(TableName "DUAL", Pos (22, 25))],
                         Pos (17, 25)]},
                  Pos (0, 25))
  in
    test_parse_select_helper "SELECT 1 + 2 sum FROM dual" [] expected

let test_parse_where_1 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "AUTHOR", Pos (7, 12)), Pos (7, 12));
                         (Column (Identifier "TITLE", Pos (15, 19)), Pos (15, 19));
                         (Column (Identifier "PRICE", Pos (22, 26)), Pos (22, 26))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (33, 37))], Pos (28, 37));
                         (WhereClause
                            (BinaryOp ("<",
                                       (Identifier "PRICE", Pos (45, 49)),
                                       (NumericLiteral "100", Pos (53, 55))),
                             Pos (45, 55)),
                          Pos (39, 55))]},
                  Pos (0, 55))
  in
    test_parse_select_helper
      "SELECT Author, Title, Price FROM Table WHERE Price < 100"
      []
      expected

let test_parse_where_2 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "AUTHOR", Pos (7, 12)), Pos (7, 12));
                         (Column (Identifier "TITLE", Pos (15, 19)), Pos (15, 19));
                         (Column (Identifier "PRICE", Pos (22, 26)), Pos (22, 26))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (33, 37))],
                          Pos (28, 37));
                         (WhereClause
                            (BinaryOp ("AND",
                                       (BinaryOp (">=",
                                                  (Identifier "PRICE", Pos (45, 49)),
                                                  (NumericLiteral "10", Pos (54, 55))),
                                        Pos (45, 55)),
                                       (BinaryOp ("<=",
                                                  (Identifier "PRICE", Pos (61, 65)),
                                                  (NumericLiteral "100", Pos (70, 72))),
                                        Pos (61, 72))),
                             Pos (45, 72)),
                          Pos (39, 72))]},
                  Pos (0, 72))
  in
    test_parse_select_helper
      "SELECT Author, Title, Price FROM Table WHERE Price >= 10 AND Price <= 100"
      []
      expected

let test_parse_order_by_1 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)), Pos (7, 7))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (14, 18))], Pos (9, 18));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (26, 30)),
                                       (NumericLiteral "10", Pos (35, 36))),
                             Pos (26, 36)),
                          Pos (20, 36));
                         (OrderByClause
                            ((Identifier "COST", Pos (47, 50)), "ASC"),
                          Pos (38, 54))]},
                  Pos (0, 54))
  in
    test_parse_select_helper
      "SELECT * FROM Table WHERE Price >= 10 ORDER BY Cost ASC"
      []
      expected

let test_parse_order_by_2 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)), Pos (7, 7))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (14, 18))], Pos (9, 18));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (26, 30)),
                                       (NumericLiteral "10", Pos (35, 36))),
                             Pos (26, 36)),
                          Pos (20, 36));
                         (OrderByClause
                            ((Identifier "COST", Pos (47, 50)), ""),
                          Pos (38, 50))]},
                  Pos (0, 50))
  in
    test_parse_select_helper
      "SELECT * FROM Table WHERE Price >= 10 ORDER BY Cost"
      []
      expected

let test_parse_order_by_3 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)), Pos (7, 7))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (14, 18))], Pos (9, 18));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (26, 30)),
                                       (NumericLiteral "10", Pos (35, 36))),
                             Pos (26, 36)),
                          Pos (20, 36));
                         (OrderByClause
                            ((Identifier "COST", Pos (47, 50)), "DESC"),
                          Pos (38, 55))]},
                  Pos (0, 55))
in
  test_parse_select_helper
    "SELECT * FROM Table WHERE Price >= 10 ORDER BY Cost DESC"
    []
    expected

let test_parse_group_by_1 () =
  let expected = (Select
     {fields =
       [(Column (Identifier "PRODUCT", Pos (7, 13)), Pos (7, 13));
        (Column (Identifier "PRICE", Pos (16, 20)), Pos (16, 20))];
      clauses =
       [(FromClause [(TableName "TABLE", Pos (27, 31))], Pos (22, 31));
        (WhereClause
          (BinaryOp (">=",
            (Identifier "PRICE", Pos (39, 43)),
            (NumericLiteral "10", Pos (48, 49))),
           Pos (39, 49)),
         Pos (33, 49));
        (GroupByClause
           [(Identifier "PRODUCT", Pos (60, 66)); (Identifier "PRICE", Pos (69, 73))],
         Pos (51, 73))]},
    Pos (0, 73))
in
  test_parse_select_helper
    "SELECT Product, Price FROM Table WHERE Price >= 10 GROUP BY Product, Price"
    []
    expected

let suite = "Parser tests" >::: ["test_lex_begin_end" >:: test_lex_begin_end;
                                 "test_lex_simple_select" >:: test_lex_simple_select;
                                 "test_lex_string_literal" >:: test_lex_string_literal;

                                 "test_pwm_lookahead" >:: test_pwm_lookahead;
                                 "test_pwm_eoi" >:: test_pwm_eoi;
                                 "test_pwm_consume_1" >:: test_pwm_consume_1;
                                 "test_pwm_consume_2" >:: test_pwm_consume_2;
                                 "test_pwm_until_eoi" >:: test_pwm_until_eoi;
                                 "test_pwm_consume_or_fake" >:: test_pwm_consume_or_fake;

                                 "test_parse_begin_end" >:: test_parse_begin_end;
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
                                 "test_parse_expression_logical_1" >::
                                   test_parse_expression_logical_1;
                                 "test_parse_expression_logical_2" >::
                                   test_parse_expression_logical_2;
                                 "test_parse_expression_logical_3" >::
                                   test_parse_expression_logical_3;
                                 "test_parse_expression_parens_1" >::
                                   test_parse_expression_parens_1;
                                 "test_parse_expression_parens_2" >::
                                   test_parse_expression_parens_2;
                                 "test_parse_expression_function_1" >::
                                   test_parse_expression_function_1;
                                 "test_parse_expression_is_null" >::
                                   test_parse_expression_is_null;
                                 "test_parse_expression_like" >::
                                   test_parse_expression_like;
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
                                 "test_parse_where_1" >:: test_parse_where_1;
                                 "test_parse_where_2" >:: test_parse_where_2;
                                 "test_parse_order_by_1" >:: test_parse_order_by_1;
                                 "test_parse_order_by_2" >:: test_parse_order_by_2;
                                 "test_parse_order_by_3" >:: test_parse_order_by_3;
                                 "test_parse_group_by_1" >:: test_parse_group_by_1;
                                ]

let _ =
  run_test_tt_main suite
