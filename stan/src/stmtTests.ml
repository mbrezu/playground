
open OUnit;;
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;
open Ast;;
open TestsCommon;;

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
    [Warning(Error, "Expected ';'.", 8)]
    (Program([Block([], []), Pos(0, 8)]), Pos(0, 8));
  test_parse_helper
    "DECLARE BEGIN END"
    [Warning(Error, "Expected ';'.", 16)]
    (Program([Block([], []), Pos(0, 16)]), Pos(0, 16));
  test_parse_helper
    "DECLARE var int BEGIN END"
    [Warning(Error, "Expected ';'.", 14);
     Warning(Error, "Expected ';'.", 24)]
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

let test_parse_select_1 () =
  test_parse_helper
    "SELECT a, b FROM c;"
    []
    (Program
       [(StmtSelect
           (Select
              {fields =
                  [(Column (Identifier "A", Pos (7, 7)), Pos (7, 7));
                   (Column (Identifier "B", Pos (10, 10)), Pos (10, 10))];
               clauses =
                  [(FromClause [(TableName "C", Pos (17, 17))], Pos (12, 17))]},
            Pos (0, 17)),
         Pos (0, 18))],
     Pos (0, 18));;

let test_parse_select_1 () =
  test_parse_helper
    "SELECT a, b FROM c;"
    []
    (Program
       [(StmtSelect
           (Select
              {fields =
                  [(Column (Identifier "A", Pos (7, 7)), Pos (7, 7));
                   (Column (Identifier "B", Pos (10, 10)), Pos (10, 10))];
               clauses =
                  [(FromClause [(TableName "C", Pos (17, 17))], Pos (12, 17))]},
            Pos (0, 17)),
         Pos (0, 18))],
     Pos (0, 18));;

let test_parse_select_2 () =
  test_parse_helper
    "BEGIN SELECT a, b FROM c; END;"
    []
    (Program
       [(Block ([],
                [(StmtSelect
                    (Select
                       {fields =
                           [(Column (Identifier "A", Pos (13, 13)), Pos (13, 13));
                            (Column (Identifier "B", Pos (16, 16)), Pos (16, 16))];
                        clauses =
                           [(FromClause [(TableName "C", Pos (23, 23))], Pos (18, 23))]},
                     Pos (6, 23)),
                  Pos (6, 24))]),
         Pos (0, 29))],
     Pos (0, 29));;

let test_parse_if_1 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            NoElse),
         Pos (0, 33))],
     Pos (0, 33));;

let test_parse_if_2 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; b := a * 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25));
             (StmtAssignment ("B",
                              (BinaryOp ("*",
                                         (Identifier "A", Pos (32, 32)),
                                         (NumericLiteral "2", Pos (36, 36))),
                               Pos (32, 36))),
              Pos (27, 37))],
            NoElse),
         Pos (0, 45))],
     Pos (0, 45));;

let test_parse_if_3 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSE b := a * 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            Else
              [(StmtAssignment ("B",
                                (BinaryOp ("*",
                                           (Identifier "A", Pos (37, 37)),
                                           (NumericLiteral "2", Pos (41, 41))),
                                 Pos (37, 41))),
                Pos (32, 42))]),
         Pos (0, 50))],
     Pos (0, 50));;

let test_parse_if_4 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSE b := a * 2; a := 7 - b; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            Else
              [(StmtAssignment ("B",
                                (BinaryOp ("*",
                                           (Identifier "A", Pos (37, 37)),
                                           (NumericLiteral "2", Pos (41, 41))),
                                 Pos (37, 41))),
                Pos (32, 42));
               (StmtAssignment ("A",
                                (BinaryOp ("-",
                                           (NumericLiteral "7", Pos (49, 49)),
                                           (Identifier "B", Pos (53, 53))),
                                 Pos (49, 53))),
                Pos (44, 54))]),
         Pos (0, 62))],
     Pos (0, 62));;

let test_parse_if_5 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSIF a < 5 THEN b := a * 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            ElsIf
              ((BinaryOp ("<",
                          (Identifier "A", Pos (33, 33)),
                          (NumericLiteral "5", Pos (37, 37))),
                Pos (33, 37)),
               [(StmtAssignment ("B",
                                 (BinaryOp ("*",
                                            (Identifier "A", Pos (49, 49)),
                                            (NumericLiteral "2", Pos (53, 53))),
                                  Pos (49, 53))),
                 Pos (44, 54))],
               NoElse)),
         Pos (0, 62))],
     Pos (0, 62));;

let test_parse_if_6 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSIF a < 5 THEN b := a * 2; ELSE b := 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            ElsIf
              ((BinaryOp ("<",
                          (Identifier "A", Pos (33, 33)),
                          (NumericLiteral "5", Pos (37, 37))),
                Pos (33, 37)),
               [(StmtAssignment ("B",
                                 (BinaryOp ("*",
                                            (Identifier "A", Pos (49, 49)),
                                            (NumericLiteral "2", Pos (53, 53))),
                                  Pos (49, 53))),
                 Pos (44, 54))],
               Else
                 [(StmtAssignment ("B",
                                   (NumericLiteral "2", Pos (66, 66))),
                   Pos (61, 67))])),
         Pos (0, 75))],
     Pos (0, 75));;

let test_parse_if_7 () =
  test_parse_helper
    "
IF a > 10 THEN a := a + 1; 
ELSIF a < 5 THEN b := a * 2; 
ELSIF a = b THEN b := 1; 
ELSE b := 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (4, 4)),
                       (NumericLiteral "10", Pos (8, 9))),
             Pos (4, 9)),
            [(StmtAssignment ("A",
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (21, 21)),
                                         (NumericLiteral "1", Pos (25, 25))),
                               Pos (21, 25))),
              Pos (16, 26))],
            ElsIf
              ((BinaryOp ("<",
                          (Identifier "A", Pos (35, 35)),
                          (NumericLiteral "5", Pos (39, 39))),
                Pos (35, 39)),
               [(StmtAssignment ("B",
                                 (BinaryOp ("*",
                                            (Identifier "A", Pos (51, 51)),
                                            (NumericLiteral "2", Pos (55, 55))),
                                  Pos (51, 55))),
                 Pos (46, 56))],
               ElsIf
                 ((BinaryOp ("=",
                             (Identifier "A", Pos (65, 65)),
                             (Identifier "B", Pos (69, 69))),
                   Pos (65, 69)),
                  [(StmtAssignment ("B",
                                    (NumericLiteral "1", Pos (81, 81))),
                    Pos (76, 82))],
                  Else
                    [(StmtAssignment ("B",
                                      (NumericLiteral "2", Pos (95, 95))),
                      Pos (90, 96))]))),
         Pos (1, 104))],
     Pos (1, 104));;

let suite = "Select tests" >::: [ "test_parse_begin_end" >:: test_parse_begin_end;
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
                                  "test_parse_select_1" >:: test_parse_select_1;
                                  "test_parse_select_2" >:: test_parse_select_2;
                                  "test_parse_if_1" >:: test_parse_if_1;
                                  "test_parse_if_2" >:: test_parse_if_2;
                                  "test_parse_if_3" >:: test_parse_if_3;
                                  "test_parse_if_4" >:: test_parse_if_4;
                                  "test_parse_if_5" >:: test_parse_if_5;
                                  "test_parse_if_6" >:: test_parse_if_6;
                                  "test_parse_if_7" >:: test_parse_if_7;
                                ]
