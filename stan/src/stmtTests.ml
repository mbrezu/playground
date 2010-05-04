
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

let test_parse_basic_loop_1 () =
  test_parse_helper
    "
LOOP
  a := 1;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLoop
           ([(StmtAssignment ("A",
                              (NumericLiteral "1", Pos (13, 13))),
              Pos (8, 14));
             (StmtAssignment ("B",
                              (StringLiteral "'a'", Pos (23, 25))),
              Pos (18, 26))],
            None),
         Pos (1, 36))],
     Pos (1, 36));;

let test_parse_basic_loop_2 () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  b := 'a';
END LOOP label;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (33, 35))),
                            Pos (28, 36))],
                          Some "LABEL"),
                       Pos (11, 52))),
         Pos (1, 52))],
     Pos (1, 52));;

let test_parse_basic_loop_3 () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (33, 35))),
                            Pos (28, 36))],
                          None),
                       Pos (11, 46))),
         Pos (1, 46))],
     Pos (1, 46));;

let test_parse_exit () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  EXIT;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtExit None, Pos (28, 32));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (41, 43))),
                            Pos (36, 44))],
                          None),
                       Pos (11, 54))),
         Pos (1, 54))],
     Pos (1, 54));;

let test_parse_exit_when () =
  test_parse_helper
    "
<<label>>
LOOP
  a := a + 1;
  EXIT WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (BinaryOp ("+",
                                                       (Identifier "A", Pos (23, 23)),
                                                       (NumericLiteral "1", Pos (27, 27))),
                                             Pos (23, 27))),
                            Pos (18, 28));
                           (StmtExitWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (42, 42)),
                                          (NumericLiteral "5", Pos (46, 46))),
                                Pos (42, 46)),
                               None),
                            Pos (32, 47));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (56, 58))),
                            Pos (51, 59))],
                          None),
                       Pos (11, 69))),
         Pos (1, 69))],
     Pos (1, 69));;

let test_parse_exit_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  EXIT label;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtExit (Some "LABEL"), Pos (28, 38));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (47, 49))),
                            Pos (42, 50))],
                          None),
                       Pos (11, 60))),
         Pos (1, 60))],
     Pos (1, 60));;

let test_parse_exit_when_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  EXIT label WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtExitWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (44, 44)),
                                          (NumericLiteral "5", Pos (48, 48))),
                                Pos (44, 48)),
                               Some "LABEL"),
                            Pos (28, 49));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (58, 60))),
                            Pos (53, 61))],
                          None),
                       Pos (11, 71))),
         Pos (1, 71))],
     Pos (1, 71));;

let test_parse_continue () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  CONTINUE;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtContinue None, Pos (28, 36));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (45, 47))),
                            Pos (40, 48))],
                          None),
                       Pos (11, 58))),
         Pos (1, 58))],
     Pos (1, 58));;

let test_parse_continue_when () =
  test_parse_helper
    "
<<label>>
LOOP
  a := a + 1;
  CONTINUE WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (BinaryOp ("+",
                                                       (Identifier "A", Pos (23, 23)),
                                                       (NumericLiteral "1", Pos (27, 27))),
                                             Pos (23, 27))),
                            Pos (18, 28));
                           (StmtContinueWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (46, 46)),
                                          (NumericLiteral "5", Pos (50, 50))),
                                Pos (46, 50)),
                               None),
                            Pos (32, 51));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (60, 62))),
                            Pos (55, 63))],
                          None),
                       Pos (11, 73))),
         Pos (1, 73))],
     Pos (1, 73));;

let test_parse_continue_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  CONTINUE label;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtContinue (Some "LABEL"), Pos (28, 42));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (51, 53))),
                            Pos (46, 54))],
                          None),
                       Pos (11, 64))),
         Pos (1, 64))],
     Pos (1, 64));;

let test_parse_continue_when_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  CONTINUE label WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ("A",
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtContinueWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (48, 48)),
                                          (NumericLiteral "5", Pos (52, 52))),
                                Pos (48, 52)),
                               Some "LABEL"),
                            Pos (28, 53));
                           (StmtAssignment ("B",
                                            (StringLiteral "'a'", Pos (62, 64))),
                            Pos (57, 65))],
                          None),
                       Pos (11, 75))),
         Pos (1, 75))],
     Pos (1, 75));;

let test_parse_for_1 () =
  test_parse_helper
    "
FOR i IN 1..10 LOOP
  a := a + i;
END LOOP;"
    []
    (Program
       [(StmtFor ((Identifier "I", Pos (5, 5)),
                  false,
                  (NumericLiteral "1", Pos (10, 10)),
                  (NumericLiteral "10", Pos (13, 14)),
                  (StmtLoop
                     ([(StmtAssignment ("A",
                                        (BinaryOp ("+",
                                                   (Identifier "A", Pos (28, 28)),
                                                   (Identifier "I", Pos (32, 32))),
                                         Pos (28, 32))),
                        Pos (23, 33))],
                      None),
                   Pos (16, 43))),
         Pos (1, 43))],
     Pos (1, 43));;

let test_parse_for_2 () =
  test_parse_helper
    "
FOR i IN REVERSE 1..10 LOOP
  a := a + i;
END LOOP;"
    []
    (Program
       [(StmtFor ((Identifier "I", Pos (5, 5)),
                  true,
                  (NumericLiteral "1", Pos (18, 18)),
                  (NumericLiteral "10", Pos (21, 22)),
                  (StmtLoop
                     ([(StmtAssignment ("A",
                                        (BinaryOp ("+",
                                                   (Identifier "A", Pos (36, 36)),
                                                   (Identifier "I", Pos (40, 40))),
                                         Pos (36, 40))),
                        Pos (31, 41))],
                      None),
                   Pos (24, 51))),
         Pos (1, 51))],
     Pos (1, 51));;

let test_parse_for_3 () =
  test_parse_helper
    "
<<lup>>
FOR i IN REVERSE 1..10 LOOP
  a := a + i;
END LOOP lup;"
    []
    (Program
       [(StmtLabeled ("LUP",
                      (StmtFor ((Identifier "I", Pos (13, 13)),
                                true,
                                (NumericLiteral "1", Pos (26, 26)),
                                (NumericLiteral "10", Pos (29, 30)),
                                (StmtLoop
                                   ([(StmtAssignment ("A",
                                                      (BinaryOp ("+",
                                                                 (Identifier "A", Pos (44, 44)),
                                                                 (Identifier "I", Pos (48, 48))),
                                                       Pos (44, 48))),
                                      Pos (39, 49))],
                                    Some "LUP"),
                                 Pos (32, 63))),
                       Pos (9, 63))),
         Pos (1, 63))],
     Pos (1, 63));;

let test_parse_for_4 () =
  test_parse_helper
    "
<<lup>>
FOR lup.i IN REVERSE 1..10 LOOP
  a := a + i;
END LOOP lup;"
    []
    (Program
       [(StmtLabeled ("LUP",
                      (StmtFor
                         ((BinaryOp (".",
                                     (Identifier "LUP", Pos (13, 15)),
                                     (Identifier "I", Pos (17, 17))),
                           Pos (13, 17)),
                          true, (NumericLiteral "1", Pos (30, 30)),
                          (NumericLiteral "10", Pos (33, 34)),
                          (StmtLoop
                             ([(StmtAssignment ("A",
                                                (BinaryOp ("+",
                                                           (Identifier "A", Pos (48, 48)),
                                                           (Identifier "I", Pos (52, 52))),
                                                 Pos (48, 52))),
                                Pos (43, 53))],
                              Some "LUP"),
                           Pos (36, 67))),
                       Pos (9, 67))),
         Pos (1, 67))],
     Pos (1, 67));;

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

                                  "test_parse_basic_loop_1" >:: test_parse_basic_loop_1;
                                  "test_parse_basic_loop_2" >:: test_parse_basic_loop_2;
                                  "test_parse_basic_loop_3" >:: test_parse_basic_loop_3;

                                  "test_parse_exit" >:: test_parse_exit;
                                  "test_parse_exit_when" >:: test_parse_exit_when;
                                  "test_parse_exit_labeled" >:: test_parse_exit_labeled;
                                  "test_parse_exit_when_labeled" >::
                                    test_parse_exit_when_labeled;

                                  "test_parse_continue" >:: test_parse_continue;
                                  "test_parse_continue_when" >:: test_parse_continue_when;
                                  "test_parse_continue_labeled" >::
                                    test_parse_continue_labeled;
                                  "test_parse_continue_when_labeled" >::
                                    test_parse_continue_when_labeled;

                                  "test_parse_for_1" >:: test_parse_for_1;
                                  "test_parse_for_2" >:: test_parse_for_2;
                                  "test_parse_for_3" >:: test_parse_for_3;
                                  "test_parse_for_4" >:: test_parse_for_4;
                                ]
