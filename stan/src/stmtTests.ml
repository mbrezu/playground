
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
                                ]