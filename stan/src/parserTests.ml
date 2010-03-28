
open OUnit
open Parser

let test_lex_helper str expected =
  let tokens, _ = tokenize str 0 in
    assert_equal expected tokens

let test_lex_begin_end () =
  test_lex_helper
    "BEGIN END;"
    [Token ("BEGIN", Pos (0, 4));
     Token ("END", Pos (6, 8));
     Token (";", Pos (9, 9))]

let test_lex_simple_select () =
  test_lex_helper
    "SELECT Field1 FROM TestTable;"
    [Token ("SELECT", Pos (0, 5));
     Token ("Field1", Pos (7, 12));
     Token ("FROM", Pos (14, 17));
     Token ("TestTable", Pos (19, 27));
     Token (";", Pos (28, 28))]

let test_parse_helper str expected =
  let tokens, _ = tokenize str 0 in
  let ast, _ = parse tokens in
    assert_equal expected ast

let test_parse_begin_end () =
  test_parse_helper
    "BEGIN END;"
    [Block([], []), Pos(0, 9)]

let test_parse_declare_begin_end () =
  test_parse_helper
    "DECLARE BEGIN END;"
    [Block([], []), Pos(0, 17)]

let test_parse_empty_block_with_decl () =
  test_parse_helper
    "DECLARE var INTEGER; BEGIN END;"
    [Block([VarDecl("var", "INTEGER"), Pos(8, 19)], []), Pos(0, 30)]

let test_parse_simple_complete_block_1 () =
  test_parse_helper
    "DECLARE var INTEGER; BEGIN var := 0; END;"
    [(Block ([(VarDecl ("var", "INTEGER"), Pos (8, 19))],
             [(StmtAssignment ("var", (ExprNumLiteral "0", Pos (34, 34))),
               Pos (27, 35))]),
      Pos (0, 40))]

let test_parse_simple_complete_block_2 () =
  test_parse_helper
    "DECLARE var INTEGER; BEGIN var := a; END;"
    [(Block ([(VarDecl ("var", "INTEGER"), Pos (8, 19))],
             [(StmtAssignment ("var", (ExprIdentifier "a", Pos (34, 34))),
               Pos (27, 35))]),
      Pos (0, 40))]

let suite = "Parser tests" >::: ["test_lex_begin_end" >:: test_lex_begin_end;
                                 "test_lex_simple_select" >:: test_lex_simple_select;
                                 "test_parse_declare_begin_end" >::
                                   test_parse_declare_begin_end;
                                 "test_parse_empty_block_with_decl" >::
                                   test_parse_empty_block_with_decl;
                                 "test_parse_simple_complete_block_1" >::
                                   test_parse_simple_complete_block_1;
                                 "test_parse_simple_complete_block_2" >::
                                   test_parse_simple_complete_block_2;
                                 "test_parse_begin_end" >:: test_parse_begin_end]

let _ =
  run_test_tt_main suite
