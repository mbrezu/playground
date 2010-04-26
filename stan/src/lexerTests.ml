
open OUnit;;
open ParserTypes;;
open Lexer;;

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

let suite = "Lexer tests" >::: ["test_lex_begin_end" >:: test_lex_begin_end;
                                 "test_lex_simple_select" >:: test_lex_simple_select;
                                 "test_lex_string_literal" >:: test_lex_string_literal;
                               ]

