open OUnit;;
open PlsqlParser.Ast;;
open Utils;;
open AbsintCompiler;;
open Absint.Ir;;
open PlsqlParser;;
open ParserTypes;;

let compile_test_helper str expected_ir =
  let actual_ir = compile_helper str in
    assert_equal expected_ir actual_ir

let test_compile_simple_program () =
  compile_test_helper
    "DECLARE N NUMBER(3); BEGIN NULL; END;"
    [AddFrame;
     Declare ("N", (Number (3, 0), Pos (10, 18)));
     DeleteFrame];;

let test_compile_assignment () =
  compile_test_helper
    "DECLARE N NUMBER(3); BEGIN N := 3 + 5; END;"
    [AddFrame;
     Declare ("N", (Number (3, 0), Pos (10, 18)));
     Assignment ((Identifier "N", Pos (27, 27)),
                 (BinaryOp ("+",
                            (NumericLiteral "3", Pos (32, 32)),
                            (NumericLiteral "5", Pos (36, 36))),
                  Pos (32, 36)));
     DeleteFrame];;

let test_compile_call () =
  compile_test_helper
    "BEGIN DBMS_OUTPUT.PUT_LINE('Hello, world'); END;"
    [AddFrame;
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (6, 16)),
                   (Identifier "PUT_LINE", Pos (18, 25))),
         Pos (6, 25)),
        [(StringLiteral "'Hello, world'", Pos (27, 40))]);
     DeleteFrame];;

let test_compile_if () =
  compile_test_helper
    "
BEGIN
  IF N = 2 THEN
    DBMS_OUTPUT.PUT_LINE('2');
  ELSE
    DBMS_OUTPUT.PUT_LINE('Not 2');
  END IF;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (65, 75)),
                   (Identifier "PUT_LINE", Pos (77, 84))),
         Pos (65, 84)),
        [(StringLiteral "'Not 2'", Pos (86, 92))]);
     Label "AfterIf_3";
     DeleteFrame];;

let test_compile_if_no_else () =
  compile_test_helper
    "
BEGIN
  IF N = 2 THEN
    DBMS_OUTPUT.PUT_LINE('2');
  END IF;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     Label "AfterIf_3";
     DeleteFrame];;

let test_compile_if_elsif () =
  compile_test_helper
    "
BEGIN
  IF N = 2 THEN
    DBMS_OUTPUT.PUT_LINE('2');
  ELSIF N = 3 THEN
    DBMS_OUTPUT.PUT_LINE('3');
  ELSE
    DBMS_OUTPUT.PUT_LINE('Not 2 or 3');
  END IF;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (62, 62)),
                   (NumericLiteral "3", Pos (66, 66))),
         Pos (62, 66)),
        "Then_4", "Else_5");
     Label "Then_4";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (77, 87)),
                   (Identifier "PUT_LINE", Pos (89, 96))),
         Pos (77, 96)),
        [(StringLiteral "'3'", Pos (98, 100))]);
     Goto ("AfterIf_6", None);
     Label "Else_5";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (115, 125)),
                   (Identifier "PUT_LINE", Pos (127, 134))),
         Pos (115, 134)),
        [(StringLiteral "'Not 2 or 3'", Pos (136, 147))]);
     Label "AfterIf_6";
     Label "AfterIf_3";
     DeleteFrame];;

let test_compile_loop_exit () =
  compile_test_helper
    "
DECLARE
  N NUMBER(2);
BEGIN
  N := 1;
  LOOP
    DBMS_OUTPUT.PUT_LINE(N);
    N := N + 1;
    EXIT WHEN N > 10;
  END LOOP;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (62, 62)),
                   (NumericLiteral "3", Pos (66, 66))),
         Pos (62, 66)),
        "Then_4", "Else_5");
     Label "Then_4";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (77, 87)),
                   (Identifier "PUT_LINE", Pos (89, 96))),
         Pos (77, 96)),
        [(StringLiteral "'3'", Pos (98, 100))]);
     Goto ("AfterIf_6", None);
     Label "Else_5";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (115, 125)),
                   (Identifier "PUT_LINE", Pos (127, 134))),
         Pos (115, 134)),
        [(StringLiteral "'Not 2 or 3'", Pos (136, 147))]);
     Label "AfterIf_6";
     Label "AfterIf_3";
     DeleteFrame];;

let suite = "Absint tests" >::: [
  "test_compile_simple_program" >:: test_compile_simple_program;
  "test_compile_assignment" >:: test_compile_assignment;
  "test_compile_call" >:: test_compile_call;
  "test_compile_if" >:: test_compile_if;
  "test_compile_if_no_else" >:: test_compile_if_no_else;
  "test_compile_if_elsif" >:: test_compile_if_elsif;
  "test_compile_loop_exit" >:: test_compile_loop_exit;
];;
