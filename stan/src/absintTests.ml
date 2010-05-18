
open OUnit;;
open Absint;;

let absint_test_helper program expected_env expected_messages =
  let env, messages = run program in
    assert_equal expected_env env;
    assert_equal expected_messages messages;;

let test_simple_seq () =
  let program = Seq [Nop;] in
  let expected_env = [[]] in
  let expected_messages = ["Seq"; "Nop"] in
    absint_test_helper program expected_env expected_messages;;

let test_simple_assignment () =
  let program = Seq [Declare "a"; Assignment("a", Number 2)] in
  let expected_env = [["'a' -> Unknown"]] in
  let expected_messages = ["Seq"; "Declare 'a'."; "Assignment"] in
    absint_test_helper program expected_env expected_messages;;

let test_undeclared_variable () =
  let program = Seq [Assignment("a", Number 2)] in
  let expected_env = [[]] in
  let expected_messages = ["Seq"; "Assignment"; "Undeclared variable 'a'."] in
    absint_test_helper program expected_env expected_messages;;

let test_if_1 () =
  let program = Seq [Declare "a";
                     If(LessThan(Number 1, Number 2),
                                 Assignment("a", Number 2),
                                 Nop)]
  in
  let expected_env = [["'a' -> Uninitialized"]] in
  let expected_messages = ["Seq"; "Declare 'a'."; "If"; "Nop"; "Assignment"] in
    absint_test_helper program expected_env expected_messages;;

let test_if_2 () =
  let program = Seq [Declare "a";
                     Declare "b";
                     Assignment("a", Sum(Number 1, Number 2));
                     If(LessThan(Var "a", Var "b"),
                                 Seq [Declare "c";
                                      Assignment ("c", Number 3);
                                      Assignment("a", Var "c")],
                                 Seq [Declare "d";
                                      Assignment("a", Sum(Number 2, Var "d"))])]
  in
  let expected_env = [["'b' -> Uninitialized";
                       "'a' -> Uninitialized"]] in
  let expected_messages = ["Seq";
                           "Declare 'a'.";
                           "Declare 'b'.";
                           "Assignment";
                           "If";
                           "Uninitialized variable 'b'.";
                           "Seq";
                           "Declare 'd'.";
                           "Assignment";
                           "Uninitialized variable 'd'.";
                           "Seq";
                           "Declare 'c'.";
                           "Assignment";
                           "Assignment"]
  in
    absint_test_helper program expected_env expected_messages;;

let test_while_1 () =
  let program = Seq [Declare "a";
                     Declare "b";
                     Assignment("a", Number 1);
                     While (LessThan(Var "a", Number 5),
                            Seq [Assignment("b", Var "a");
                                 Assignment("a", Sum(Var "a", Number 1))])]
  in
  let expected_env = [["'b' -> Uninitialized";
                       "'a' -> Unknown"]]
  in
  let expected_messages = ["Seq";
                           "Declare 'a'.";
                           "Declare 'b'.";
                           "Assignment";
                           "While";
                           "Nop";
                           "Seq";
                           "Assignment";
                           "Assignment"]
  in
    absint_test_helper program expected_env expected_messages;;

let suite = "Absint tests" >::: ["test_simple_seq" >:: test_simple_seq;
                                 "test_simple_assignment" >:: test_simple_assignment;
                                 "test_undeclared_variable" >:: test_undeclared_variable;
                                 "test_if_1" >:: test_if_1;
                                 "test_if_2" >:: test_if_2;
                                 "test_while_1" >:: test_while_1;
                                ];;
