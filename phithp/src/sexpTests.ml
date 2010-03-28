
open OUnit

open Utils
open Sexp

let sexp1 = Nil
let sexp2 = Pair(Number 1, Pair (Number 2, Nil))

let test_is_nil () =
  assert_equal true (is_nil Nil)

let test_length () =
  assert_equal 2 <| length sexp2

let test_car () =
  assert_equal (Number 1) <| car sexp2

let test_cdr () =
  assert_equal (Pair(Number 2, Nil)) <| cdr sexp2

let test_map () =
  let test_sexp = Pair (Number 2, Pair (Number 3, Nil)) in
  let expected_sexp = Pair (String "2", Pair (String "3", Nil)) in
  let number_to_string sexp =
    match sexp with
      | Number k -> String (string_of_int k)
      | _ -> sexp in
  let mapped = test_sexp |> Sexp.map number_to_string in
    assert_equal expected_sexp mapped

let test_reverse () =
  let test_sexp = Pair (Number 2, Pair (Number 3, Nil)) in
  let expected_sexp = Pair (Number 3, Pair (Number 2, Nil)) in
    assert_equal expected_sexp (reverse test_sexp)

let test_nth () =
  let test_sexp = Pair (Number 2, Pair (Number 3, Nil)) in
    assert_equal (Number 2) <| nth test_sexp 0;
    assert_equal (Number 3) <| nth test_sexp 1

let test_to_list () =
  let test_sexp = Pair (Number 2, Pair (Number 3, Nil)) in
  let expected_list = [Number 2; Number 3] in
    assert_equal expected_list (to_list test_sexp)

let test_env_simple () =
  let env = make_env None in
    env#set "foo" (Number 1) |> ignore;
    env#set "bar" (Number 2) |> ignore;
    assert_equal (Number 1) (env#get "foo");
    assert_equal (Number 2) (env#get "bar")

let test_env_chained () =
  let root_env = make_env None in
  let child_env = make_env (Some root_env) in
    root_env#set "foo" (Number 1) |> ignore;
    child_env#set "foo" (Number 2) |> ignore;
    assert_equal (Number 2) (root_env#get "foo");
    assert_equal (Number 2) (child_env#get "foo");
    child_env#set_local "foo" (Number 3) |> ignore;
    assert_equal (Number 3) (child_env#get "foo");
    assert_equal (Number 2) (root_env#get "foo")

let suite = "Sexp tests" >::: [ "test_is_nil" >:: test_is_nil;
                                "test_car" >:: test_car;
                                "test_cdr" >:: test_cdr;
                                "test_map" >:: test_map;
                                "test_reverse" >:: test_reverse;
                                "test_nth" >:: test_nth;
                                "test_to_list" >:: test_to_list;
                                "test_env_simple" >:: test_env_simple;
                                "test_env_chained" >:: test_env_chained;
                                "test_length" >:: test_length]

