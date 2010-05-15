
open Utils;;
open Printf;;
open PlsqlParser.Ast;;

type 'a absint = AbsintM of 'a list;;

let result k = AbsintM k;;

let run_absint (AbsintM list) = list;;

let bind (AbsintM m) f = AbsintM (List.map f m |> List.map run_absint |> List.concat);;

let (>>=) m f = bind m f;;

type expr =
  | Number of int
  | Sum of expr * expr
  | Var of string
  | LessThan of expr * expr
  | Equal of expr * expr

type program =
  | Seq of program list
  | Declare of string
  | Assignment of string * expr
  | Alternative of expr * program * program
  | While of expr * program
  | Nop

let program_1 = Seq [Declare "a";
                     Declare "b";
                     Assignment("a", Sum(Number 1, Number 2));
                     Alternative(LessThan(Var "a", Var "b"),
                                 Assignment("a", Var("b")),
                                 Nop)];;

type var_state =
  | Uninitialized
  | InitializedUnknown
  | Value of int

module VarMap = Map.Make(String);;

let rec int_abs program vars =
  match program with
    | Seq statements ->
        print_endline "Seq";
        chain statements vars
    | Declare var ->
        printf "Declare %s\n" var;
        let new_vars = VarMap.add var Uninitialized vars in
          result [new_vars]
    | _ ->
        failwith "gigeeeleeee"

and chain statements vars =
  match statements with
    | hd :: tl ->
        (int_abs hd vars >>= fun new_vars ->
           chain tl new_vars)
    | [] ->
        result []

let run program =
  let absint_monad = int_abs program VarMap.empty in
    run_absint absint_monad;;
