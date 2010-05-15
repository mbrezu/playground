
open Utils;;
open Printf;;
open PlsqlParser.Ast;;

type ('a, 'b) absint = AbsintM of ('a -> 'a * 'b list) * ('a list -> 'a);;

let result k state_combiner = AbsintM ((fun state -> state, k),
                                       state_combiner);;

let run_absint (AbsintM(f, _)) state = f state;;

let bind m f =
  let AbsintM(_, state_combiner) = m in
    AbsintM ((fun st ->
                let new_state, results = run_absint m st in
                let conts = List.map f results in
                let states_and_results =
                  List.map (fun cont -> run_absint cont new_state) conts in
                let states = List.map fst states_and_results in
                let results = List.map snd states_and_results |> List.concat in
                  (state_combiner states, results)),
             state_combiner);;

let (>>=) m f = bind m f;;

let (<|>) m1 m2 =
  let AbsintM(_, state_combiner) = m1 in
    AbsintM ((fun st ->
                let new_state1, results1 = run_absint m1 st in
                let new_state2, results2 = run_absint m2 st in
                  ((state_combiner [new_state1; new_state2]), results1 @ results2)),
             state_combiner);;

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
  | Unknown
      (* | Value of int *)

module VarMap = Map.Make(String);;

let result k new_message = AbsintM ((fun state -> new_message @ state, k),
                                    List.concat);;

let (<+>) m1 m2 = m1 >>= fun _ -> m2;;

let print_vars vars =
  VarMap.iter
    (fun key value ->
       let value_str = match value with
         | Uninitialized -> "uninitialized"
         | Unknown -> "unknown"
       in
         printf "%s %s\n" key value_str)
    vars;;

let rec int_abs program vars =
  match program with
    | Seq statements ->
        result [vars] ["Seq"] <+> chain statements vars
    | Assignment(var, expr) ->
        ((if VarMap.mem var vars
          then result [vars] []
          else result [vars] [sprintf "Undeclared variable '%s'." var])
         <+> eval expr vars >>= fun value ->
           let new_vars = VarMap.add var value vars in
             result [new_vars] [])
    | Declare var ->
        let new_vars = VarMap.add var Uninitialized vars in
          result [new_vars] [sprintf "Declare '%s'." var]
    | Nop ->
        result [vars] ["Nop"]
    | While(cond, body) ->
        result [vars] ["While"]
        <+> eval cond vars
        <+> int_abs body vars
    | Alternative(cond, then_body, else_body) ->
        result [vars] ["Alternative"]
        <+> eval cond vars
        <+> ((int_abs then_body vars) <|> (int_abs else_body vars))
    | _ ->
        failwith "gigeeeleeee"

and eval expr vars =
  match expr with
    | Number _ -> result [Unknown] []
    | Sum(expr1, expr2)
    | LessThan(expr1, expr2)
    | Equal(expr1, expr2)  ->
        (eval expr1 vars >>= fun val1 ->
           eval expr2 vars >>= fun val2 ->
             result [combine_vals val1 val2] [])
    | Var var ->
        if not (VarMap.mem var vars)
        then result [Unknown] [sprintf "Undeclared variable '%s'." var]
        else
          let variable_state = VarMap.find var vars in
            match variable_state with
              | Uninitialized ->
                  result [Uninitialized] [sprintf "Uninitialized variable '%s'." var]
              | Unknown ->
                  result [Unknown] []

and combine_vals expr1 expr2 =
  match (expr1, expr2) with
    | Unknown, Unknown -> Unknown
    | Uninitialized, _ -> Uninitialized
    | _, Uninitialized -> Uninitialized

and chain statements vars =
  match statements with
    | hd :: tl ->
        (int_abs hd vars >>= fun new_vars ->
           chain tl new_vars)
    | [] ->
        result [] [""]

let run program =
  let absint_monad = int_abs program VarMap.empty in
  let state, vars = run_absint absint_monad [] in
    (List.rev state, vars);;
