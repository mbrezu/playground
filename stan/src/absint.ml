
(* ABSINT is short for 'Abstract Interpreter'.

   This file contains some experiments with a state monad enriched
   with backtracking support. I plan to use it to write the STAN
   abstract interpreter. The simple version below contains an abstract
   interpreter for a language with integer variables, assignment,
   looping and alternatives (if statement).

   Nested scopes and GOTO mean that the version for Plsql version will
   be more complicated.

*)

open Utils;;
open Printf;;
(* open PlsqlParser.Ast;; *)

type ('a, 'b) absint_monad = AbsintM of ('a -> 'a * 'b option)

let run_absint (AbsintM(f)) state = f state;;

let bind m f = AbsintM(fun state ->
                         let new_state, maybe_result = run_absint m state in
                           match maybe_result with
                             | Some k ->
                                 let new_absint = f k in
                                   run_absint new_absint new_state
                             | None ->
                                 new_state, None);;

let (>>=) m f = bind m f;;

let (<+>) m1 m2 = m1 >>= fun _ -> m2;;

let result k = AbsintM(fun state -> state, Some k);;

let error = AbsintM(fun state -> state, None);;

let get_state = AbsintM(fun state -> state, Some state);;

let set_state new_state = AbsintM(fun state -> new_state, Some ());;

let update_state f =
  get_state >>= fun state ->
    set_state (f state);;

let amb state_combiner result_combiner ms =
  AbsintM(fun state ->
            let states_results = List.map (fun m -> run_absint m state) ms in
            let states = List.map fst states_results in
            let results = List.map snd states_results in
            let new_state = state_combiner states in
            let result = result_combiner results in
              (new_state, result));;

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
                                 Assignment("a", Number 34),
                                 Assignment("a", Number 2))];;

type var_state =
  | Uninitialized
  | Unknown
      (* | Value of int *)

module VarMap = Map.Make(String);;



(* Specialization of the backtracking state monad above. *)
let add_message message =
  update_state (fun (var_map, messages) -> (var_map, message :: messages));;

let combine_vals expr1 expr2 =
  match (expr1, expr2) with
    | Unknown, Unknown -> Unknown
    | _ -> Uninitialized;;

let absint_amb m1 m2 =
  let combine_var_maps var_maps =
    let combine_maps map1 map2 =
      VarMap.fold
        (fun key value other_map ->
           let combined_value =
             if VarMap.mem key other_map
             then combine_vals value (VarMap.find key other_map)
             else value
           in
             VarMap.add key combined_value other_map)
        map1
        map2
    in
      List.fold_left combine_maps VarMap.empty var_maps
  in
  let combine_states states =
    let combine_two_states state1 state2 =
      let var_map1, messages1 = state1 in
      let var_map2, messages2 = state2 in
      let var_map = combine_var_maps [var_map1; var_map2] in
      let messages = messages1 @ messages2 in
        (var_map, messages)
    in
      List.fold_left combine_two_states (List.hd states) (List.tl states)
  in
    amb combine_states (const (Some ())) [m1; m2];;

let set_vars new_vars =
  get_state >>= fun (_, messages) ->
    set_state (new_vars, messages);;

let get_vars =
  get_state >>= fun (vars, _) ->
    result vars;;

let save_messages =
  get_state >>= fun (var_map, messages) ->
    set_state (var_map, [])
    <+> (result messages);;

let restore_messages messages =
  get_state >>= fun (var_map, current_messages) ->
    set_state (var_map, current_messages @ messages);;

let dump_var_map vars =
  VarMap.iter
    (fun key value ->
       let value_str = match value with
         | Uninitialized -> "Uninitialized"
         | Unknown -> "Unknown"
       in
         printf "'%s' -> %s\n" key value_str)
    vars;;

let rec int_abs program =
  match program with
    | Seq statements ->
        add_message "Seq"
        <+> chain statements
    | Assignment(var, expr) ->
        (add_message "Assignment"
         <+> get_vars >>= fun vars ->
           (if not (VarMap.mem var vars)
            then add_message (sprintf "Undeclared variable '%s'." var)
            else result ())
           <+> eval expr >>= fun value ->
             let new_vars = VarMap.add var value vars in
               set_vars new_vars)
    | Declare var ->
        (get_vars >>= fun vars ->
           let new_vars = VarMap.add var Uninitialized vars in
             set_vars new_vars
             <+> add_message (sprintf "Declare '%s'." var))
    | Nop ->
        add_message "Nop"
    | While(cond, body) ->
        add_message "While"
        <+> eval cond
        <+> int_abs body
    | Alternative(cond, then_body, else_body) ->
        (add_message "Alternative"
         <+> eval cond
         <+> save_messages >>= fun current_messages ->
           (absint_amb (int_abs then_body) (int_abs else_body))
           <+> restore_messages current_messages)

and eval expr =
  match expr with
    | Number _ -> result Unknown
    | Sum(expr1, expr2)
    | LessThan(expr1, expr2)
    | Equal(expr1, expr2)  ->
        (eval expr1 >>= fun val1 ->
           eval expr2 >>= fun val2 ->
             result (combine_vals val1 val2))
    | Var var ->
        get_vars >>= fun vars ->
          if not (VarMap.mem var vars)
          then
            add_message (sprintf "Undeclared variable '%s'." var)
            <+> result Uninitialized
          else
            let variable_state = VarMap.find var vars in
              match variable_state with
                | Uninitialized ->
                    add_message (sprintf "Uninitialized variable '%s'." var)
                    <+> result Uninitialized
                | Unknown ->
                    result Unknown

and chain statements =
  match statements with
    | hd :: tl ->
        int_abs hd <+> (chain tl)
    | [] ->
        result ()

let run program =
  let absint_monad = int_abs program in
  let (vars, messages), _ = run_absint absint_monad (VarMap.empty, []) in
    dump_var_map vars;
    List.rev messages;;
