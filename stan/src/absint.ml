
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
  | If of expr * program * program
  | While of expr * program
  | Label of string
  | Goto of string
  | ConditionalGoto of expr * string
  | Nop

type var_state =
  | Uninitialized
  | Unknown
      (* | Value of int *)

module VarMap = Map.Make(String);;

type env = var_state VarMap.t list;;

(* Specialization of the backtracking state monad above. *)

(* Environment *)
let combine_vals expr1 expr2 =
  match (expr1, expr2) with
    | Unknown, Unknown -> Unknown
    | _ -> Uninitialized;;

let absint_amb m1 m2 =
  let combine_2_maps map1 map2 =
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
  let combine_envs envs =
    let combine_2_envs env1 env2 =
      List.map2 combine_2_maps env1 env2
    in
      List.fold_left combine_2_envs (List.hd envs) (List.tl envs)
  in
  let combine_states states =
    let combine_two_states state1 state2 =
      let env1, messages1 = state1 in
      let env2, messages2 = state2 in
      let env = combine_envs [env1; env2] in
      let messages = messages1 @ messages2 in
        (env, messages)
    in
      List.fold_left combine_two_states (List.hd states) (List.tl states)
  in
    amb combine_states (const (Some ())) [m1; m2];;

let set_var var value =
  let rec env_set_var env var value =
    match env with
      | var_map :: other_maps ->
          if VarMap.mem var var_map
          then (VarMap.add var value var_map) :: other_maps
          else var_map :: (env_set_var other_maps var value)
      | [] -> []
  in
    get_state >>= fun (env, messages) ->
      let new_env = env_set_var env var value in
        set_state (new_env, messages);;

let declare_var var =
  get_state >>= fun (env, messages) ->
    let new_env =
      match env with
        | var_map :: tl ->
            (VarMap.add var Uninitialized var_map) :: tl
        | [] ->
            (VarMap.empty |> VarMap.add var Uninitialized) :: []
    in
      set_state (new_env, messages);;

let get_var var =
  let rec env_get_var env var =
    match env with
      | var_map :: other_maps ->
          if VarMap.mem var var_map
          then Some(VarMap.find var var_map)
          else env_get_var other_maps var
      | [] ->
          None
  in
    get_state >>= fun (env, _) ->
      result <| env_get_var env var;;

let add_env_frame =
  get_state >>= fun (env, messages) ->
    set_state (VarMap.empty :: env, messages);;

let remove_env_frame =
  get_state >>= fun (env, messages) ->
    if List.length env > 1
    then set_state (List.tl env, messages)
    else result ();;

(* Messages *)

let add_message message =
  update_state (fun (var_map, messages) -> (var_map, message :: messages));;

let save_messages =
  get_state >>= fun (var_map, messages) ->
    set_state (var_map, [])
    <+> (result messages);;

let restore_messages messages =
  get_state >>= fun (var_map, current_messages) ->
    set_state (var_map, current_messages @ messages);;

(* Debugging *)
let show_env env =
  let dump_var_map var_map =
    VarMap.fold
      (fun key value bindings ->
         let value_str = match value with
           | Uninitialized -> "Uninitialized"
           | Unknown -> "Unknown"
         in
           (sprintf "'%s' -> %s" key value_str) :: bindings)
      var_map
      []
  in
    List.rev env |> List.map dump_var_map;;

(* Abstract Interpreter *)
let rec int_abs program =
  match program with
    | Seq statements ->
        add_message "Seq"
        <+> add_env_frame
        <+> chain statements
        <+> remove_env_frame
    | Assignment(var, expr) ->
        (add_message "Assignment"
         <+> get_var var >>= fun value ->
           (if (value = None)
            then add_message (sprintf "Undeclared variable '%s'." var)
            else result ())
           <+> eval expr >>= fun value ->
             set_var var value)
    | Declare var ->
        add_message <| sprintf "Declare '%s'." var
          <+> declare_var var
    | Nop ->
        add_message "Nop"
    | While(cond, body) ->
        add_message "While"
        <+> eval cond
        <+> eval_alt body Nop
    | If(cond, then_body, else_body) ->
        (add_message "If"
         <+> eval cond
         <+> eval_alt then_body else_body)
    | ConditionalGoto(_, _) | Goto(_) | Label(_) ->
        failwith "Internal error -> remove goto statements before running absint."

and eval_alt body1 body2 =
  save_messages >>= fun current_messages ->
    absint_amb (int_abs body1) (int_abs body2)
    <+> restore_messages current_messages

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
        get_var var >>= fun maybe_value ->
          match maybe_value with
            | Some value ->
                (match value with
                   | Uninitialized ->
                       add_message (sprintf "Uninitialized variable '%s'." var)
                       <+> result Uninitialized
                   | Unknown ->
                       result Unknown)
            | None ->
                add_message (sprintf "Undeclared variable '%s'." var)
                <+> result Uninitialized

and chain statements =
  match statements with
    | hd :: tl ->
        int_abs hd <+> (chain tl)
    | [] ->
        result ();;

let run program =
  let absint_monad = int_abs program in
  let (env, messages), _ = run_absint absint_monad ([], []) in
    (show_env env, List.rev messages);;

(* An element on the GOTO removal transformation stack is the list of
   instructions already processed (in reverse order) and the list of
   unprocessed instructions.

   This stack is probably kin to the zipper data structures. *)
type transform_stack = program list * program list;;

let remove_goto program =
  let rec remove_goto_impl stack =
    match stack with
      | (processed, unprocessed) :: tl ->
          (match unprocessed with
             | to_process :: others ->
                 (match to_process with
                    | Seq(stmts) ->
                        let new_stack = ([], stmts) :: (processed, others) :: tl in
                        let transformed_stmts, transformed_tl = remove_goto_impl new_stack in
                        let new_seq = Seq transformed_stmts in
                        let new_top = (new_seq :: processed, others) in
                          remove_goto_impl (new_top :: transformed_tl)
                    | If(cond, then_body, else_body) ->
                        let then_stack = ([], [then_body]) :: (processed, others) :: tl in
                        let tr_then_body, tr_tl_1 = remove_goto_impl then_stack in
                        let else_stack =
                          ([], [else_body]) :: (processed, others) :: tr_tl_1
                        in
                        let tr_else_body, tr_tl_2 = remove_goto_impl else_stack in
                        let new_if = If(cond,
                                        List.hd tr_then_body,
                                        List.hd tr_else_body) in
                        let new_top = (new_if :: processed, others) in
                          remove_goto_impl (new_top :: tr_tl_2)
                    | While(cond, body) ->
                        let new_stack = ([], [body]) :: (processed, others) :: tl in
                        let tr_body, tr_tl = remove_goto_impl new_stack in
                        let new_while = While(cond, List.hd tr_body) in
                        let new_top = (new_while :: processed, others) in
                          remove_goto_impl (new_top :: tr_tl)
                    | stmt ->
                        let new_top = (stmt :: processed, others) in
                          remove_goto_impl (new_top :: tl))
             | [] ->
                 List.rev processed, tl)
      | [] ->
          [], []
  in
  let processed, final_stack = remove_goto_impl [([], [program])] in
    List.hd processed;;

let program_1 = Seq [Declare "a";
                       Declare "b";
                       Assignment("a", Number 1);
                       ConditionalGoto(Equal(Var "a", Number 1), "Gogu");
                       Assignment("b", Number 1);
                       Label "Gogu";
                       Assignment("a", Number 2)];;
