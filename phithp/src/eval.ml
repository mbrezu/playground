
open Utils
open Sexp

type arity =
  | Exactly of int
  | Minimum of int

let check_arity arity args =
  let count = args |> Sexp.length in
    match arity with
      | Exactly m ->
          if count <> m
          then failwithf "Needed exactly %d arguments, but got %d." m count
      | Minimum m ->
          if count < m
          then failwithf "Needed at least %d arguments, but got %d." m count

let rec numeric_builtin binary_fn neutral_element args =
  match args with
    | Nil -> Number neutral_element
    | Pair (Number k, otherArgs) ->
        (match  numeric_builtin binary_fn neutral_element otherArgs with
           | Number othersSum -> Number (binary_fn k othersSum)
           | _ -> failwith "Binary operator did not return a number.")
    | _ -> failwith "Can only add numbers."

let reverse_builtin args =
  check_arity (Exactly 1) args;
  args |> Sexp.car |> Sexp.reverse

let global_env =
  let result = make_env None in
    result#set "+" <| BuiltIn (numeric_builtin (+) 0) |> ignore;
    result#set "*" <| BuiltIn (numeric_builtin ( * ) 1) |> ignore;
    result#set "reverse" <| BuiltIn reverse_builtin |> ignore;
    result

let create_closure env sexp =
  let args = sexp |> Sexp.car in
  let body = sexp |> Sexp.cdr in
    Closure (args, body, env)

let rec extend_env env arg_names argValues =
  match arg_names with
    | Nil -> env
    | Pair (namesHead, namesTail) ->
        (match namesHead with
           | Symbol symbol ->
               (match argValues with
                  | Pair(values_head, values_tail) ->
                      env#set_local symbol values_head;
                      extend_env env namesTail values_tail
                  | _ -> failwithf "Missing argument value for argument '%s'." symbol)
           | _ -> failwithf "Sexp is not a symbol.")
    | _ -> failwith "Invalid argument name."

let rec eval_sexp env sexp =
  match sexp with
    | Nil | Number _ | String _ -> sexp
    | Symbol k -> env#get k
    | Pair (_, _) -> eval_list env sexp
    | _ -> failwith "Cannot eval sexp."
and eval_list env sexp =
  match Sexp.car sexp with
    | Symbol "quote" -> Sexp.nth sexp 1
    | Symbol "fn" -> create_closure env (Sexp.cdr sexp)
    | Symbol "def" ->
        let symbol_sexp = sexp |> Sexp.cdr |> Sexp.car in
        let value_sexp = sexp |> Sexp.cdr |> Sexp.cdr |> Sexp.car in
          (match symbol_sexp with
             | Symbol symbol ->
                 let value = eval_sexp env value_sexp in
                   env#set symbol value |> ignore;
                   symbol_sexp
             | _ -> failwith "sexp is not a symbol.")
    | _ ->
        let func = Sexp.car sexp |> eval_sexp env in
          match func with
            | BuiltIn fn ->
                let args = cdr sexp |> Sexp.map (eval_sexp env) in
                  fn args
            | Closure (arg_names, body, env) ->
                let arg_values = cdr sexp |> Sexp.map (eval_sexp env) in
                let new_env = make_env (Some env) in
                  extend_env env arg_names arg_values |> ignore;
                  eval_sequence new_env body
            | _ -> failwith "Invalid function sexp."
and eval_sequence env sequence =
  match sequence with
    | Pair(first, rest) ->
        let result = eval_sexp env first in
          if rest = Nil then result
          else eval_sequence env rest
    | _ -> failwith "Invalid sequence."

let eval sexp = eval_sexp global_env sexp

