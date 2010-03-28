
open Utils
open Sexp
module CoreString = Core.Std.String

type formatter_input =
  | NoBreak of formatter_input list
  | AlwaysBreak of formatter_input list
  | MaybeBreak of formatter_input list
  | Str of string

type formatter_state = { max_column: int; lines: string list }

let get_indent state =
  match state.lines with
    | [] -> 0
    | h::t -> String.length h

let render state =
  state.lines
    |> List.map CoreString.rstrip
    |> List.rev
    |> String.concat "\n"

let new_line indent state =
  let newIndentedLine = String.make indent ' ' in
    { max_column = state.max_column; lines = newIndentedLine :: state.lines }

let append str state =
  match state.lines with
    | [] -> { max_column = state.max_column; lines = [str] }
    | h :: t -> { max_column = state.max_column; lines = (h ^ str) :: t }

let flip f a b = f b a

let rec format_impl input state =
  match input with
    | NoBreak inpList ->
        List.fold_left (flip format_impl) state inpList
    | Str str ->
        append str state
    | AlwaysBreak inpList ->
        format_always_break inpList state
    | MaybeBreak inpList ->
        format_maybe_break inpList state

and format_maybe_break inpList state =
  let oneLiner = { max_column = -1; lines = [] } |> format_impl (NoBreak inpList) in
    match oneLiner.lines with
      | [h] ->
          if state.max_column == (-1) || (get_indent oneLiner) + (get_indent state) < state.max_column
          then append (render oneLiner) state
          else format_always_break inpList state
      | _ -> failwith "Cannot nest `AlwaysBreak` inside `MaybeBreak`."

and format_always_break inpList state =
  let indent = get_indent state in
    match inpList with
      | [] -> state
      | [h] -> format_impl h state
      | h :: t ->
          format_impl h state |> new_line indent |> format_impl (AlwaysBreak t)

let format max_column input =
  { max_column = max_column; lines = [] }
      |> format_impl input
      |> render

let format_initial_indent initial_indent max_column input =
  let result_with_indent =
    { max_column = max_column; lines = [String.make initial_indent ' '] }
      |> format_impl input
      |> render
  in
  let length = (String.length result_with_indent) - initial_indent in
    String.sub result_with_indent initial_indent length

let rec interpose elm list =
  match list with
    | [] -> []
    | [h] -> [h]
    | h :: t -> h :: elm :: (interpose elm t)

let rec decorate_for_format sexp =
  match sexp with
    | String str -> NoBreak [Str "\""; Str str; Str "\""]
    | Symbol symbol -> Str symbol
    | Number num -> Str (string_of_int num)
    | Nil -> Str "nil"
    | Pair (_, _) ->
        let rec add_no_break list =
          match list with
            | [] -> []
            | [h] -> [h]
            | h :: Str " " :: t -> NoBreak [h; Str " "] :: add_no_break t
            | _ -> failwith "add_no_break: internal error in formatter"
        in let decorated_sexp =
            sexp
            |> Sexp.to_list
            |> List.map decorate_for_format
            |> interpose (Str " ")
            |> add_no_break
        in
          NoBreak [Str "("; MaybeBreak decorated_sexp; Str ")"]
    | T -> Str "t"
    | Closure (_,_,_) | BuiltIn _ -> failwith "Cannot print closures or builtins."

