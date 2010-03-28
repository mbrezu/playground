
open Utils
open Sexp
open Formatter
open Parser

type compiler_options = { max_column: int }

type php_expression =
  | Number of int
  | String of string
  | Identifier of string
  | BinaryOperator of string * php_expression list

let rec extract_php_ast sexp =
  match sexp with
    | Sexp.Number n -> Number n
    | Sexp.String str -> String str
    | Sexp.Symbol symbol -> Identifier symbol
    | Sexp.Pair (Sexp.Symbol symbol, args) ->
        (let args_list = Sexp.to_list args in
         let binary_operator_folder operator =
           let combiner ast newArg = BinaryOperator (operator, [ast; extract_php_ast newArg]) in
             List.fold_left
               combiner
               (List.hd args_list |> extract_php_ast)
               (List.tl args_list)
         in
           match symbol with
             | "+" | "*" -> binary_operator_folder symbol
             | _ -> failwithf "Unknown function '%s'." symbol)
    | _ -> failwith "Don't know how to compile sexp."

let rec decorate_php_ast_for_format php_ast =
  match php_ast with
    | Number k -> Str (string_of_int k)
    | String str -> NoBreak [Str "\""; Str str; Str "\""]
    | Identifier id -> Str id
    | BinaryOperator (operator, _) ->
        decorate_binary_operator " " php_ast
and decorate_binary_operator outer_operator php_ast =
  match php_ast with
    | BinaryOperator (operator, terms) ->
        let decorated_terms = terms |> List.map (decorate_binary_operator operator) in
        let rec add_no_break decorated_terms =
          match decorated_terms with
            | [] -> []
            | [h] -> [h]
            | h1 :: h2 :: t ->
                NoBreak [ h1; Str " " ] :: NoBreak [ Str operator;
                                                     Str " ";
                                                     h2 ] :: add_no_break t
        in
        let result_without_parens = MaybeBreak (add_no_break decorated_terms) in
        let needs_brackets =
          match (outer_operator, operator) with
            | "*", "+" -> true
            | _ -> false
        in
          if needs_brackets
          then NoBreak [ Str "("; result_without_parens; Str ")" ]
          else result_without_parens
    | _ -> decorate_php_ast_for_format php_ast

let compile_to_ast compiler_options program =
  let sexp, _ = parse program 0 in
    extract_php_ast sexp

let generate_code_from_ast compiler_options php_ast =
  php_ast
    |> decorate_php_ast_for_format
    |> format_initial_indent (String.length "print_r (") compiler_options.max_column
    |> Printf.sprintf "<?php\nprint_r (%s);\n?>"

