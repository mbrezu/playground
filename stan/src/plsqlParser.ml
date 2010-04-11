
open ParserTypes;;
open Utils;;
open Pwm;;
open Lexer;;

type plsql_ast =
  | Program of plsql_ast_with_pos list
  | Block of plsql_ast_with_pos list * plsql_ast_with_pos list
  | VarDecl of string * string
  | StmtAssignment of string * plsql_ast_with_pos
  | ExprNumLiteral of string
  | ExprIdentifier of string
and plsql_ast_with_pos = plsql_ast * pos;;

let combine_pos start_token end_token =
  match start_token, end_token with
    | Token(_, Pos(start_pos, _)), Token(_, Pos(_, end_pos)) ->
        Pos(start_pos, end_pos);;

let extract_limits ast_list =
  let rec last list =
    match list with
      | [hd] -> hd
      | hd :: tl -> last tl
      | [] -> failwith "Empty list."
  in
  match ast_list with
    | hd :: tl ->
        let _, Pos(start_pos, _) = hd in
        let last = last ast_list in
        let _, Pos(_, end_pos) = last in
          Pos(start_pos, end_pos)
    | [] -> Pos(0, 0);;

let rec pick_first_valid list_option =
  match list_option with
    | Some k :: _ -> k
    | None :: tl -> pick_first_valid tl
    | [] -> failwith "No valid element."

let check_all pred str =
  let rec check_all_iter pos =
    if pos >= String.length str
    then true
    else if not (pred str.[pos])
    then false
    else check_all_iter (pos + 1)
  in
    check_all_iter 0;;

let rec parse_statement () =
  parse_block () <|> parse_assignment ()

and parse_block () =
  let parse_vardecl =
    item () >>= fun variable ->
      item () >>= fun type_name ->
        parse_semicolon () >>= fun end_token ->
          result (VarDecl(token_content variable,
                          token_content type_name),
                  combine_pos variable end_token)
  in
  let parse_begin_end declarations start_token =
    (consume "BEGIN" >>= fun start_token2 ->
       until (parse_statement ()) "END" >>= fun statements ->
       consume "END" >>= fun end1 ->
         parse_semicolon () >>= fun end2 ->
         let real_start = [start_token; Some start_token2] |> pick_first_valid in
           result (Block(declarations, statements), combine_pos real_start end2))
  in
    lookahead () >>= fun content ->
      match content with
        | Token("DECLARE", _) ->
            (consume "DECLARE" >>= fun start_token ->
               until parse_vardecl "BEGIN" >>= fun declarations ->
                 parse_begin_end declarations (Some start_token))
        | _ ->
            parse_begin_end [] None

and parse_assignment () =
  item () >>= fun var_name ->
    consume ":" <+> consume "=" <+> parse_expression () >>= fun expression ->
      parse_semicolon () >>= fun end_token ->
        result (StmtAssignment(token_content var_name, expression),
                combine_pos var_name end_token)

and parse_expression () =
  item () >>= fun expr ->
    let content = token_content expr in
      if (check_all is_digit content)
      then result (ExprNumLiteral content, combine_pos expr expr)
      else result (ExprIdentifier content, combine_pos expr expr);;

let plsql_parser =
  many <| parse_statement () >>= fun statements ->
    result <| (Program(statements), (extract_limits statements))

(* The PLSQL parser. Should be the only public function in this
module. *)
let parse tokens =
  run_parser plsql_parser (None, tokens, []);;

