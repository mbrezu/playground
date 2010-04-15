
open ParserTypes;;
open Utils;;
open Pwm;;
open Lexer;;
open Printf;;

type plsql_ast =
  | Program of plsql_ast_with_pos list
  | Block of plsql_ast_with_pos list * plsql_ast_with_pos list
  | VarDecl of string * string
  | StmtAssignment of string * plsql_ast_with_pos
  | ExprNumLiteral of string
  | ExprIdentifier of string
  | ExprBinaryOp of string * plsql_ast_with_pos * plsql_ast_with_pos
  | Select of select_components
  | SelectFromClause of plsql_ast_with_pos list
  | TableAlias of string * plsql_ast_with_pos * plsql_ast_with_pos
  | ColumnAlias of string * plsql_ast_with_pos * plsql_ast_with_pos
and plsql_ast_with_pos = plsql_ast * pos
and select_components = { fields : plsql_ast_with_pos list;
                          from : plsql_ast_with_pos };;

let parse_semicolon () =
  consume_or_fake ";";;

let combine_ast_pos start_ast end_ast =
  match start_ast, end_ast with
    | (_, Pos(start_pos, _)), (_, Pos(_, end_pos)) ->
        Pos(start_pos, end_pos);;

let wrap_pos p =
  get_next_pos >>= fun start_pos ->
    p >>= fun res ->
      get_previous_pos >>= fun end_pos ->
        result (res, Pos(start_pos, end_pos));;

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
    wrap_pos (item >>= fun variable ->
                item >>= fun type_name ->
                  parse_semicolon () >>= fun _ ->
                    result (VarDecl(token_content variable,
                                    token_content type_name)))
  in
  let parse_begin_end declarations =
    consume "BEGIN" >>= fun start_token2 ->
      until (parse_statement ()) "END" >>= fun statements ->
        consume "END" <+> parse_semicolon () >>= fun _ ->
          result (Block(declarations, statements))
  in
    wrap_pos (lookahead >>= fun content ->
                match content with
                  | Some(Token("DECLARE", _)) ->
                      (consume "DECLARE" >>= fun start_token ->
                         until parse_vardecl "BEGIN" >>= fun declarations ->
                           parse_begin_end declarations)
                  | _ ->
                      parse_begin_end [])

and parse_assignment () =
  wrap_pos (item >>= fun var_name ->
              consume ":" <+> consume "=" <+> parse_expression () >>= fun expression ->
                parse_semicolon () >>= fun end_token ->
                  result (StmtAssignment(token_content var_name, expression)))

and parse_identifier () =
  wrap_pos (item >>= fun (Token(content, _)) ->
              result <| ExprIdentifier(content))

and parse_dotted_identifier () =
   parse_binary_op_left_assoc ["."] (parse_identifier ())

and parse_number () =
  wrap_pos (item >>= fun expr ->
              let content = token_content expr in
                if (check_all is_digit content)
                then result (ExprNumLiteral content)
                else fail)

and parse_unary () =
  parse_number () <|> parse_dotted_identifier ()

and parse_binary_op_left_assoc ops term_parser =
  let rec bin_op_iter left_term =
    lookahead >>= function
      | Some(Token(found_op, _)) when List.mem found_op ops ->
          (consume found_op <+> term_parser >>= fun right_term ->
             bin_op_iter (ExprBinaryOp(found_op, left_term, right_term),
                          combine_ast_pos left_term right_term))
      | _ -> result left_term
  in
    term_parser >>= fun first_term -> bin_op_iter first_term

and parse_expression () =
  let parse_term = parse_binary_op_left_assoc ["*"; "/"] (parse_unary ()) in
    parse_binary_op_left_assoc ["+"; "-"] parse_term

and parse_select () =
  wrap_pos (consume "SELECT" <+> parse_select_fields () >>= fun fields ->
              parse_from_clause () >>= fun from_clause ->
                result <| Select { fields = fields; from = from_clause })

and parse_select_fields () =
  let parse_column =
    parse_expression () >>= fun expr ->
      lookahead >>= function
        | Some(Token(tok, _)) when tok <> "FROM" && tok <> "," ->
            let make_alias alias_connector =
              parse_identifier () >>= fun alias ->
                result (ColumnAlias(alias_connector, expr, alias),
                        combine_ast_pos expr alias)
            in
              if tok = "AS"
              then consume "AS" <+> make_alias "AS"
              else make_alias ""
        | _ ->
            result expr
  in
    sep_by parse_column (consume ",")

and parse_from_clause () =
  wrap_pos (consume "FROM" >>= fun _ ->
              parse_table_list () >>= fun tables ->
                result <| SelectFromClause (tables))

and parse_table_expression () =
  parse_identifier () >>= fun ident ->
    lookahead >>= function
      | Some(Token(tok, _)) when tok <> "," ->
          (parse_identifier () >>= fun alias ->
             result (TableAlias("", ident, alias),
                     combine_ast_pos ident alias))
      | _ -> result ident

and parse_table_list () =
  sep_by (parse_table_expression ()) (consume ",")

let plsql_parser =
  until_eoi <| parse_statement () >>= fun statements ->
    result <| (Program(statements), (extract_limits statements))

let run_parser_helper parser tokens =
  run_parser parser (Stream(None, tokens), []);;

(* The PLSQL parser. Should be the only public function in this
module. *)
let parse tokens =
  run_parser_helper plsql_parser tokens;;

let parse_expr tokens =
  run_parser_helper (parse_expression ()) tokens;;

let parse_select_helper tokens =
  run_parser_helper (parse_select ()) tokens;;
