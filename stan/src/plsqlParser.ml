
open ParserTypes;;
open Utils;;
open Pwm;;
open Lexer;;
open Printf;;

let parse_semicolon () =
  consume_or_fake ";";;

let combine_ast_pos start_ast end_ast =
  match start_ast, end_ast with
    | (_, Pos(start_pos, _)), (_, Pos(_, end_pos)) ->
        Pos(start_pos, end_pos);;

let string_item =
  item >>= fun (Token(item_content, _)) ->
    result item_content;;

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

module Expression :
sig
  type expression_ast =
    | NumericLiteral of string
    | Identifier of string
    | BinaryOp of string * expression_ast_with_pos * expression_ast_with_pos
  and expression_ast_with_pos = expression_ast * pos;;

  val parse_expression : (token, expression_ast_with_pos) parser;;
end =
struct
  type expression_ast =
    | NumericLiteral of string
    | Identifier of string
    | BinaryOp of string * expression_ast_with_pos * expression_ast_with_pos
  and expression_ast_with_pos = expression_ast * pos;;

  let rec parse_identifier () =
    wrap_pos (item >>= fun (Token(content, _)) ->
                result <| Identifier(content))

  and parse_dotted_identifier () =
    parse_binary_op_left_assoc ["."] (parse_identifier ())

  and parse_number () =
    wrap_pos (item >>= fun expr ->
                let content = token_content expr in
                  if (check_all is_digit content)
                  then result (NumericLiteral content)
                  else fail)

  and parse_unary () =
    parse_number () <|> parse_dotted_identifier ()

  and parse_binary_op_left_assoc ops term_parser =
    let rec bin_op_iter left_term =
      lookahead >>= function
        | Some(Token(found_op, _)) when List.mem found_op ops ->
            (consume found_op <+> term_parser >>= fun right_term ->
               bin_op_iter (BinaryOp(found_op, left_term, right_term),
                            combine_ast_pos left_term right_term))
        | _ -> result left_term
    in
      term_parser >>= fun first_term -> bin_op_iter first_term

  and parse_expression () =
    let parse_term = parse_binary_op_left_assoc ["*"; "/"] (parse_unary ()) in
      parse_binary_op_left_assoc ["+"; "-"] parse_term;;

  let parse_expression = parse_expression ();;
end;;

open Expression;;

module Select :
sig
  type select_ast =
    | Select of select_components
    | SelectFromClause of select_ast_with_pos list
    | TableAlias of string * string
    | TableName of string
    | Column of expression_ast_with_pos
    | ColumnAlias of string * expression_ast_with_pos * string
  and select_ast_with_pos = select_ast * pos
  and select_components = { fields : select_ast_with_pos list;
                            clauses : select_ast_with_pos list };;

  val parse_select : (token, select_ast_with_pos) parser;;
end =
struct
  type select_ast =
    | Select of select_components
    | SelectFromClause of select_ast_with_pos list
    | TableAlias of string * string
    | TableName of string
    | Column of expression_ast_with_pos
    | ColumnAlias of string * expression_ast_with_pos * string
  and select_ast_with_pos = select_ast * pos
  and select_components = { fields : select_ast_with_pos list;
                            clauses : select_ast_with_pos list };;

  let rec parse_select () =
    wrap_pos (consume "SELECT" <+> parse_select_fields () >>= fun fields ->
                parse_from_clause () >>= fun from_clause ->
                  result <| Select { fields = fields; clauses = [from_clause] })

  and parse_select_fields () =
    let parse_column =
      wrap_pos (parse_expression >>= fun expr ->
                  lookahead >>= function
                    | Some(Token(tok, _)) when tok <> "FROM" && tok <> "," ->
                        let make_alias alias_connector =
                          string_item >>= fun alias ->
                            result (ColumnAlias(alias_connector, expr, alias))
                        in
                          if tok = "AS"
                          then consume "AS" <+> make_alias "AS"
                          else make_alias ""
                    | _ ->
                        result <| Column(expr))
    in
      sep_by "," parse_column

  and parse_from_clause () =
    wrap_pos (consume "FROM" >>= fun _ ->
                parse_table_list () >>= fun tables ->
                  result <| SelectFromClause (tables))

  and parse_table_expression () =
    wrap_pos (string_item >>= fun ident ->
                string_item >>= fun alias ->
                  if alias <> "," && alias <> ";"
                  then result (TableAlias(ident, alias))
                  else fail)
    <|>
        wrap_pos (string_item >>= fun ident -> result <| TableName(ident))

  and parse_table_list () =
    sep_by "," (parse_table_expression ());;

  let parse_select = parse_select ();;

end;;

open Select;;

type plsql_ast =
  | Program of plsql_ast_with_pos list
  | Block of plsql_ast_with_pos list * plsql_ast_with_pos list
  | VarDecl of string * string
  | StmtAssignment of string * expression_ast_with_pos
and plsql_ast_with_pos = plsql_ast * pos;;

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
      until "END" (parse_statement ()) >>= fun statements ->
        consume "END" <+> parse_semicolon () >>= fun _ ->
          result (Block(declarations, statements))
  in
    wrap_pos (lookahead >>= fun content ->
                match content with
                  | Some(Token("DECLARE", _)) ->
                      (consume "DECLARE" >>= fun start_token ->
                         until "BEGIN" parse_vardecl >>= fun declarations ->
                           parse_begin_end declarations)
                  | _ ->
                      parse_begin_end [])

and parse_assignment () =
  wrap_pos (item >>= fun var_name ->
              consume ":" <+> consume "=" <+> parse_expression >>= fun expression ->
                parse_semicolon () <+>
                  result (StmtAssignment(token_content var_name, expression)));;

let plsql_parser =
  until_eoi <| parse_statement () >>= fun statements ->
    result <| (Program(statements), (extract_limits statements));;

let run_parser_helper parser tokens =
  run_parser parser (Stream(None, tokens), []);;

(* The PLSQL parser. Should be the only public function in this
module. *)
let parse tokens =
  run_parser_helper plsql_parser tokens;;

let parse_expr tokens =
  run_parser_helper parse_expression tokens;;

let parse_select_helper tokens =
  run_parser_helper parse_select tokens;;
