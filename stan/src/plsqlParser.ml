
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

let wrap_pos_from ast p =
  let _, Pos(start_pos, _) = ast in
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

let check_sequence seq =
  lookahead_many (List.length seq) >>= function
    | Some tokens ->
        let content = List.map (fun (Token(content, _)) -> content) tokens in
          result (seq = content)
    | None ->
        result false;;

let matching_sequence seqs =
  let parse_seq seq =
    check_sequence seq >>= function
      | true ->
          (consume_many (List.length seq) >>= fun tokens ->
             let entire_op =
               String.concat "" <| List.map (fun (Token(content, _)) -> content) tokens
             in
               result entire_op)
      | false ->
          fail
  in
  let parsers = List.map (fun seq -> parse_seq seq) seqs in
    (List.fold_left (<|>) (List.hd parsers) (List.tl parsers) >>= fun str ->
       result <| Some str)
    <|> result None;;

module Expression :
sig
  type expression_ast =
    | NumericLiteral of string
    | StringLiteral of string
    | Identifier of string
    | BinaryOp of string * expression_ast_with_pos * expression_ast_with_pos
    | UnaryOp of string * expression_ast_with_pos
    | Call of expression_ast_with_pos * expression_ast_with_pos list
    | IsNull of expression_ast_with_pos
    | IsNotNull of expression_ast_with_pos
    | Like of expression_ast_with_pos * expression_ast_with_pos
  and expression_ast_with_pos = expression_ast * pos;;

  val parse_expression : (token, expression_ast_with_pos) parser;;
end =
struct
  type expression_ast =
    | NumericLiteral of string
    | StringLiteral of string
    | Identifier of string
    | BinaryOp of string * expression_ast_with_pos * expression_ast_with_pos
    | UnaryOp of string * expression_ast_with_pos
    | Call of expression_ast_with_pos * expression_ast_with_pos list
    | IsNull of expression_ast_with_pos
    | IsNotNull of expression_ast_with_pos
    | Like of expression_ast_with_pos * expression_ast_with_pos
  and expression_ast_with_pos = expression_ast * pos;;

  let rec parse_identifier () =
    wrap_pos (item >>= fun (Token(content, _)) ->
                if is_letter content.[0] || content.[0] = '_' || content = "*"
                then result <| Identifier(content)
                else warning (sprintf "Expected an identifier, but got '%s'." content) <+>
                  (result <| Identifier(content)))

  and parse_dotted_identifier () =
    parse_binary_op_left_assoc [["."]] (parse_identifier ())

  and parse_number () =
    wrap_pos (item >>= fun expr ->
                let content = token_content expr in
                  if (check_all is_digit content)
                  then result (NumericLiteral content)
                  else fail)

  and parse_parenthesis () =
    wrap_pos (consume "(" >>= fun _ ->
                parse_expression () >>= fun (expr, _) ->
                  consume ")" <+> result expr)

  and parse_dotted_identifier_or_function_call () =
    wrap_pos (parse_dotted_identifier () >>= fun (ident, ident_pos) ->
                lookahead >>= function
                  | Some(Token("(", _)) ->
                      (consume "("
                       <+> sep_by "," (parse_expression ()) >>= fun args ->
                         consume ")" <+> (result <| Call((ident, ident_pos), args)))
                  | _ ->
                      result ident)

  and parse_string () =
    wrap_pos (string_item >>= fun str -> result <| StringLiteral str)

  and parse_unary () =
    lookahead >>= function
      | Some (Token("(", _)) -> parse_parenthesis ()
      | Some (Token(num, _)) when check_all is_digit num -> parse_number ()
      | Some (Token(str, _)) when str.[0] = '\'' -> parse_string ()
      | Some (Token(id, _)) when is_letter id.[0] || id.[0] = '_' || id.[0] = '*'
          -> parse_dotted_identifier_or_function_call ()
      | _ ->
          warning "Expected an identifier, number or '('. Inserted a '_'." <+>
            get_next_pos >>= fun pos ->
              result <| (Identifier "_", Pos(pos, pos))

  and parse_binary_op_left_assoc ops term_parser =
    let rec bin_op_iter left_term =
      matching_sequence ops >>= function
        | Some found_op ->
            (term_parser >>= fun right_term ->
               bin_op_iter (BinaryOp(found_op, left_term, right_term),
                            combine_ast_pos left_term right_term))
        | None -> result left_term
    in
      term_parser >>= fun first_term -> bin_op_iter first_term

  and parse_maybe_unary unary_op term_parser =
    lookahead >>= function
      | Some(Token(content, _)) when content = unary_op ->
          wrap_pos (consume unary_op <+> term_parser >>= fun expr ->
                      result <| UnaryOp(unary_op, expr))
      | _ -> term_parser

  and parse_is_null p_result =
    wrap_pos_from p_result (lookahead_many 3 >>= function
                              | Some [Token("IS", _); Token("NULL", _); _] ->
                                  (consume_many 2) <+> (result <| IsNull(p_result) )
                              | Some [Token("IS", _); Token("NOT", _); Token("NULL", _)] ->
                                  (consume_many 3) <+> (result <| IsNotNull(p_result))
                              | _ ->
                                  let result_without_pos, _ = p_result in
                                    result result_without_pos)

  and parse_like p_result p =
    wrap_pos_from p_result (lookahead >>= function
                              | Some (Token("LIKE", _)) ->
                                  (consume "LIKE" <+> p >>= fun expr ->
                                     result <| Like(p_result, expr))
                              | _ ->
                                  let result_without_pos, _ = p_result in
                                    result result_without_pos)

  and parse_comparison p =
    p >>= fun p_result ->
      lookahead >>= function
        | Some (Token("IS", _)) ->
            parse_is_null p_result
        | Some (Token("LIKE", _)) ->
            parse_like p_result p
        | _ ->
            result p_result

  and parse_expression () =
    let parse_term = parse_binary_op_left_assoc [["*"]; ["/"]] (parse_unary ()) in
    let parse_arithmetic = parse_binary_op_left_assoc [["+"]; ["-"]] parse_term in
    let rel_ops = [["<"; "="]; [">"; "="]; ["<"; ">"]; ["<"]; [">"]; ["="]] in
    let parse_relational = parse_binary_op_left_assoc rel_ops  parse_arithmetic in
    let parse_comparison = parse_comparison parse_relational in
    let parse_logical_factor = parse_maybe_unary "NOT" parse_comparison in
    let parse_logical_term = parse_binary_op_left_assoc [["AND"]] parse_logical_factor in
      parse_binary_op_left_assoc [["OR"]] parse_logical_term;;

  let parse_expression = parse_expression ();;

end;;

open Expression;;

module Select :
sig
  type select_ast =
    | Select of select_components
    | FromClause of select_ast_with_pos list
    | WhereClause of expression_ast_with_pos
    | GroupByClause of expression_ast_with_pos list
    | OrderByClause of expression_ast_with_pos * string
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
    | FromClause of select_ast_with_pos list
    | WhereClause of expression_ast_with_pos
    | GroupByClause of expression_ast_with_pos list
    | OrderByClause of expression_ast_with_pos * string
    | TableAlias of string * string
    | TableName of string
    | Column of expression_ast_with_pos
    | ColumnAlias of string * expression_ast_with_pos * string
  and select_ast_with_pos = select_ast * pos
  and select_components = { fields : select_ast_with_pos list;
                            clauses : select_ast_with_pos list };;

  let rec parse_select () =
    let filter_maybe maybe_list =
      maybe_list |> List.map (function | Some(x) -> [x] | None -> []) |> List.concat
    in
      wrap_pos (consume "SELECT" <+> parse_select_fields () >>= fun fields ->
                  parse_from_clause () >>= fun from_clause ->
                    parse_where_clause () >>= function maybe_where_clause ->
                      parse_group_by_clause () >>= function maybe_group_by_clause ->
                        parse_order_by_clause () >>= function maybe_order_by_clause ->
                          let raw_clause_list = [Some from_clause;
                                                 maybe_where_clause;
                                                 maybe_group_by_clause;
                                                 maybe_order_by_clause] in
                          let clause_list = filter_maybe raw_clause_list in
                            result <| Select { fields = fields; clauses = clause_list })

  and parse_group_by_clause () =
    lookahead_many 2 >>= function
      | Some [Token("GROUP", _); Token("BY", _)] ->
          (wrap_pos (consume_many 2 <+> sep_by "," parse_expression >>= fun exprs ->
                       result <| GroupByClause(exprs)) >>= fun group_by_clause ->
             result <| Some group_by_clause)
      | _ -> result None

  and parse_order_by_clause () =
    lookahead_many 2 >>= function
      | Some [Token("ORDER", _); Token("BY", _)] ->
          (wrap_pos (consume_many 2 <+> parse_expression >>= fun expr ->
                       lookahead >>= function
                         | Some (Token("ASC", _)) ->
                             consume "ASC" <+> (result <| OrderByClause(expr, "ASC"))
                         | Some (Token("DESC", _)) ->
                             consume "DESC" <+> (result <| OrderByClause(expr, "DESC"))
                         | _ ->
                             result <| OrderByClause(expr, "")) >>= fun order_by_clause ->
             result <| Some order_by_clause)
      | _ -> result None

  and parse_where_clause () =
    lookahead >>= function
      | Some (Token("WHERE",_)) ->
          (wrap_pos (consume "WHERE" <+> parse_expression >>= fun expr ->
                       result <| WhereClause(expr)) >>= fun where_clause ->
             result <| Some where_clause)
      | _ -> result None

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
                  result <| FromClause (tables))

  and parse_table_expression () =
    let validate alias = alias <> "," && alias <> ";" && alias <> "WHERE" in
    wrap_pos (string_item >>= fun ident ->
                lookahead >>= function
                  | Some(Token(alias, _)) when validate alias ->
                      (string_item >>= fun alias ->
                        result (TableAlias(ident, alias)))
                  | _ ->
                      result <| TableName(ident))

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
  lookahead >>= function
    | Some (Token("BEGIN", _))
    | Some (Token("DECLARE", _)) -> parse_block ()
    | _ -> parse_assignment ()

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
