
type pos = Pos of int * int

type token = Token of string * pos

let is_digit ch = ch >= '0' && ch <= '9'
let is_letter ch = ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z'
let is_alnum ch = is_letter ch || is_digit ch

let tokenize str start_pos =
  let is_ws ch = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t' in
  let lex_while str start_pos pred =
    let rec lex_while' pos =
      if String.length str <= pos then pos - 1
      else (if pred str.[pos] then lex_while' (pos + 1)
            else pos - 1)
    in
    let final_pos = lex_while' start_pos in
      if final_pos < start_pos
      then failwith "Lexer internal error."
      else
        Token (String.sub str start_pos (final_pos - start_pos + 1),
               Pos (start_pos, final_pos)), final_pos + 1
  in
  let rec tokenize' str start_pos acc =
    if String.length str <= start_pos
    then List.rev acc, start_pos
    else
      let firstChar = str.[start_pos] in
        if is_letter firstChar then
          let parsed, new_pos = lex_while str start_pos is_alnum in
            tokenize' str new_pos (parsed :: acc)
        else if is_ws firstChar then
          tokenize' str (start_pos + 1) acc
        else
          let token = Token (String.sub str start_pos 1,
                             Pos (start_pos, start_pos)) in
            tokenize' str (start_pos + 1) (token :: acc)
  in
    tokenize' str start_pos []

let combine_pos start_pos end_pos =
  let (Pos (start, _)) = start_pos in
  let (Pos (_, endp)) = end_pos in
    Pos (start, endp)

type ast =
    (* PL/SQL block, with declarations and statements. *)
  | Block of ast_with_pos list * ast_with_pos list
      (* A variable declaration, variable name and type name. *)
  | VarDecl of string * string
      (* An assignment statement. *)
  | StmtAssignment of string * ast_with_pos
      (* A numeric literal expression. *)
  | ExprNumLiteral of string
      (* An identifier used in an expression. *)
  | ExprIdentifier of string
and ast_with_pos = ast * pos

exception ParseError of string * token list

let parse tokens =

  let rec parse' tokens asts =
    match tokens with
      | [] -> List.rev asts, tokens
      | Token("BEGIN", start_pos) :: rest_tokens ->
          parse_block rest_tokens start_pos [] [] asts
      | Token("DECLARE", start_pos) :: rest_tokens ->
          parse_declare rest_tokens start_pos [] asts
      | _ -> raise (ParseError("Unknown token", tokens))

  and parse_declare tokens start_pos declarations asts =
    match tokens with
      | [] -> raise (ParseError("Expected BEGIN", tokens))
      | Token("BEGIN", _) :: rest_tokens ->
          parse_block rest_tokens start_pos (List.rev declarations) [] asts
      | _ -> parse_one_vardecl tokens start_pos declarations asts

  and parse_one_vardecl tokens start_pos declarations asts =
    match tokens with
      | Token(var_name, decl_start)
        :: Token(var_type, _)
        :: Token(";", decl_end)
        :: rest_tokens ->
          let var_declaration = VarDecl(var_name, var_type) in
          let new_ast = (var_declaration, combine_pos decl_start decl_end) in
            parse_declare rest_tokens start_pos (new_ast :: declarations) asts
      | _ -> raise (ParseError("Unknown token", tokens))

  and parse_block tokens start_pos declarations statements asts =
    match tokens with
      | [] -> failwith "Expected END"
      | Token("END", _) :: Token(";", end_pos) :: rest ->
          let block = Block(declarations, List.rev statements) in
          let new_ast = block, combine_pos start_pos end_pos in
          parse' rest (new_ast :: asts)
      | _ -> parse_assignment_statement tokens start_pos declarations statements asts

  and parse_assignment_statement tokens start_pos declarations statements asts =
    match tokens with
      | Token(var_name, var_pos)
        :: Token(":", _)
        :: Token("=", _)
        :: rest_tokens ->
          let expression, after_expr = parse_expression rest_tokens in
            match after_expr with
              | Token(";", end_pos) :: rest ->
                  let assignment = StmtAssignment(var_name, expression) in
                  let new_ast = assignment, combine_pos var_pos end_pos in
                    parse_block rest start_pos declarations (new_ast :: statements) asts
              | _ -> raise (ParseError("Expected ';'", tokens))
      | _ -> raise (ParseError("Unknown token", tokens))

  and parse_expression tokens =
    let rec check_all str pred pos =
      if pos >= String.length str then true
      else if not (pred str.[pos]) then false
      else check_all str pred (pos + 1)
    in
    let is_int str = check_all str is_digit 0 in
    match tokens with
      | Token(num, num_pos) :: after_expr when is_int num
          -> (ExprNumLiteral num, num_pos), after_expr
      | Token(ident, ident_pos) :: after_expr
        -> (ExprIdentifier ident, ident_pos), after_expr
      | _ -> raise (ParseError("Unknown token", tokens))
  in
    parse' tokens []

(* This function is a REPL helper. *)
let parseit str =
  let tokens, _ = tokenize str 0 in
  let ast, _ = parse tokens in
    ast
