
type pos = Pos of int * int;;

type token = Token of string * pos;;

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
      (* A sum expression. *)
  | ExprBinaryOp of string * ast_with_pos * ast_with_pos
      (* Select main node . *)
  | Select of select_stmt
      (* FROM clause. *)
  | SelectFromClause of ast_with_pos list
and ast_with_pos = ast * pos
and select_stmt = { fields: ast_with_pos list;
                    from: ast_with_pos };;

exception ParseError of string * token list;;

type parser_error = PE of string * pos;;

type parser_state = ParserState of token list * parser_error list * ast_with_pos list;;
