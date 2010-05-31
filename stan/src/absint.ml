
open Utils;;
open Printf;;
open ParserTypes;;
open PlsqlParser;;
open PlsqlParser.Ast;;

module Types =
struct

  type ir =
    | AddFrame
    | DeleteFrame
    | Goto of string * plsql_ast_with_pos option
    | GotoIf of expression_ast_with_pos * string
    | Label of string
    | Assignment of expression_ast_with_pos * expression_ast_with_pos * plsql_ast_with_pos
    | Declare of expression_ast_with_pos * typ_with_pos

end;;
