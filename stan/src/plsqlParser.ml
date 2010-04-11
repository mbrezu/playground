
open ParserTypes;;
open Utils;;
open Pwm;;

type plsql_ast =
  | Program of plsql_ast_with_pos list
  | Block of plsql_ast_with_pos list * plsql_ast_with_pos list
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

let rec parse_statement () =
  parse_block ()

and parse_block () =
  lookahead () >>= fun content ->
    match content with
      | Token("DECLARE", _) ->
          (consume "DECLARE" >>= fun start_token ->
             consume "BEGIN" <+> consume "END" <+> consume ";" >>= fun end_token ->
               result (Block([], []), combine_pos start_token end_token))
      | _ ->
          (consume "BEGIN" >>= fun start_token ->
             consume "END" <+> consume ";" >>= fun end_token ->
               result (Block([], []), combine_pos start_token end_token));;

let plsql_parser =
  many <| parse_statement () >>= fun statements ->
    result <| (Program(statements), (extract_limits statements))

(* The PLSQL parser. Should be the only public function in this
module. *)
let parse tokens =
  run_parser plsql_parser (tokens, []);;

