
open Utils

open Sexp

let parse_check_eoi str pos =
  if pos >= (String.length str) then failwith "Unexpected end of input."

let rec progress_while pred str current_pos =
  if current_pos = String.length str then current_pos
  else if not (pred str current_pos) then current_pos
  else if str.[current_pos] = ')' then current_pos
  else progress_while pred str (current_pos + 1)

let is_ws ch =
  ch == ' ' || ch == '\n' || ch == '\t'

let is_digit ch =
  ch >= '0' && ch <= '9'

let rec parse str pos =
  let after_white_space_pos = progress_while (fun str pos -> is_ws <| String.get str pos) str pos
  in
    parse_check_eoi str after_white_space_pos;
    let first_char = str.[after_white_space_pos] in
      if first_char = '(' then parse_list str (after_white_space_pos + 1)
      else if first_char = '\"' then parse_string str (after_white_space_pos + 1)
      else if is_digit first_char then parse_number str after_white_space_pos
      else if first_char = '\'' then
        let quoted, endPos = parse str (after_white_space_pos + 1) in
          (Pair (Symbol "quote", Pair (quoted, Nil)), endPos)
      else parse_symbol str after_white_space_pos
and parse_list str pos =
  let after_white_space_pos = progress_while (fun str pos -> is_ws str.[pos]) str pos
  in
    parse_check_eoi str after_white_space_pos;
    if str.[after_white_space_pos] = ')' then (Nil, after_white_space_pos + 1)
    else
      let node, restPos = parse str after_white_space_pos in
      let restOfList, finalRestPos = parse_list str restPos in
        (Pair (node, restOfList), finalRestPos)
and parse_number str pos =
  let endPos = progress_while (fun str pos -> is_digit str.[pos]) str pos in
  let strNumber = String.sub str pos (endPos - pos) in
  let number = int_of_string strNumber in
    (Number number, endPos)
and parse_symbol str pos =
  let endPos = progress_while (fun str pos -> not <| is_ws str.[pos]) str pos in
  let symbol = String.sub str pos (endPos - pos) in
    if symbol = "nil" then Nil, endPos
    else if symbol = "t" then T, endPos
    else Symbol symbol, endPos
and parse_string str pos =
  let endPos = progress_while (fun str pos -> str.[pos] <> '\"') str pos in
  let string = String.sub str pos (endPos - pos) in
    String string, endPos + 1


