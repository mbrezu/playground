
open ParserTypes;;
open Utils;;

let is_digit ch = ch >= '0' && ch <= '9';;
let is_letter ch = ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z';;
let is_alnum ch = is_letter ch || is_digit ch;;

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
        Token (String.sub str start_pos (final_pos - start_pos + 1) |> String.uppercase,
               Pos (start_pos, final_pos)), final_pos + 1
  in
  let parse_string_literal str str_pos =
    let rec psl_iter current_pos =
      if current_pos >= String.length str
      then (Token(String.sub str (str_pos - 1) (current_pos - str_pos + 1),
                  Pos(str_pos - 1, current_pos - 1)),
            current_pos)
      else if str.[current_pos] = '\''
        && current_pos + 1 < String.length str
        && str.[current_pos + 1] = '\''
      then psl_iter (current_pos + 2)
      else if str.[current_pos] = '\''
      then (Token(String.sub str (str_pos - 1) (current_pos - str_pos + 2),
                  Pos(str_pos - 1, current_pos)),
            current_pos + 1)
      else psl_iter (current_pos + 1)
    in
      psl_iter str_pos
  in
  let rec tokenize' str start_pos acc =
    if String.length str <= start_pos
    then List.rev acc, start_pos
    else
      let cont new_pos parsed = tokenize' str new_pos (parsed :: acc) in
      let firstChar = str.[start_pos] in
        if is_letter firstChar then
          let ident_pred ch = is_alnum ch || ch = '_' in
          let parsed, new_pos = lex_while str start_pos ident_pred in
            cont new_pos parsed
        else if is_digit firstChar then
          let parsed, new_pos = lex_while str start_pos is_digit in
            cont new_pos parsed
        else if is_ws firstChar then
          tokenize' str (start_pos + 1) acc
        else if firstChar = '\'' then
          let parsed, new_pos = parse_string_literal str (start_pos + 1) in
            cont new_pos parsed
        else
          let token = Token (String.sub str start_pos 1,
                             Pos (start_pos, start_pos)) in
            cont (start_pos + 1) token
  in
    tokenize' str start_pos [];;

