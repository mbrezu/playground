
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
  let rec tokenize' str start_pos acc =
    if String.length str <= start_pos
    then List.rev acc, start_pos
    else
      let firstChar = str.[start_pos] in
        if is_letter firstChar then
          let parsed, new_pos = lex_while str start_pos is_alnum in
            tokenize' str new_pos (parsed :: acc)
        else if is_digit firstChar then
          let parsed, new_pos = lex_while str start_pos is_digit in
            tokenize' str new_pos (parsed :: acc)
        else if is_ws firstChar then
          tokenize' str (start_pos + 1) acc
        else
          let token = Token (String.sub str start_pos 1,
                             Pos (start_pos, start_pos)) in
            tokenize' str (start_pos + 1) (token :: acc)
  in
    tokenize' str start_pos [];;

