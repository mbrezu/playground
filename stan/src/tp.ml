
(* Parser monad playground. *)
open Printf;;

type pos = Pos of int * int;;

type warning = Warning of (string * int);;

type token = Token of (string * pos);;

type input = (token list) * (warning list);;

type 'a parser = ParserM of (input -> (warning list) * (token list * 'a) option);;

let run_parser (ParserM fn) inp =
  match fn inp with
    | warnings, Some hd -> warnings, Some hd
    | warnings, None -> warnings, None;;

let (|>) x f = f x;;

let (<|) f x = f x;;

let bind m f =
  ParserM (fun (tokens, warnings) ->
             let apply_next (new_tokens, result) warnings =
               let new_parser = f result in
                 run_parser new_parser (new_tokens, warnings)
             in
               match run_parser m (tokens, warnings) with
                 | warnings, Some hd -> apply_next hd warnings
                 | warnings, None -> warnings, None);;

let (>>=) = bind;;

let (<+>) p q =
  p >>= fun _ -> q;;

let result v = ParserM (fun (tokens, warnings) -> warnings, Some (tokens, v));;

let warning warning_message =
  ParserM (fun (tokens, warnings) ->
             let pos = match tokens with
               | Token(_, Pos(start, _)) :: _ -> start
               | [] -> 0 in
             let warning = Warning(warning_message, pos) in
             let new_warnings = warning :: warnings in
               new_warnings, Some (tokens, ()));;

let error () =
  ParserM (fun (tokens, warnings) -> warnings, None);;

(* End of parser monad with warnings. *)

let item () =
  ParserM (fun (tokens, warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (tl, hd)
               | [] -> warnings, None);;

let sat p =
  item () >>= fun token ->
    if p token then result token else error ();;

let char str = sat (fun (Token(content, _)) ->  content = str);;

let pred_first_char pred =
  fun (Token(content, _)) -> pred content.[0];;

let digit = sat <| pred_first_char (fun ch -> ch >= '0' && ch <= '9');;

let upper = sat <| pred_first_char (fun ch -> ch >= 'A' && ch <= 'Z');;

let lower = sat <| pred_first_char (fun ch -> ch >= 'a' && ch <= 'z') |> sat;;

let (<|>) p q = ParserM (fun inp ->
                           match run_parser p inp with
                             | (_, Some _) as p_results ->
                                 p_results
                             | (_, None) ->
                                 run_parser q inp);;

let letter = upper <|> lower;;

let alnum = letter <|> digit;;

let rec word () =
  let rec neWord () =
    letter >>= fun token ->
      print_string "ch\n";
      word () >>= fun tokens ->
        print_string "word\n";
        let extract (Token(content, _)) = content in
        let strs = (token :: tokens) |> List.map extract in
          result <| [Token(String.concat "" strs, Pos(0, 0))]
  in
    neWord () <|> (result [Token("", Pos(0,0))]);;

let string str =
  let rec string_iter str pos =
    if String.length str > pos then
      char str.[pos] >>= fun _ ->
        string_iter str (pos + 1)
    else result ""
  in
    string_iter str 0;;

let rec many p =
  (p >>= fun x ->
     many p >>= fun xs ->
       result (x :: xs)) <|> result []

let many1 p =
  p >>= fun x ->
    many p >>= fun xs ->
      result (x :: xs)

let nat = many1 digit;;

let int = (char "-" <+> nat) <|> nat;;

let ints = char "[" <+> int >>= fun n ->
  many (char "," <+> int) >>= fun ns ->
    char "]" <+> result (n :: ns);;

let optional p message value = p <|> (warning message <+> result value);;

let sepby1 p sep =
  p >>= fun x ->
    many (sep <+> p) >>= fun xs ->
      result (x :: xs);;

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
        else if is_ws firstChar then
          tokenize' str (start_pos + 1) acc
        else
          let token = Token (String.sub str start_pos 1,
                             Pos (start_pos, start_pos)) in
            tokenize' str (start_pos + 1) (token :: acc)
  in
    tokenize' str start_pos [];;

let tokenize_chars string =
  let rec tokenize_iter pos acc =
    if String.length string > pos
    then tokenize_iter (pos + 1) (Token(String.sub string pos 1, Pos(pos, pos)) :: acc)
    else List.rev acc
  in
    tokenize_iter 0 [];;
