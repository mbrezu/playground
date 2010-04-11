
open ParserTypes;;

open Utils;;

let run_parser (ParserM fn) inp = fn inp;;

let bind m f =
  ParserM (fun (last_token, tokens, warnings) ->
             let apply_next (new_last_token, new_tokens, result) warnings =
               let new_parser = f result in
                 run_parser new_parser (new_last_token, new_tokens, warnings)
             in
               match run_parser m (last_token, tokens, warnings) with
                 | warnings, Some hd -> apply_next hd warnings
                 | warnings, None -> warnings, None);;

(* Syntactic sugar for bind. *)
let (>>=) = bind;;

(* Parse with `p`, ignore, then parse with `q`. *)
let (<+>) p q =
  p >>= fun _ -> q;;

(* Parse with either p and q. Only the first valid alternative is kept. *)
let (<|>) p q = ParserM (fun inp ->
                           match run_parser p inp with
                             | (_, Some _) as p_results ->
                                 p_results
                             | (_, None) ->
                                 run_parser q inp);;

(* A parser that does not consume any input, but `return`s v. *)
let result v = ParserM (fun (last_token, tokens, warnings) ->
                          warnings, Some (last_token, tokens, v));;

(* Add a warning, but don't terminate the parser. *)
let warning warning_message =
  ParserM (fun (last_token, tokens, warnings) ->
             let pos = match tokens with
               | Token(_, Pos(start, _)) :: _ -> start
               | [] -> (match last_token with
                          | Some(Token(_, Pos(_, pos))) -> pos + 1
                          | None -> 0)
             in
             let warning = Warning(warning_message, pos) in
             let new_warnings = warning :: warnings in
               new_warnings, Some (last_token, tokens, ()));;

(* Terminate this parser. *)
let error () =
  ParserM (fun (last_token, tokens, warnings) -> warnings, None);;

(* Parse one item. *)
let item () =
  ParserM (fun (last_token, tokens, warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (Some hd, tl, hd)
               | [] -> warnings, None);;

(* Lookahead one item. Like `item`, but doesn't consume the input. *)
let lookahead () =
  ParserM (fun (last_token, tokens, warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (last_token, tokens, hd)
               | [] -> warnings, None);;

(* Parse one item, if it satisfies predicate `p`. *)
let sat p =
  item () >>= fun token ->
    if p token then result token else error ();;

(* Parse zero or many items with parser `p`. *)
let rec many p =
  (p >>= fun x ->
     many p >>= fun xs ->
       result (x :: xs)) <|> result []

(* Parse at least one item with parser `p`. *)
let many1 p =
  p >>= fun x ->
    many p >>= fun xs ->
      result (x :: xs)

(* Try parser `p`; if it fails, add warning `message` and return `value`. *)
let optional p message value = p <|> (warning message <+> result value);;

(* Parse one or more `p`, separated by `sep`. *)
let sepby1 p sep =
  p >>= fun x ->
    many (sep <+> p) >>= fun xs ->
      result (x :: xs);;

(* Parse one token with content `content`. *)
let consume content =
  sat (fun (Token(token_content, _)) -> token_content = content);;

(* Parse using `p` until next token has value `next`. *)
let rec until p next =
  lookahead () >>= fun (Token(token, _)) ->
    if token <> next
    then
      p >>= fun first ->
        until p next >>= fun others ->
          result (first :: others)
    else
      result [];;

let token_content (Token(content, _)) = content;;

let token_pos (Token(_, pos)) = pos;;

let parse_semicolon () =
  ParserM (fun (last_token, tokens, warnings) ->
             match tokens with
               | Token(";", pos) as hd :: tl -> warnings, Some (Some hd, tl, hd)
               | _ ->
                   let pos = match last_token with
                     | Some(Token(_, Pos(_, pos))) -> Pos(pos, pos)
                     | None -> Pos(0, 0) in
                   let token = Token(";", pos) in
                   let (Pos(pos_start, _)) = pos in
                   let new_warning = Warning("Expected ';'.", pos_start) in
                     (new_warning :: warnings), Some (last_token, tokens, token));;
