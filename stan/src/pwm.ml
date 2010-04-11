
open ParserTypes;;

open Utils;;

let run_parser (ParserM fn) inp =
  match fn inp with
    | warnings, Some hd -> warnings, Some hd
    | warnings, None -> warnings, None;;

let bind m f =
  ParserM (fun (tokens, warnings) ->
             let apply_next (new_tokens, result) warnings =
               let new_parser = f result in
                 run_parser new_parser (new_tokens, warnings)
             in
               match run_parser m (tokens, warnings) with
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
let result v = ParserM (fun (tokens, warnings) -> warnings, Some (tokens, v));;

(* Add a warning, but don't terminate the parser. *)
let warning warning_message =
  ParserM (fun (tokens, warnings) ->
             let pos = match tokens with
               | Token(_, Pos(start, _)) :: _ -> start
               | [] -> 0 in
             let warning = Warning(warning_message, pos) in
             let new_warnings = warning :: warnings in
               new_warnings, Some (tokens, ()));;

(* Terminate this parser. *)
let error () =
  ParserM (fun (tokens, warnings) -> warnings, None);;

(* Parse one item. *)
let item () =
  ParserM (fun (tokens, warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (tl, hd)
               | [] -> warnings, None);;

(* Lookahead one item. Like `item`, but doesn't consume the input*)
let lookahead () =
  ParserM (fun (tokens, warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (tokens, hd)
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
