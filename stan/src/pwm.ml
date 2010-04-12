
open ParserTypes;;
open Printf;;
open Utils;;

let run_parser (ParserM fn) inp = fn inp;;

let bind m f =
  ParserM (fun (stream, warnings) ->
             let apply_next (new_stream, result) warnings =
               let new_parser = f result in
                 run_parser new_parser (new_stream, warnings)
             in
               match run_parser m (stream, warnings) with
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
let result v = ParserM (fun (stream, warnings) ->
                          warnings, Some (stream, v));;

(* Add a warning, but don't terminate the parser. *)
let warning warning_message =
  ParserM (fun (stream, warnings) ->
             let Stream(last_token, tokens) = stream in
             let pos = match tokens with
               | Token(_, Pos(start, _)) :: _ -> start
               | [] -> (match last_token with
                          | Some(Token(_, Pos(_, pos))) -> pos + 1
                          | None -> 0)
             in
             let warning = Warning(warning_message, pos) in
             let new_warnings = warning :: warnings in
               new_warnings, Some (stream, ()));;

(* Terminate this parser. *)
let error () =
  ParserM (fun (stream, warnings) -> warnings, None);;

(* Parse one item. *)
let item () =
  ParserM (fun (Stream(_, tokens), warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (Stream(Some hd, tl), hd)
               | [] -> warnings, None);;

(* Lookahead one item. Like `item`, but doesn't consume the input. *)
let lookahead () =
  ParserM (fun (stream, warnings) ->
             let Stream(_, tokens) = stream in
               match tokens with
                 | hd :: tl -> warnings, Some (stream, hd)
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

(* Will consume `str` like `consume`, or will add a warning and create
   a fake `token` with content `str`. *)
let consume_or_fake str =
  ParserM (fun (stream, warnings) ->
             let Stream(last_token, tokens) = stream in
               match tokens with
                 | Token(content, pos) as hd :: tl when content = str ->
                     warnings, Some (Stream(Some hd, tl), hd)
                 | _ ->
                     let str_len = String.length str in
                     let pos = match last_token with
                       | Some(Token(_, Pos(_, pos))) -> Pos(pos, pos + str_len - 1)
                       | None -> Pos(0, str_len - 1) in
                     let token = Token(str, pos) in
                     let (Pos(pos_start, _)) = pos in
                     let warning_message = sprintf "Expected '%s'." str in
                     let new_warning = Warning(warning_message, pos_start) in
                       (new_warning :: warnings), Some (stream, token));;

(* Parser that returns `true` if there aren't any symbols left in the
   input, `false` otherwise. *)
let eoi () =
  ParserM (fun (stream, warnings) ->
             let Stream(last_token, tokens) = stream in
               match tokens with
                 | [] -> (warnings, Some(stream, true))
                 | _ -> (warnings, Some(stream, false)));;

(* Applies parser `p` until end of input and collects results in a list. *)
let until_eoi p =
  let rec until_eoi_impl acc =
    eoi () >>= fun finished ->
      if finished
      then result (List.rev acc)
      else p >>= fun result ->
        until_eoi_impl (result :: acc)
  in
    until_eoi_impl [];;

(* Parse one token with content `content`. *)
let consume content =
  eoi () >>= fun eoi ->
    if not eoi then
      item () >>= fun token ->
        let Token(token_content, _) = token in
          if token_content = content
          then result token
          else
            let error_message =
              sprintf "Expected '%s' but got '%s'." content token_content
            in
              warning error_message <+> error ()
    else
      let error_message = sprintf "Expected '%s' but reached end of input." content in
        warning error_message <+> error ();;

