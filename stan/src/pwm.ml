
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

(* A parser that returns the next position in the stream. *)
let get_next_pos = ParserM (fun (stream, warnings) ->
                              let Stream(last_token, tokens) = stream in
                                match tokens with
                                  | Token(_, Pos(start_pos, _)) :: _ ->
                                      warnings, Some (stream, start_pos)
                                  | [] ->
                                      (match last_token with
                                         | Some(Token(_, Pos(_, end_pos))) ->
                                             warnings, Some (stream, end_pos + 1)
                                         | None ->
                                             warnings, Some (stream, 0)));;

(* A parser that returns the previous position in the stream. *)
let get_previous_pos = ParserM (fun (stream, warnings) ->
                                  let Stream(last_token, tokens) = stream in
                                    match last_token with
                                      | Some(Token(_, Pos(_, end_pos))) ->
                                          warnings, Some (stream, end_pos)
                                      | None ->
                                          warnings, Some (stream, 0));;

(* Add a warning, but don't terminate the parser. *)
let warning warning_message =
  let warning_impl pos =
    ParserM (fun (stream, warnings) ->
               let warning = Warning(warning_message, pos) in
               let new_warnings = warning :: warnings in
                 new_warnings, Some (stream, ()))
  in
    get_next_pos >>= fun pos ->
      warning_impl pos;;

(* Terminate this parser. *)
let fail =
  ParserM (fun (stream, warnings) -> warnings, None);;

(* Terminate the parser with a message. *)
let error message =
  warning message <+> fail ;;

(* Parse one item. *)
let item =
  ParserM (fun (Stream(_, tokens), warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (Stream(Some hd, tl), hd)
               | [] -> warnings, None);;

(* Lookahead one item. Like `item`, but doesn't consume the input. *)
let lookahead =
  ParserM (fun (stream, warnings) ->
             let Stream(_, tokens) = stream in
               match tokens with
                 | hd :: tl -> warnings, Some (stream, Some(hd))
                 | [] -> warnings, Some (stream, None));;

(* Parse using `p` until next token has value `next`. *)
let until p next =
  let rec until_impl acc =
    lookahead >>= function
      | Some(Token(token, _)) when token <> next ->
          (p >>= fun res -> until_impl (res :: acc))
      | _ ->
          result (List.rev acc)
  in
    until_impl [];;

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
let eoi  =
  lookahead >>= function
    | Some(Token _) -> result false
    | None -> result true;;

(* Applies parser `p` until end of input and collects results in a list. *)
let until_eoi p =
  let rec until_eoi_impl acc =
    eoi >>= fun finished ->
      if finished
      then result (List.rev acc)
      else p >>= fun result ->
        until_eoi_impl (result :: acc)
  in
    until_eoi_impl [];;

(* Parse one token with content `content`. *)
let consume content =
  lookahead >>= function
    | Some(Token(token_content, _)) when token_content = content ->
        item
    | Some(Token(token_content, _)) ->
        let error_message =
          sprintf "Expected '%s' but got '%s'." content token_content
        in
          error error_message
    | None ->
        let error_message = sprintf "Expected '%s' but reached end of input." content in
          error error_message;;

(* A parser that accumulates results of parsing elements with `elm_p`,
as long as `sep_p` succeeds on whatever is found between the
elements. The list has a minimum of one element. *)
let sep_by elm_p sep_p =
  let rec sep_by_iter acc =
    elm_p >>= fun elm ->
      (sep_p >>= fun _ -> sep_by_iter (elm :: acc)) <|>
          result (List.rev (elm :: acc))
  in
    sep_by_iter [];;
