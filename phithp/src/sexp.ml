
open Utils

type sexp =
  | Nil
  | Number of int
  | String of string
  | Symbol of string
  | Pair of sexp * sexp
  | BuiltIn of (sexp -> sexp)
  | Closure of sexp * sexp * env
  | T
and env = < find_containing_frame: string -> (string, sexp) Hashtbl.t option;
  get: string -> sexp;
  set: string -> sexp -> env;
  set_local: string -> sexp -> env >;;

let make_env parent =
object(self)
  val parent = parent;
  val frame = Hashtbl.create 10;
  method find_containing_frame key =
    if Hashtbl.mem frame key
    then Some frame
    else match parent with
      | None -> None
      | Some env -> env#find_containing_frame key
  method get key =
    match self#find_containing_frame key with
      | None -> failwithf "Key '%s' not found." key
      | Some hash -> Hashtbl.find hash key
  method set key value =
    (match self#find_containing_frame key with
       | None -> Hashtbl.add frame key value
       | Some hash -> Hashtbl.add hash key value);
    self
  method set_local key value =
    Hashtbl.add frame key value;
    self
end

let is_nil sexp =
  match sexp with
    | Nil -> true
    | _ -> false

let car sexp =
  match sexp with
    | Pair(first, rest) -> first
    | _ -> Nil

let cdr sexp =
  match sexp with
    | Pair(first, rest) -> rest
    | _ -> Nil

let length sexp =
  let rec length_impl sexp count =
    if is_nil sexp then count
    else length_impl (cdr sexp) (count + 1)
  in length_impl sexp 0

let rec map func sexp =
  match sexp with
    | Nil -> Nil
    | Pair (head, rest) -> Pair (func (head), map func rest)
    | _ -> failwith "Invalid sexp in map."

let reverse sexp =
  let rec reverse_impl sexp accum =
    match sexp with
      | Nil -> accum
      | Pair (head, rest) -> reverse_impl rest <| Pair (head, accum)
      | _ -> failwith "Invalid sexp in reverse."
  in
    reverse_impl sexp Nil

let rec nth sexp n =
  if sexp = Nil then Nil
  else if n = 0 then car sexp
  else nth (cdr sexp) (n - 1)

let rec to_list sexp =
  match sexp with
    | Pair (car, cdr) -> car :: to_list cdr
    | Nil -> []
    | _ -> failwith "Cannot convert sexp to list."
