
open ParserTypes;;

(* The State - Error monad used to drive parsing in a functional way.
*)

type ('a, 'b) sem_monad = StateErrorM of ('a -> ('a * 'b option));;

let run (StateErrorM fn) s = fn s;;

let bind m f = StateErrorM (
  fun s ->
    let new_state, result = run m s in
      match result with
        | Some k ->
            let new_monad = f k in
              run new_monad new_state
        | None -> new_state, None);;
(* Switching the returned value to (new_state, result) on the last
line of `bind` breaks the function prototype. *)

let return k = StateErrorM (fun s -> s, Some k);;

let error () = StateErrorM (fun s -> s, None);;

let get_state () = StateErrorM (fun s -> s, Some s);;

let set_state k = StateErrorM (fun _ -> k, Some ());;

let (>>=) = bind;;

