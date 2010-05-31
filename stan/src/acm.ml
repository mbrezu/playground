
(* Absint Compiler Monad - in fact, another specialized state monad. *)

type ('a, 'b) compiler_monad = CompilerM of ('a -> 'a * 'b);;

let run_compiler (CompilerM fn) state = fn state;;

let bind m f =
  CompilerM (fun s ->
             let new_state, result = run_compiler m s in
             let new_m = f result in
               run_compiler new_m new_state);;

(* Syntactic sugar for bind. *)
let (>>=) = bind;;

let result k = CompilerM (fun s -> (s, k));;

let get_state = CompilerM (fun s -> (s, s));;

let set_state s = CompilerM (fun _ -> (s, ()));;

let add_ir ir =
  get_state >>= fun lst ->
    set_state (ir :: lst);;

let (<+>) m1 m2 = m1 >>= fun _ -> m2;;
