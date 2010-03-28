
(* F# forward pipe *)
let (|>) x f = f x

(* F# backward pipe *)
let (<|) f x = f x

(* F# failwithf *)
let failwithf format = Printf.ksprintf failwith format

