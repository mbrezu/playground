
type pos = Pos of int * int;;

type warning = Warning of (string * int);;

type token = Token of (string * pos);;

type 'a stream = Stream of ('a option * 'a list);;

type 'a input = 'a stream * warning list;;

type ('a, 'b) parser = ParserM of ('a input -> (warning list) * ('a stream * 'b) option);;

