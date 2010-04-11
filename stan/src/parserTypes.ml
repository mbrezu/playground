
type pos = Pos of int * int;;

type warning = Warning of (string * int);;

type token = Token of (string * pos);;

type input = (token list) * (warning list);;

type 'a parser = ParserM of (input -> (input * 'a) list);;
