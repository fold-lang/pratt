(* The lexer for the Meta language. *)

let identifier =
    ('_' | ['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* ['?']?)

let digit = ['0'-'9']

let symbol = [
  '+'
  '-'
  '*'
  '='
  '('
  ')'
  ';'
  '?'
  '@'
  '~'
  '`'
  '!'
  '#'
  '$'
  '%'
]



rule token = parse
  | [' ' '\t' '\r' '\n']
    { token lexbuf }
  | _ as c
    { failwith ("Unrecognized character: " ^ (String.make 1 c)) }
  | eof
    { Token.End }

