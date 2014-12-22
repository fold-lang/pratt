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
  | digit+ as x
    { Token.Integer (int_of_string x) }
  | identifier as x
    { Token.Symbol x}
  | symbol* as x
    { Token.Symbol x}
  | _ as c
    { failwith ("Unrecognized character: " ^ (String.make 1 c)) }
  | eof
    { Token.End }

