(* The lexer for the Meta language. *)

let identifier = ('_' | ['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* ['?']?)
let digit = ['0'-'9']

let symbol = ['+' '-' '*' '=' '(' ')']

  
rule token = parse
  | [' ' '\t' '\r' '\n']
    { token lexbuf }
  | digit+ as num
    { Token.({ kind = Number; text = num }) }
  | identifier as id
    { Token.({ kind = Symbol; text = id }) }
  | symbol* as op
    { Token.({ kind = Symbol; text = op }) }
  | _ as c
    { failwith ("Unrecognized character: " ^ (String.make 1 c)) }
  | eof
    { Token.({ kind = End; text = "" }) }

