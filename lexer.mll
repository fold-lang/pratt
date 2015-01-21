
rule token = parse
  | [' ' '\t' '\r' '\n']
    { token lexbuf }
  | ['a' - 'z'] as x
    { Token.Letter (String.make 1 x) }
  | ['*' '-' '+' '#'] + as x
    { Token.Symbol x }
  | _ as c
    { failwith ("Unrecognized character: " ^ (String.make 1 c)) }
  | eof
    { Token.End }

