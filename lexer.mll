
rule token = parse
  | [' ' '\t' '\r' '\n']
    { token lexbuf }
  | ['a' - 'z'] as x
    { Token.Letter (String.make 1 x) }
  | '*' as x
    { Token.Symbol (String.make 1 x) }
  | _ as c
    { failwith ("Unrecognized character: " ^ (String.make 1 c)) }
  | eof
    { Token.End }

