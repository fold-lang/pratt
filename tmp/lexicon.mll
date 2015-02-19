
{

open Foundation

(* -- Literal -- *)

type literal =
    | Symbol of string
    | String of string
    | Float of float
    | Integer of int

let show_literal = function
    | Symbol  x -> format "`%s" x
    | String  x -> format "\"%s\"" x
    | Float   x -> format "%f" x
    | Integer x -> format "%d" x

(* -- Location -- *)

type location =
  { filename : string;
    line     : int;
    column   : int }

let empty_location =
	{ filename = "REPL";
	  line     = 0;
	  column   = 0 }

let show_location x =
	format "%s:%d:%d" x.filename x.line x.column


(* -- Token -- *)

type token_stream = Lexing.lexbuf

type token =
    { value    : literal;
      location : location }

let create_token value ?loc_opt () =
    { value = value;
      location = loc_opt => function
        | Some loc -> loc
        | None -> empty_location }

let start_token   = create_token (Symbol "module")  ()
let end_token     = create_token (Symbol "EOF")     ()
let newline_token = create_token (Symbol "EOL") ()

let show_token tok =
    format "%s @ %s" (show_literal  tok.value)
                     (show_location tok.location)
}


let newline_char    = ('\013'* '\010')
let blank_char      = [' ' '\009' '\012']
let identifier_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let operator_char   = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<'
                       '=' '>' '?' '@' '^' '|' '~']
let single_operator_char = ['(' ')' '`' ',' ';' '"' '\'']
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let hex_literal     = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal     = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal     = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal     = decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal   = ['0'-'9'] ['0'-'9' '_']*
                      ('.' ['0'-'9' '_']* )?
                      (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule read_token = parse
  | newline_char
      { Lexing.new_line lexbuf;
        (* newline_token *)
        read_token lexbuf }
  | blank_char +
      { read_token lexbuf }
  | int_literal as x
      { create_token (Integer (int_of_string x)) () }
  | float_literal as x
      { create_token (Float (float_of_string x)) () }
  | single_operator_char as c
      { create_token (Symbol (String.make 1 c)) () }
  | (identifier_char+ | operator_char +) as x
      { create_token (Symbol x) () }
  | _ as c
    { failwith ("Unrecognized character: " ^ (String.make 1 c)) }
  | eof
    { end_token }


