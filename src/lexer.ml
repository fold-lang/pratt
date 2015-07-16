
open Foundation

(* -- Literal Type -- *)

type literal =
    | Sym of string
    | Str of string
    | Char of char
    | Float of float
    | Int of int

let show_literal = function
    | Sym   x -> fmt "`%s" (bright_white x)
    | Str   x -> fmt "\"%s\"" x
    | Float x -> fmt "%f" x
    | Int x   -> yellow (fmt "%d" x)
    | Char x  -> yellow (fmt "'%c'" x)

let string_of_literal = function
    | Sym  x  -> fmt "`%s" x
    | Str  x  -> fmt "\"%s\"" x
    | Float x -> fmt "%f" x
    | Int x   -> fmt "%d" x
    | Char x  -> fmt "'%c'" x

(* -- Location -- *)

type location =
  { line     : int;
    column   : int;
    length   : int }

let empty_location =
    { line     = 0;
      column   = 0;
      length   = 0 }

let show_location x =
    fmt "%d/%d" x.line x.column

(* -- Token -- *)
module Separation = struct
  type t =
    | Stuck    (* Stuck to the previous element *)
    | Normal   (* Separated with a space *)
    | Strong   (* Separated with \n *)
    | Explicit (* Separated with ; *)

  let to_int = function
    | Stuck    -> 0
    | Normal   -> 1
    | Strong   -> 2
    | Explicit -> 3

  let of_int = function
    | 0 -> Stuck
    | 1 -> Normal
    | 2 -> Strong
    | 3 -> Explicit
    | _ -> assert false

  let to_str = function
    | Stuck    -> "Stuck"
    | Normal   -> "Normal"
    | Strong   -> "Strong"
    | Explicit -> "Explicit"

  let max x y = of_int (max (to_int x) (to_int y))
end

type token_stream = Sedlexing.lexbuf

type token =
  { value             : literal;
    location          : location }

let show_token tok =
    fmt "%s: %s" (show_location tok.location) (show_literal tok.value)


(* -- Lexer -- *)

let decimal_literal = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_') ]
let hex_literal     = [%sedlex.regexp? '0', Chars "xX",
                          ('0'..'9' | 'A'..'F' | 'a'..'f'),
                          Star ('0'..'9' | 'A'..'F' | 'a'..'f' | '_') ]
let oct_literal     = [%sedlex.regexp? '0', Chars "oO", '0'..'7',
                          Star ('0'..'7' | '_') ]
let bin_literal     = [%sedlex.regexp? '0', Chars "bB", '0'..'1',
                          Star ('0'..'1' | '_') ]
let int_literal     = [%sedlex.regexp? decimal_literal | hex_literal |
                          oct_literal | bin_literal ]
let float_literal   = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_'),
                          Opt ('.', Star ('0'..'9' | '_')),
                          Opt (Chars "eE", Opt (Chars "+-"), '0'..'9',
                               Star ('0'..'9' | '_')) ]
let identifier_char = [%sedlex.regexp? alphabetic | Chars "_'"]
let operator_char   = [%sedlex.regexp? Chars "!$%&*+-./\\:<=>?@^|~#" ]
let delimeter_char  = [%sedlex.regexp? Chars "{}[]`,;\"'"]
let symbol_literal  = [%sedlex.regexp? operator_char | delimeter_char]
let comment         = [%sedlex.regexp? "--", Star (Compl '\n')]
let white_space     = [%sedlex.regexp? Plus (' ' | '\t')]

type lexer =
  { filename                    : string;
    lexbuf                      : Sedlexing.lexbuf;
    mutable line_start          : int;
    mutable line_count          : int;
    mutable group_counter       : int }

let increment_line lexer =
  lexer.line_start <- Sedlexing.lexeme_end lexer.lexbuf;
  lexer.line_count <- lexer.line_count + 1

let current_token_column lexer =
  Sedlexing.lexeme_end lexer.lexbuf -
    lexer.line_start - Sedlexing.lexeme_length lexer.lexbuf + 1

let current_lexeme lexer =
  Sedlexing.Utf8.lexeme lexer.lexbuf

let current_location lexer =
  { line   = lexer.line_count;
    column = current_token_column lexer;
    length = Sedlexing.lexeme_length lexer.lexbuf }

let lexer_error lexer msg =
  raise (Failure (fmt "%s: %s: '%s'."
                    (show_location (current_location lexer))
                    msg
                    (current_lexeme lexer)))

(* let rec read_separator ({ lexbuf } as lexer) current = *)
(*   match%sedlex lexbuf with *)
(*   | Plus (white_space | comment) *)
(*          -> read_separator lexer (Separation.max current Separation.Normal) *)
(*   | '\n' -> read_separator lexer (Separation.max current Separation.Strong) *)
(*   | ';'  -> read_separator lexer (Separation.max current Separation.Explicit) *)
(*   | eof  -> Separation.max current Separation.Strong *)
(*   | ""   -> current *)
(*   | _    -> assert false *)

let rec read_literal ({lexbuf} as lexer) =
  match%sedlex lexbuf with
  | Plus (white_space | comment) -> read_literal lexer
  | int_literal    -> Int (int_of_string (current_lexeme lexer))
  | float_literal  -> Float (float_of_string (current_lexeme lexer))
  | '(', Star ((white_space | '\n') | comment), ')' -> Sym "()"
  | '('            -> lexer.group_counter <- (lexer.group_counter + 1);
                      Sym (current_lexeme lexer)
  | ')'            -> lexer.group_counter <- (lexer.group_counter - 1);
                      (match lexer.group_counter <~> 0 with
                       | `EQ | `GT -> Sym (current_lexeme lexer)
                       | `LT -> lexer_error lexer "unbalanced parenthesis")
  | '\n'           -> (match lexer.group_counter <~> 0 with
                       | `EQ -> Sym "EOL"
                       | `GT -> read_literal lexer
                       | `LT -> lexer_error lexer "unbalanced parenthesis")
  | symbol_literal -> Sym (current_lexeme lexer)
  | Plus identifier_char -> Sym (current_lexeme lexer)
  | eof            -> Sym "EOF"
  | any            -> lexer_error lexer "illegal character"
  | _              -> assert false
let read_token lexer =
  let literal = read_literal lexer in
  let location = current_location lexer in
  { value             = literal;
    location          = location }

let create_lexer name lexbuf =
  { filename      = name;
    lexbuf        = lexbuf;
    line_start    = 0;
    line_count    = 1;
    group_counter = 0 }

let create_lexer_with_string name str =
  create_lexer name (Sedlexing.Utf8.from_string str)

let create_lexer_with_channel name chn =
  create_lexer name (Sedlexing.Utf8.from_channel chn)

