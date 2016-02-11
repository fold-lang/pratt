
open Pratt_foundation

(* -- Literal Type -- *)

type literal =
  | Bool of bool
  | Char of char
  | Float of float
  | Int of int
  | Str of string
  | Sym of string
  | Unit
  [@@deriving show]

let show_literal = function
  | Bool  x -> fmt "%s"     (if x then "T" else "F")
  | Char  x -> fmt "'%c'"   x
  | Float x -> fmt "%f"     x
  | Int   x -> fmt "%d"     x
  | Str   x -> fmt "\"%s\"" x
  | Sym   x -> fmt "%s"     x
  | Unit    -> fmt "()"

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
  fmt "line %d, column %d" x.line x.column

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
let symbol_literal  = [%sedlex.regexp? Plus (operator_char | delimeter_char)]
let comment         = [%sedlex.regexp? "--", Star (Compl '\n')]
let white_space     = [%sedlex.regexp? Plus (' ' | '\t')]

type lexer =
  { filename                    : string;
    lexbuf                      : Sedlexing.lexbuf;
    mutable peek_cache          : token option;
    mutable line_start          : int;
    mutable line_count          : int;
    mutable group_counter       : int }

let increment_line lexer =
  lexer.line_start <- Sedlexing.lexeme_end lexer.lexbuf;
  lexer.line_count <- lexer.line_count + 1

let current_token_column lexer =
  let open Sedlexing in
  lexeme_end lexer.lexbuf - lexer.line_start - lexeme_length lexer.lexbuf + 1

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

(* FIXME: Ensure line numbers are incremented in all cases. *)
let rec read_literal ({lexbuf} as lexer) =
  match%sedlex lexbuf with
  | Plus (white_space | comment) -> read_literal lexer
  | int_literal    -> Int (int_of_string (current_lexeme lexer))
  | float_literal  -> Float (float_of_string (current_lexeme lexer))
  | '(', Star ((white_space | '\n') | comment), ')' -> Unit
  | '('  -> lexer.group_counter <- (lexer.group_counter + 1);
            Sym (current_lexeme lexer)
  | ')' -> begin
      lexer.group_counter <- (lexer.group_counter - 1);
      match lexer.group_counter <~> 0 with
      | `EQ | `GT -> Sym (current_lexeme lexer)
      | `LT -> lexer_error lexer "unbalanced parenthesis"
    end
  | '\n' -> begin
      increment_line lexer;
      match lexer.group_counter <~> 0 with
      (* EOL is treated as EOF (input termination) inside the REPL. *)
      | `EQ -> if lexer.filename = "<REPL>" then Sym "EOF" else Sym "EOL"
      | `GT -> read_literal lexer
      | `LT -> lexer_error lexer "unbalanced parenthesis"
    end
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

let next lexer =
  match lexer.peek_cache with
  | None -> read_token lexer
  | Some x ->
    lexer.peek_cache <- None;
    x

let peek lexer =
  match lexer.peek_cache with
  | None ->
    let token = read_token lexer in
    lexer.peek_cache <- Some token;
    token
  | Some x -> x

let junk t = ignore (next t)

let create_lexer name lexbuf =
  { filename      = name;
    lexbuf        = lexbuf;
    peek_cache    = None;
    line_start    = 0;
    line_count    = 1;
    group_counter = 0 }

let create_lexer_with_string name str =
  create_lexer name (Sedlexing.Utf8.from_string str)

let create_lexer_with_channel name chn =
  create_lexer name (Sedlexing.Utf8.from_channel chn)

