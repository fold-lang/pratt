module String = Astring.String

let format, undefined = Kernel.(format, undefined)

let decimal_int =
  [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')]

let hex_int =
  [%sedlex.regexp?
    '0', Chars "xX", ('0'..'9' | 'A'..'F' | 'a'..'f'),
    Star ('0'..'9' | 'A'..'F' | 'a'..'f' | '_')]

let oct_int =
  [%sedlex.regexp? '0', Chars "oO", '0'..'7', Star ('0'..'7' | '_') ]

let bin_int =
  [%sedlex.regexp? '0', Chars "bB", '0'..'1', Star ('0'..'1' | '_') ]

let int =
  [%sedlex.regexp? decimal_int | hex_int | oct_int | bin_int ]

let float =
  [%sedlex.regexp?
    '0'..'9', Star ('0'..'9' | '_'),
    Opt ('.', Star ('0'..'9' | '_')),
    Opt (Chars "eE", Opt (Chars "+-"), '0'..'9', Star ('0'..'9' | '_'))]

let identifier =
  [%sedlex.regexp? Plus (alphabetic | Chars "_'")]

let delimeter =
  [%sedlex.regexp? Chars "{}[].,;:=?"]

let comment =
  [%sedlex.regexp? "//", Star (Compl '\n')]

let white_space =
  [%sedlex.regexp? Plus (' ' | '\t')]

type token = [
  | `Bool of bool
  | `Char of char
  | `Float of float
  | `Int of int
  | `String of string
  | `Operator of string
  | `Delimiter of string
  | `Identifier of string
  | `Keyword of string
] [@@deriving show, ord, eq]

let is_keyword    = function `Keyword    _ -> true | _ -> false
let is_delimiter  = function `Delimiter  _ -> true | _ -> false
let is_identifier = function `Identifier _ -> true | _ -> false

let rec pp_token ppf token =
  let open Fmt in
  match token with
  | `Bool v       -> pf ppf "%b" v
  | `Char v       -> pf ppf "%c" v
  | `Float v      -> pf ppf "%f" v
  | `Int v        -> pf ppf "%d" v
  | `String v     -> pf ppf "\"%s\"" v
  | `Operator v   -> pf ppf "[operator %s]" v
  | `Delimiter v  -> pf ppf "[delimeter %s]" v
  | `Identifier v -> pf ppf "[identifier %s]" v
  | `Keyword v    -> pf ppf "[keyword %s]" v

module Token = struct
  type t = token
  let pp = pp_token
  let compare (self : token) (other : token) =
    Pervasives.compare self other
end

module Location = struct
  type t =
    { line   : int;
      column : int;
      length : int }

  let empty =
    { line   = 0;
      column = 0;
      length = 0 }

  let to_string self =
    format "%d,%d/%d" self.line self.column self.length
end

type t =
  { mutable lexbuf      : Sedlexing.lexbuf;
    mutable line_start  : int;
    mutable line_count  : int;
    mutable group_count : int }


let increment_line self =
  self.line_start <- Sedlexing.lexeme_end self.lexbuf;
  self.line_count <- self.line_count + 1


let current_location { lexbuf; line_count; line_start } =
  let open Sedlexing in
  let open Location in
  { line   = line_count;
    column = lexeme_end lexbuf - line_start - lexeme_length lexbuf + 1;
    length = lexeme_length lexbuf }


let current_lexeme self =
  Sedlexing.Utf8.lexeme self.lexbuf


exception Error of
    { lexeme   : string;
      location : Location.t;
      message  : string }


let error self message =
  let lexeme   = current_lexeme self in
  let location = current_location self in
  raise (Error { message; lexeme; location })


let rec read self =
  let lexbuf = self.lexbuf in
  match%sedlex lexbuf with

  (* Operators *)
  | "!="   -> `Operator (current_lexeme self)
  | "!=="  -> `Operator (current_lexeme self)
  | "%"    -> `Operator (current_lexeme self)
  | "%="   -> `Operator (current_lexeme self)
  | "&"    -> `Operator (current_lexeme self)
  | "&&"   -> `Operator (current_lexeme self)
  | "&="   -> `Operator (current_lexeme self)
  | "*"    -> `Operator (current_lexeme self)
  | "*="   -> `Operator (current_lexeme self)
  | "+"    -> `Operator (current_lexeme self)
  | "++"   -> `Operator (current_lexeme self)
  | "+="   -> `Operator (current_lexeme self)
  | "-"    -> `Operator (current_lexeme self)
  | "--"   -> `Operator (current_lexeme self)
  | "-="   -> `Operator (current_lexeme self)
  | "/"    -> `Operator (current_lexeme self)
  | "/="   -> `Operator (current_lexeme self)
  | "<"    -> `Operator (current_lexeme self)
  | "<<"   -> `Operator (current_lexeme self)
  | "<<="  -> `Operator (current_lexeme self)
  | "<="   -> `Operator (current_lexeme self)
  | "=="   -> `Operator (current_lexeme self)
  | "==="  -> `Operator (current_lexeme self)
  | ">"    -> `Operator (current_lexeme self)
  | ">="   -> `Operator (current_lexeme self)
  | ">>"   -> `Operator (current_lexeme self)
  | ">>="  -> `Operator (current_lexeme self)
  | ">>>"  -> `Operator (current_lexeme self)
  | ">>>=" -> `Operator (current_lexeme self)
  | "^="   -> `Operator (current_lexeme self)
  | "|"    -> `Operator (current_lexeme self)
  | "|="   -> `Operator (current_lexeme self)
  | "||"   -> `Operator (current_lexeme self)

  (* Whitespace and comment *)
  | Plus (white_space | comment) ->
    read self

  (* Int literal *)
  | int ->
    `Int (int_of_string (current_lexeme self))

  (* Float literal *)
  | float ->
    `Float (float_of_string (current_lexeme self))

  (* Group start *)
  | '('  ->
    self.group_count <- (self.group_count + 1);
    `Delimiter (current_lexeme self)

  (* Group end *)
  | ')' ->
    self.group_count <- (self.group_count - 1);

    if self.group_count < 0 then
      error self "unbalanced parenthesis"
    else
      `Delimiter (current_lexeme self)

  (* Delimiters *)
  | "=>" -> `Delimiter (current_lexeme self)
  | delimeter ->
    `Delimiter (current_lexeme self)

  (* Strings *)
  | '"',  Star (Compl '"'), '"' ->
    let lexeme = current_lexeme self in
    let s = String.(Sub.to_string (sub lexeme ~start:1 ~stop:(length lexeme - 1))) in
    `String s

  (* Chars *)
  | '\'', Compl '\'', '\'' ->
    `Char (String.get (current_lexeme self) 1)

  (* Booleans *)
  | "true"  -> `Bool true
  | "false" -> `Bool false

  (* Keywords *)
  | "function" -> `Keyword (current_lexeme self)
  | "var"      -> `Keyword (current_lexeme self)
  | "return"   -> `Keyword (current_lexeme self)
  | "for"      -> `Keyword (current_lexeme self)
  | "while"    -> `Keyword (current_lexeme self)

  (* Identifiers *)
  | identifier ->
    `Identifier (current_lexeme self)

  (* Newline symbol *)
  | '\n' ->
    increment_line self;
    read self

  (* EOF symbol *)
  | eof -> raise End_of_file

  (* Everything else is illegal *)
  | any ->
    error self "illegal character"

  (* Sedlex: the last branch must be a catch-all error case *)
  | _ -> undefined ()


let from_lexbuf lexbuf =
  { lexbuf;
    line_start  = 0;
    line_count  = 0;
    group_count = 0 }


let of_string s =
  from_lexbuf (Sedlexing.Utf8.from_string s)

let of_channel c =
  from_lexbuf (Sedlexing.Utf8.from_channel c)

let rec to_stream lexer =
  try
    let token = read lexer in
    Pratt.Stream.Yield (token, fun () -> to_stream lexer)
  with End_of_file ->
    Pratt.Stream.Empty

