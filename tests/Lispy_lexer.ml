
module String = Astring.String

let identifier =
  [%sedlex.regexp? Plus (alphabetic | Chars "_'")]

let white_space =
  [%sedlex.regexp? Plus (' ' | '\t')]

module Token = struct
  type t = string

  let pp = Fmt.string
  let compare = Pervasives.compare
end

type t =
  { mutable lexbuf      : Sedlexing.lexbuf;
    mutable line_start  : int;
    mutable line_count  : int;
    mutable group_count : int }


let increment_line self =
  self.line_start <- Sedlexing.lexeme_end self.lexbuf;
  self.line_count <- self.line_count + 1


let current_lexeme self =
  Sedlexing.Utf8.lexeme self.lexbuf


let rec read self =
  let lexbuf = self.lexbuf in
  match%sedlex lexbuf with

  (* Whitespace *)
  | Plus white_space -> read self

  (* Group start *)
  | '('  ->
    self.group_count <- (self.group_count + 1);
    current_lexeme self

  (* Operators *)
  | '+' | '-' ->
    current_lexeme self

  (* Group end *)
  | ')' ->
    self.group_count <- (self.group_count - 1);

    if self.group_count < 0 then
      failwith "unbalanced parenthesis"
    else
      current_lexeme self

  (* Identifiers *)
  | identifier -> current_lexeme self

  (* Newline symbol *)
  | '\n' ->
    increment_line self;
    read self

  (* EOF symbol *)
  | eof -> raise End_of_file

  (* Everything else is illegal *)
  | any ->
    failwith "illegal character"

  (* Sedlex: the last branch must be a catch-all error case *)
  | _ -> failwith "impossible"


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

