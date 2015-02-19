
open Foundation

(* -- Literal Type -- *)

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
    { filename = "";
      line     = 0;
      column   = 0 }

let show_location x =
    format "%s:%d:%d" x.filename x.line x.column

(* -- Token -- *)

type token_stream = Sedlexing.lexbuf

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
let identifier_char = [%sedlex.regexp? 'A'..'Z' | 'a'..'z' | '_' | '\'' | '0'..'9']
let operator_char   = [%sedlex.regexp? Chars "!$%&*+-./:<=>?@^|~" ]
let delimeter_char  = [%sedlex.regexp? Chars "()`,;\"'"]

type lexer =
  { lexbuf     : Sedlexing.lexbuf;
    line_start : int;
    line_count : int }

let increment_line lexer =
  { lexer with line_start = Sedlexing.lexeme_end lexer.lexbuf;
               line_count = lexer.line_count + 1 }

let rec read_token ({ lexbuf } as lexer) = match%sedlex lexbuf with
    | '\n'       -> read_token (increment_line lexer)
    | '\t' | ' ' -> read_token lexer
    | int_literal ->
        begin try
          create_token (Integer (int_of_string (Sedlexing.Utf8.lexeme lexbuf))) ()
        with Failure _ ->
          raise (Failure (format "Int literal overflow: %d, %d"
                                 (fst (Sedlexing.loc lexbuf))
                                 (snd (Sedlexing.loc lexbuf))))
        end
    | float_literal ->
        create_token (Float (float_of_string (Sedlexing.Utf8.lexeme lexbuf))) ()
    | Plus identifier_char | delimeter_char ->
        create_token (Symbol (Sedlexing.Utf8.lexeme lexbuf)) ()
    | Plus operator_char ->
        create_token (Symbol (Sedlexing.Utf8.lexeme lexbuf)) ()
    | eof -> end_token
    | any ->
        raise (Failure (format "%d: %d: Illegal_character: %s"
                                 (fst (Sedlexing.loc lexbuf))
                                 (snd (Sedlexing.loc lexbuf))
                                 (Sedlexing.Utf8.lexeme lexbuf)))
    | _ -> assert false (* https://github.com/alainfrisch/sedlex/issues/16 *)

let lexer_with_string str =
  { lexbuf = Sedlexing.Utf8.from_string str;
    line_start = 0;
    line_count = 1 }

let lexer_with_channel chn =
  { lexbuf = Sedlexing.Utf8.from_channel chn;
    line_start = 0;
    line_count = 1 }

(* let currentPosition () = Token.{fileName=name; lineNumber=state.line; lineOffset = Sedlexing.lexeme_end buf-state.lineStart} in *)
