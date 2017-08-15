(*
 * Pratt 0.1
 * Copyright (c) 2017 Rizo Isrof. All rights reserved.
 *
 * Distributed under the ISC license, see LICENSE file.
 *)

module Stream : module type of Stream

(**
   Pratt is a library for simple top-down precedence parsing.

   The grammar is defined as a set of rules that are matched on tokens. Input
   is processed linearly with an iterator. The parser searches for rules that
   match the current token based on its position (prefix or infix). Rules store
   the matching token, the precedence (for infix rules) and the parser that
   builds the final Abstract Syntax Tree (AST) object.
 *)

type 'token error =
  | Unexpected     of {expected : 'token option; actual : 'token option}
  | Invalid_infix  of 'token
  | Invalid_prefix of 'token
  | Zero
(** The type of errors for tokens of type ['a]. *)

val unexpected_token : ?expected : 'token -> 'token -> 'token error
(** [unexpected_token ?expected t] is [Unexpected {actual = Some t;
    expected}]. *)

val unexpected_end : ?expected : 'token -> unit -> 'token error
(** [unexpected_end ?expected ()] is [Unexpected {actual = None; expected}]. *)

val invalid_prefix : 'token -> 'token error
(** [invalid_prefix t] is [Invalid_prefix t]. *)

val invalid_infix : 'token -> 'token error
(** [invalid_infix t] is [Invalid_infix t]. *)

val error_to_string : 'token printer -> 'token error -> string
(** [error_to_string token_pp e] is a human-readable representation of [e]. *)

val pp_error : 'token printer -> 'token error printer
(** [pp_error token_pp] is a pretty printer for values of type [e] and
    contained tokens. *)


(** {1:parser Parser} *)

type ('token, 'a) parser
(** A parser for tokens of type ['t] producing values of type ['a]. *)


(** {2:parser-monad Monad Instance} *)

val return : 'a -> ('token, 'a) parser
(** [return x] is a parser producing [x] as a value regardless of the input. *)

val (>>=) : ('token, 'a) parser -> ('a -> ('token, 'b) parser) ->
            ('token, 'b) parser
(** [p >>= f] is a parser returned by [f] after applying [f] to the result of
    [p]. *)


val error : 'token error -> ('token, 'a) parser
(** [error e] is a parser that fails with the error [e] without consuming any
    input *)

val zero : ('token, 'a) parser
(** [zero] is a parser that fails without consuming any input. *)

val (<|>) : ('token, 'a) parser -> ('token, 'a) parser -> ('token, 'a) parser
(** [p <|> q] is a choice combinator. Parser [p] is first applied, if it
    succeeds its value is returned. If [p] fails {e without consuming any
    input}, [q] is tried. *)


(** {1:combinators Combinators} *)

val default : 'a -> ('token, 'a) parser -> ('token, 'a) parser
(** [default x p] runs the parser [p] producing the default [x] value if
    it fails. *)

val combine : ('token, 'a) parser -> ('token, 'b) parser ->
              ('token, 'a * 'b) parser
(** [combine p q] first parses [p] and then [q] returning a pair with
    corresponding results. *)

val many : ('token, 'a) parser -> ('token, 'a list) parser
(** [many p] applies the parser [p] zero or more times. Returns a list of the
    returned values of [p]. *)

val some : ('token, 'a) parser -> ('token, ('a * 'a list)) parser
(** [some p] applies the parser [p] one or more times. Returns the guaranteed
    first value and a potentially empty list of values parsed by [p]. *)

val optional : ('token, 'a) parser -> ('token, unit) parser
(** [optional p] tries to optionally parse the input with parser [p] without
    returning its output. *)

val current : ('token, 'token) parser
(** [current] is the parser that produces the current token as the result. *)

val next : ('token, 'token) parser
(** [next] advances to the next token producing it as the parser result. *)

val expect : 'token -> ('token, 'token) parser
(** [expect token] checks if the current token in the input is equal to [token]
    failing if it is not. *)

val advance : ('token, unit) parser
(** [advance] advances the parser to the next token. *)

val consume : 'token -> ('token, unit) parser
(** [consume token] checks if the current token is equal to [token] and
    advances the parser to the next token, or fails if they are different. *)

val satisfy : ('token -> bool) -> ('token, 'token) parser
(** [satisfy test] is a parser that returns the current input token if it
    satisfies [test] predicate or fails otherwise. *)

val exactly : 'token -> ('token, 'token) parser
(** [exactly token] parses *exactly* [token]. *)

val any : ('token, 'token) parser
(** [any] is a parser that accepts any input token. *)

val from : 'token list -> ('token, 'token) parser
(** [from tokens] parses any token from [tokens] list. *)

val none : 'token list -> ('token, 'token) parser
(** [none tokens] parses any token *not* present in [tokens] list. *)

val range : ?compare: ('token -> 'token -> order) -> 'token -> 'token ->
            ('token, 'token) parser
(** [range ?compare s e] parses any token in the range defined by [s] and [e].
    Optionally a custom [compare] function can be supplied. *)

val choice : ('token, 'a) parser list -> ('token, 'a) parser
(** [choice ps] is a parser that tries all the parsers in [ps] until one of
    them succeeds. *)

val guard : bool -> ('token, unit) parser

val when'  : bool -> ('token, unit) parser -> ('token, unit) parser
val unless : bool -> ('token, unit) parser -> ('token, unit) parser

val many_while : ('token -> bool) -> ('token, 'a) parser -> ('token, 'a list) parser
(** [many_while test p] repeatedly runs the parser [p] while the input token
    satisfies [test]. Stops when the token fails the [test] or if the input is
    empty. *)

(** {1:grammar Grammar} *)

type ('token, 'a) grammar
(** Grammar type holding parsing rules for tokens of type ['t] and parsed
    values of type ['a]. *)

module Grammar : sig
  type ('token, 'a) t = ('token, 'a) grammar

  val has_null : 'token -> ('token, 'a) grammar -> bool
  val has_left : 'token -> ('token, 'a) grammar -> bool
end


val nud : int -> ('token, 'a) grammar -> ('token, 'a) parser
val led : int -> ('token, 'a) grammar -> 'a -> ('token, 'a) parser


(** {1:rules Rules} *)

type ('token, 'a) rule
(** The type for parsing rules for tokens of type ['t] producing results of
    type ['a]. *)

val rule : 'token -> (('token, 'a) grammar -> ('token, 'a) parser) -> ('token, 'a) rule
(** [rule t p] is a rule with parser [p] for prefix token [t]. *)

val term : (('token, 'a) grammar -> ('token, 'a) parser) -> ('token, 'a) rule
(** [term p] is a parser for literals or variables. *)

val infix : int -> 'token -> ('a -> 'a -> 'a) -> ('token, 'a) rule
(** [infix precedence token f] is a rule that parses infix occurrences of
    [token] with given [precedence] applying [f] to the {e lhs} and {e rhs}
    expressions. This rule is left-associative. *)

val infixr : int -> 'token -> ('a -> 'a -> 'a) -> ('token, 'a) rule
(** [infixr precedence token f] is like {!infix} except it is
    right-associative. *)

val prefix : 'token -> ('a -> 'a) -> ('token, 'a) rule
(** [prefix token f] is a rule that parses prefix occurrences of [token]
applying [f] to the {e rhs} expression. *)

val postfix : int -> 'token -> ('a -> 'a) -> ('token, 'a) rule
(** [postfix precedence token f] is a rule that parses postfix occurrences of
    [token] with given [precedence] applying [f] to the {e lhs} expression. *)

val between : 'token -> 'token -> ('a -> 'a) -> ('token, 'a) rule
(** [between s e f] is a rule that parses expressions occurring between the
    [s] and [e] tokens applying [f] to the expression. *)

val delimiter : 'token -> ('token, 'a) rule
(** [delimiter token] is a rule that parses a delimiter [token]. *)

val binary : ('a -> 'a -> 'a) -> ('token, 'a) grammar -> 'a -> ('token, 'a) parser
val unary : ('a -> 'a) -> ('token, 'a) grammar -> ('token, 'a) parser

val null :        'token -> (('token, 'a) grammar ->       ('token, 'a) parser) -> ('token, 'a) rule
val left : int -> 'token -> (('token, 'a) grammar -> 'a -> ('token, 'a) parser) -> ('token, 'a) rule


(** {1:parsing Parsing} *)

val grammar : ('token, 'a) rule list -> ('token, 'a) grammar
(** [grammar rules] is a grammar constructed with [rules]. *)

val parse : ?precedence: int -> ('token, 'a) grammar -> ('token, 'a) parser
(** [parse ?precedence g] is the parser for the grammar [g] starting with
    binding power [precedence]. *)

val run : ('token, 'a) parser -> 'token Stream.t -> ('a * 'token Stream.t, 'token error) result
(** [run p input] is the result of running the parser [p] with the given
    [input]. The parsed value is produced with the remaining input. *)


