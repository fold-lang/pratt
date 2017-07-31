(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   Pratt 0.1
   Copyright (c) 2017 Rizo Isrof. All rights reserved.

   Distributed under the ISC license, see LICENSE file.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

type 't error =
  | Unexpected     of { expected : 't option; actual : 't option }
  | Invalid_infix  of 't
  | Invalid_prefix of 't
  | Empty
(** The type of errors for tokens of type ['a]. *)

val unexpected : ?expected : 't -> ?actual : 't -> unit -> 't error
(** [unexpected ~actual ~expected ()] is the [Unexpected] error. *)

val error_to_string : 't printer -> 't error -> string
(** [error_to_string token_pp e] is a human-readable representation of [e]. *)

val pp_error : 't printer -> 't error printer
(** [pp_error token_pp] is a pretty printer for values of type [e] and
    contained tokens. *)


type ('t, 'a) grammar
(** Grammar type holding parsing rules for tokens of type ['t] and parsed
    values of type ['a]. *)


(** {1:parser Parser} *)

type ('t, 'a) parser
(** A parser for tokens of type ['t] producing values of type ['a]. *)

(** {2:parser-monad Monad Instance} *)

val return : 'a -> ('t, 'a) parser
(** [return x] is a parser producing [x] as a value regardless of the input. *)

val (>>=) : ('t, 'a) parser -> ('a -> ('t, 'b) parser) -> ('t, 'b) parser
(** [p >>= f] is a parser returned by [f] after applying [f] to the result of [p]. *)


val error : 't error -> ('t, 'a) parser
(** [error e] is a parser that fails with the error [e] without consuming any
    input *)

val zero : ('t, 'a) parser
(** [zero] is a parser that fails without consuming any input. *)

val (<|>) : ('t, 'a) parser -> ('t, 'a) parser -> ('t, 'a) parser
(** [p <|> q] is a choice combinator. Parser [p] is first applied, if it
    succeeds its value is returned. If [p] fails {e without consuming any
    input}, [q] is tried. *)

val default : 'a -> ('t, 'a) parser -> ('t, 'a) parser
(** [default x p] runs the parser [p] producing the default [x] value if
    it fails. *)

val combine : ('t, 'a) parser -> ('t, 'b) parser -> ('t, 'a * 'b) parser
(** [combine p q] first parses [p] and then [q] returning a pair with
    corresponding results. *)

val many : ('t, 'a) parser -> ('t, 'a list) parser
(** [many p] applies the parser [p] zero or more times. Returns a list of the
    returned values of [p]. *)

val some : ('t, 'a) parser -> ('t, ('a * 'a list)) parser
(** [some p] applies the parser [p] one or more times. Returns the guaranteed
    first value and a potentially empty list of values parsed by [p]. *)

val optional : ('t, 'a) parser -> ('t, unit) parser
(** [optional p] tries to optionally parse the input with parser [p] without
    returning its output. *)

val current : ('t, 't) parser
(** [current] is the parser that produces the current token as the result. *)

val expect : 't -> ('t, 't) parser
(** [expect token] checks if the current token in the input is equal to [token]
    failing if it is not. *)

val advance : ('t, unit) parser
(** [advance] advances the parser to the next token. *)

val consume : 't -> ('t, unit) parser
(** [consume token] checks if the current token is equal to [token] and advances
    the parser to the next token, or fails if they are different. *)

val satisfy : ('t -> bool) -> ('t, 't) parser
(** [satisfy test] is a parser that returns the current input token if it
    satisfies [test] predicate or fails otherwise. *)

val exactly : 't -> ('t, 't) parser
(** [exactly token] parses *exactly* [token]. *)

val any : ('t, 't) parser
(** [any] is a parser that accepts any input token. *)

val from : 't list -> ('t, 't) parser
(** [from tokens] parses any token present from [tokens] list. *)

val none : 't list -> ('t, 't) parser
(** [none tokens] parses any token *not* present in [tokens] list. *)


(** {1:rules Rules} *)

type ('t, 'a) rule
(** The type for parsing rules for tokens of type ['t] producing results of
    type ['a]. *)

module Rule : sig
  type ('t, 'a) t = ('t, 'a) rule
  (** See {!rule}. *)

  val token : ('t -> 'a option) -> ('t, 'a) rule
  (** [token f] is a rule for token parsing. *)

  val infix : int -> 't -> ('a -> 'a -> 'a) -> ('t, 'a) rule
  (** [infix precedence token f] is a rule that parses infix occurrences of
      [token] with given [precedence] applying [f] to the {e lhs} and {e rhs}
      expressions. This rule is left-associative. *)

  val infixr : int -> 't -> ('a -> 'a -> 'a) -> ('t, 'a) rule
  (** [infixr precedence token f] is like {!infix} except it is
      right-associative. *)

  val prefix : 't -> ('a -> 'a) -> ('t, 'a) rule
  (** [prefix token f] is a rule that parses prefix occurrences of [token]
      applying [f] to the {e rhs} expression. *)

  val postfix : int -> 't -> ('a -> 'a) -> ('t, 'a) rule
  (** [postfix precedence token f] is a rule that parses postfix occurrences of
      [token] with given [precedence] applying [f] to the {e lhs} expression. *)

  val between : 't -> 't -> ('a -> 'a) -> ('t, 'a) rule
  (** [between s e f] is a rule that parses expressions occurring between the
      [s] and [e] tokens applying [f] to the expression. *)

  val delimiter : 't -> ('t, 'a) rule
  (** [delimiter token] is a rule that parses a delimiter [token]. *)
end

val parse : ('t, 'a) rule list -> ('t, 'a) parser
(** [parse rules] is the parser for the grammar defined by [rules]. *)

val run : ('t, 'a) parser -> 't Iter.t -> ('a, 't error) result
(** [run p input] is the result of running the parser [p] with the given
    [input].. *)

