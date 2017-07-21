(**
 * Structurally a parser is a function which takes an input stream of
 * characters and yields a parse tree by applying the parser logic over
 * tokens to build up a composite data structure for the AST.
 *
 * Running the function will result in traversing the token stream yielding a
 * value of type [a] that usually represents the AST for the parsed
 * expression.
*)

module type Input = sig
  type t
  type item

  val iter : t -> item Iter.t
end

module type Self = sig
  type input
  type token

  type error =
    | Empty
    | Unexpected_end   of { expected : token }
    | Unexpected_token of { expected : token; actual : token }
    | Failed_satisfy   of token

  val pp_error : token Fmt.t -> error Fmt.t

  include StateT
    with type 'a monad = ('a, error) Result.t

  include Functor
    with type 'a t := 'a t

  include Applicative
    with type 'a t := 'a t

  include Alternative
    with type 'a t := 'a t

  val combine : 'a t -> 'b t -> ('a * 'b) t
  (** [combine p1 p2] first parses [p1] and then [p2] returning a pair with
      corresponding results. *)

  val default : 'a -> 'a t -> 'a t
  (** [default default p] runs the parser [p] returning the default value in
      case it fails. *)

  val optional : 'a t -> unit t
  (** [optional p] tries to optionally parse the input with parser [p] without
      returning its output. *)

  val parse : 'a t -> input -> ('a, error) Result.t
  (** [parse p input] runs the parser [p] with input [input] producing a
      result of type [a] or an [error]. *)

  val error : error -> 'a t
  (** [error e] is a parser that always fails with error [e]. *)

  val consume : token -> unit t
  (** [advance tok] checks if the current token is equal to [tok] and advances
      the parser to the next token, or fails tokens are different. *)

  val expect : token -> token t
  (** [expect tok] checks if the current token is equal to [tok] failing if
      tokens are different. *)

  val advance : unit t
  (** [advance] advances the parser to the next token. *)

  val satisfy : (token -> bool) -> token t
  (** [satisfy test] is a parser that returns the current input token if it
      satisfies [test] predicate or fails otherwise. *)

  val exactly : token -> token t
  (** [exactly token] parses *exactly* [token]. *)

  val any : token t
  (** [any] is a parser that accepts any input token. *)

  val one_of : token list -> token t
  (** [one_of tokens] parses any token present in list [tokens]. *)

  val none_of : token list -> token t
  (** [none_of tokens] parses any token *not* present in list [tokens]. *)
end

module Make(Input : Input) : Self
  with type input := Input.t
   and type token := Input.item

module String : Self
  with type input := String.t
   and type token := char

module List(Item : Type) : Self
  with type input = Item.t list
   and type token = Item.t


