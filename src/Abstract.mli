
module type Type = sig
  type t
end



module type Monad = sig
  (** Signature for the monad abstraction. *)

  type 'a t
  (** The type of the monadic values. *)

  val pure : 'a -> 'a t
  (** [pure x] injects the value [x] into the monadic type. *)

  val ( >>= )  : 'a t -> ('a -> 'b t) -> 'b t
  val ( =<< )  : ('a -> 'b t) -> 'a t -> 'b t
  val bind     : ('a -> 'b t) -> 'a t -> 'b t
  (** Sequentially compose two actions, passing any value produced by the first
      as an argument to the second. *)

  val ( >> ) : 'a t -> 'b t Lazy.t -> 'b t
  (** [m1 >> lazy m2] sequentially composes two actions, discarding any value
      produced by [m1], like sequencing operators (such as the semicolon) in
      imperative languages. *)

  val sequence : 'a t list -> 'a list t
  (** [sequence l] evaluates each monadic action in [l] from left to right, and
      collect the results. For a version that ignores the results see
      [sequence_unit]. *)

  val join : 'a t t -> 'a t
  (** [join m] removes one level of monadic structure, projecting its bound argument into
      the outer level. *)
end


module Monad : sig
  (** Structure defining the [Base] implementation for [Monad] signature and a
      functor to build the extended signature. *)

  module type Base = sig
    (** The base implementation for the [Monad] instance used to build the
        extended version with [Monad.Make]. *)

    type 'a t
    (** The type of the monadic values. *)

    val pure : 'a -> 'a t
    (** [pure x] injects the value [x] into the monadic type. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** [bind f m] sequentially composes two actions, passing any value
        produced by [m] as an argument to [f]. *)
  end

  module Make(B : Base) : (Monad with type 'a t := 'a B.t)
  (** Functor building an instance of {!Monad} given a {!Monad.Base}
      implementation. *)
end



module type Functor = sig
  type 'a t

  val map : ('a -> 'b ) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b ) -> 'a t -> 'b t
  val ( $>) : ('a -> 'b ) -> 'a t -> 'b t
end


module Functor : sig
  module type Base = sig
    type 'a t

    val map   : ('a -> 'b ) -> 'a t -> 'b t
  end

  module Of_monad(M : Monad.Base) : (Functor with type 'a t := 'a M.t)
  (** Functor building an instance of {!Functor} signature given a
      {!Monad.Base} implementation. *)
end



module type Applicative = sig
  type 'a t

  val pure : 'a -> 'a t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( <* ) : 'a t -> 'b t -> 'a t

  val ( *> ) : 'a t -> 'b t -> 'b t

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end


module Applicative : sig
  module Base : sig
    type 'a t

    val pure : 'a -> 'a t

    val ap : ('a -> 'b) t -> 'a t -> 'b t
  end

  module Of_monad (M : Monad.Base) : (Applicative with type 'a t = 'a M.t)

  module To_functor (A : Base) : (Functor.Base with type 'a t = 'a A.t)

  module Make (B : Base) : (Applicative with type 'a t := 'a B.t)
end



module type State = sig
  type state

  include Monad

  val get : state t
  val put : state -> unit t
  val run : 'a t -> state -> 'a * state
end
