
module type Type = sig
  type t
end


module type Monad = sig
  type 'a t
  val unit : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t 
end


module Identity_Monad : Monad = struct
  type 'a t = 'a
  let unit a = a
  let bind a f = f a
end


module StateT = functor (S : Type) -> functor (M : Monad) -> struct

  module Monad = struct
    type 'a t = S.t -> (S.t * 'a) M.t
    let unit a = fun s -> M.unit (s, a)
    let bind (m : 'a t) (f : 'a -> 'b t) =
      fun s -> M.bind (m s) (fun (s', a) -> f a s')
  end

  type 'a t = S.t -> (S.t * 'a) M.t

  let unit : 'a -> 'a t =
    Monad.unit 

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
    Monad.bind

  let access : S.t t =
    fun s -> M.unit (s, s)

  let zero s : unit t =
    fun _ -> M.unit (s, ())

  let update f : unit t =
    fun s -> M.unit (f s, ())

  let lift (a : 'a M.t) : 'a t =
    fun s -> M.bind a (fun x -> M.unit (s, x))
end


type 's state = StateT s Identity

