


module type Monad = sig
	type 'a t
	val return : 'a -> 'a t
	val bind : 'a t -> ('a -> 'b t) -> 'b t
end


module type Monad_state = functor (M : Monad) -> sig
	(* Return the state from the internals of the monad. *)
	val get : 's M.t

	(* Replace the state inside the monad. *)
	val put : 's -> unit M.t
end


(* Construct a state monad computation from a function.
   (The inverse of 'runState'.) *)

type ('s, 'm, 'a) state_t = State_t of { run_state_t : 's -> 'm * 'a }


type StateT s m a = StateT { runStateT :: s -> m (a,s) }



let state (module M : Monad) f : ('s, 'a M.t, 'a) state_t =
	State_t (M.return f)


type ('s, 'a) t = 's -> ('a * 's)


(* 

type State s = StateT s Identity

let state :: (Monad m)
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state f = StateT (return . f)

 *)