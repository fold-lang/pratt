

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

  val default : 'a -> 'a t -> 'a t

  val optional : 'a t -> unit t

  val parse : 'a t -> input -> ('a, error) Result.t

  val error : error -> 'a t

  val consume : token -> unit t

  val expect : token -> token t

  val advance : unit t

  val satisfy : (token -> bool) -> token t

  val exactly : token -> token t

  val any : token t

  val one_of : token list -> token t

  val none_of : token list -> token t
end


module Make(Input : Input) = struct
  type input = Input.t
  type token = Input.item

  type error =
    | Empty
    | Unexpected_end   of { expected : token }
    | Unexpected_token of { expected : token; actual : token }
    | Failed_satisfy   of token

  let pp_error pp_token ppf = function
    | Empty -> Fmt.pf ppf "Empty"
    | Unexpected_end { expected } ->
      Fmt.pf ppf "@[<2>(Unexpected_end@ {@ expected =@ @[%a@]@ })@]" pp_token expected
    | Unexpected_token { expected; actual } ->
      Fmt.pf ppf "@[<2>(Unexpected_token@ {@ expected =@ @[%a@];@ actual =@ @[%a@]@ })@]"
        pp_token expected pp_token actual
    | Failed_satisfy token ->
      Fmt.pf ppf "@[<2>(Failed_satisfy@ @[%a@])@]" pp_token token

  module Result =
    Result.Of_error(struct type t = error end)

  module State = struct
    type t = token Iter.t
  end

  module StateT =
    StateT(State)(Result)

  module Functor =
    Functor.Of_monad(StateT)

  module Applicative =
    Applicative.Of_monad(StateT)

  include StateT
  include Functor
  include Applicative

  let parse p input =
    let iter = Input.iter input in
    match run p iter with
    | Ok (a, input') -> Ok a
    (* TODO: Return incomplete input too *)
    | Error e -> Error e

  let empty = fun _state -> Error Empty

  let (<|>) p1 p2 = fun state ->
    match p1 state with
    | Ok value -> Ok value
    | Error _  -> p2 state

  let default default p =
    p <|> pure default

  let rec many p =
    (p >>= fun x -> many p >>= fun xs -> pure (x :: xs))
    |> default []

  let combine p1 p2 =
    p1 >>= fun x ->
    p2 >>= fun y -> pure (x, y)

  let rec some p =
    combine p (many p) >>= fun (x, xs) -> pure (x :: xs)

  let optional p =
    p >> lazy (pure ()) |> default ()

  let error e =
    fun _state -> Error e

  let advance =
    get >>= fun iter ->
    match Iter.view iter with
    | Some (_, state) -> put state
    | None -> pure ()

  let expect expected =
    get >>= fun state ->
    match Iter.view state with
    | Some (actual, _) when actual = expected -> pure actual
    | Some (actual, _) -> error (Unexpected_token { expected; actual })
    | None -> error (Unexpected_end { expected })

  let consume tok =
    expect tok >> lazy advance

  let exactly x =
    expect x >>= fun x -> advance >> lazy (pure x)

  let satisfy test =
    get >>= fun iter ->
    match Iter.view iter with
    | Some (token, _) when test token -> pure token
    | Some (token, _) -> error (Failed_satisfy token)
    | None -> error Empty

  let any = satisfy (const true)

  let one_of list =
    satisfy (fun x -> List.mem x list)

  let none_of list =
    satisfy (fun x -> not (List.mem x list))
end

(* Default inputs *)

module String = Make(struct
    type t = string

    type item = char

    let iter self =
      let next (str, i) =
        try
          Some (String.get str i, (str, i + 1))
        with Invalid_argument _ ->
          None in
      Iter.Iter ((self, 0), next)
  end)

module List(Item : Type) = Make(struct
    type t = Item.t list

    type item = Item.t

    let rec iter self =
      let next s =
        match s with
        | [] -> None
        | x :: xs -> Some (x, xs) in
      Iter.Iter (self, next)
  end)

