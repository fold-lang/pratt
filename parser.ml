
open Foundation

module type Language = sig
    type t
    val show : t -> string
    val empty : t
end

module Make (A: Language) = struct
    (* Parser Types *)

    type 'a t = state -> ('a * state)

    and state =
        { lexbuf  : Lexing.lexbuf
        ; grammar : A.t grammar
        ; token   : Token.t }

    and 'a grammar =
        { led_provider : Token.t -> ('a led option)
        ; nud_provider : Token.t -> ('a nud option)}

    and 'a nud = 'a -> 'a t
    and 'a led = int * ('a -> 'a t)

    (* State Monad *)

    let return : 'a -> 'a t =
        fun x -> fun s -> (x, s)

    let bind m f =
        fun s -> let (x, s') = m s in f x s'

    let ( >>= ) = bind

    let ( >>| ) m f = m >>= fun x -> return (f x)

    let ( >> ) m x = m >>= fun _ -> x

    let run m = fun s -> (m s)

    let get = fun s -> (s, s)

    let put s = fun _ -> ((), s)

    let combine p1 p2 s = match p1 s with
        | Ok expr    -> expr
        | Error msg1 -> begin match p2 s with
            | Ok expr    -> expr
            | Error msg2 -> Error msg1
        end

    let (<|>) = combine

    (* Parsing *)

    let advance : 'a t =
        get >>= fun state ->
            put {state with token = Lexer.token state.lexbuf}

    let consume : 'a t = advance >> get

    let rec parse_loop (rbp : int) (left : A.t) : 'a t =
        get >>= fun {token; grammar} ->
            match grammar.nud_provider token with
            | Some nud_parser -> advance >> nud_parser left >>= parse_loop rbp
            | None -> begin match grammar.led_provider token with
                | None -> error (format "No led for token `%s`." (Token.show token))
                | Some (lbp, led_parser) ->
                    if lbp > rbp
                        then advance >> led_parser left >>= parse_loop rbp
                        else return left
            end

    let parse_expression (rbp : int) : 'a t =
        get >>= fun {token; grammar} ->
            match grammar.nud_provider token with
            | None -> error (format "No nud for token `%s`." (Token.show token))
            | Some nud_parser -> advance >> nud_parser A.empty >>= parse_loop rbp

    let parse ~lexbuf ~grammar =
        let state = {lexbuf; grammar; token = Lexer.token lexbuf} in
        first (run (parse_expression Precedence.start) state)
end


