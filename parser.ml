
open Foundation

module type Language = sig
    type t
    val show : t -> string
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

    and 'a nud = 'a t
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

    let rec parse_loop (rbp : int) (left : 'a) : 'a t =
        get >>= fun {token; grammar} ->
            match grammar.led_provider token with
            | None -> error (format "No led for token `%s`." (Token.show token))
            | Some (lbp, parser) ->
                if lbp > rbp
                    then advance >> parser left >>= parse_loop rbp
                    else return left

    let parse_expression (rbp : int) : 'a t =
        get >>= fun {token; grammar} ->
            match grammar.nud_provider token with
            | None -> error (format "No nud for token `%s`." (Token.show token))
            | Some parser -> advance >> parser >>= parse_loop rbp


    let parse ~lexbuf ~grammar =
        let state = {lexbuf; grammar; token = Token.Start} in
        first (run (parse_expression Precedence.start) state)


    (* # Helpers *)

    let infix precedence expr_cons =
        (precedence,
         fun left  -> parse_expression precedence >>=
         fun right -> return (expr_cons left right))

    let prefix precedence expr_cons =
        parse_expression precedence >>=
        fun right -> return (expr_cons right)

    let atomic a = return a

    let postfix precedence expr_cons =
        (precedence, expr_cons)
end


