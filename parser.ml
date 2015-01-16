
open Foundation

module type Language = sig
    type t
    val show : t -> string
    val empty : t
end

module Make (A: Language) = struct

    (* Parser Types *)

    type 'a t = state -> ('a * state)

    and state = {
        lexbuf  : Lexing.lexbuf;
        grammar : Token.t -> A.t symbol;
        symbol  : A.t symbol;
    }

    and 'a symbol = {
        tok : Token.t;
        lbp : int;
        led : ('a -> 'a t) option;
        nud : ('a -> 'a t) option;
    }

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
            let tok = Lexer.token state.lexbuf in
            let sym = state.grammar tok in
            put {state with symbol = sym}

    let consume : 'a t = advance >> get

    let rec parse_loop (rbp : int) (left : A.t) : 'a t =
        get >>= fun {symbol} ->
            match symbol.nud with
            | Some nud_parser -> advance >> nud_parser left >>= parse_loop rbp
            | None -> begin match symbol.led with
                | None -> error (format "No led for token `%s`." (Token.show symbol.tok))
                | Some led_parser ->
                    if symbol.lbp > rbp
                        then advance >> led_parser left >>= parse_loop rbp
                        else return left
            end

    let parse_expression (rbp : int) : 'a t =
        get >>= fun {symbol} ->
            match symbol.nud with
            | None -> error (format "No nud for token `%s`." (Token.show symbol.tok))
            | Some nud_parser -> advance >> nud_parser A.empty >>= parse_loop rbp

    let parse ~lexbuf ~grammar =
        let tok = Lexer.token lexbuf in
        let sym = grammar tok in
        let state = {lexbuf; grammar; symbol = sym} in
        first (run (parse_expression Precedence.start) state)
end


