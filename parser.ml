
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
        { infix  : Token.t -> 'a infix_handler
        ; prefix : Token.t -> 'a prefix_handler }

    and 'a prefix_handler = 'a t
    and 'a infix_handler  = int * ('a -> 'a t)

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

    (* Parsing *)

    let advance : 'a t =
        get >>= fun state ->
            put {state with token = Lexer.token state.lexbuf}

    let consume : 'a t = advance >> get

    let rec parse_loop (rbp : int) (left : 'a) : 'a t =
        get >>= fun {token; grammar} ->
            let lbp, infix = grammar.infix token in
            if lbp > rbp then
                advance >> infix left >>= fun new_left ->
                    parse_loop rbp new_left
            else
                return left

    let parse_expression (rbp : int) : 'a t =
        get >>= fun {token; grammar} ->
            let prefix = grammar.prefix token in
            advance >> prefix >>= fun left ->
                parse_loop rbp left

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


