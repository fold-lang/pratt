
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

    let rec parse_loop : int -> 'a -> 'a t =
        fun rbp left -> get >>= fun {token; grammar} ->
            (* printf "Next token: %s\n" (Token.show token); *)
            let lbp, infix = grammar.infix token in
            if lbp > rbp then
                advance >> infix left >>= fun new_left ->
                    (* printf "Did parse infix expression: `%s`.\n"
                    (A.show new_left); *)
                    parse_loop rbp new_left
            else
                (* (printf "Cut on infix token: `%s` (lbp = 0x%x).\n"
                       (Token.show token) lbp; *)
                return left

    let parse_expression : int -> 'a t =
        fun rbp -> get >>= fun {token; grammar} ->
            (* printf "Next token: %s\n" (Token.show token); *)
            let prefix = grammar.prefix token in
            advance >> prefix >>= fun left ->
                (* printf "Did parse prefix expression: `%s`.\n"
                       (A.show left); *)
                parse_loop rbp left

    (* Parser entrypoint. Parses the state and produces an expression. *)
    let parse state =
        first (run (parse_expression 0x0000) state)


    (* # Helpers *)

    let infix precedence expr_builder =
        (precedence, fun lexpr ->
            parse_expression precedence >>| fun rexpr ->
                expr_builder lexpr rexpr)

    let atomic expr_builder =
        fun x -> return (expr_builder x)

    let prefix precedence expr_builder =
        parse_expression precedence >>| expr_builder
end


