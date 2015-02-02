
module type Type = sig
  type t
end



type Parser a = StateT (InputState a) Identity a


type ('x,'a) result =  store -> 'a * store

type ('a, 'e) parser = ('e input_state, 'a) state

type ('a, 'e) parser = 'e input_state -> ('a * 'e input_state)


module Parser_state = State_monad(Expr)


module Parser = functor (A : Type) -> struct

    module Parser_state = State_monad
        (struct type store = A.t input_state end)


    type symbol =
        {name : string;
         nud  : A.t parser option;
         nud  : ('x, A.t) result option;
         led  : ('a -> 'a parser) option;
         lbp  : int}

    type input_state = 
        {symbol  : A.t symbol;
         symbols : A.t symbol list}

    


    



end


module Expr = struct
    type t =
        | Sum of t * t
        | Product of t * t
        | Variable of string
        | Integer of int
end


module Expr_parser = Parser(Expr)


type 'a symbol =
    {name : string;
     nud  : 'a parser option;
     led  : ('a -> 'a parser) option;
     lbp  : int}

and 'a input_state =
    {symbol  : 'a symbol;
     symbols : 'a symbol list}

and 'a parser = (('a input_state), 'a) state


type token =
    {name  : string;
     value : string}


let tokens =
    [{name = "int", value = "2"};
     {name = "op",  value = "+"};
     {name = "int", value = "3"};
     {name = "op",  value = "*"};
     {name = "var", value = "x"}]


let create_operator : string -> expr symbol =
    function
    | "+" -> {name = "Operator +";
               lbp = 50;
               nud = None;
               led = (fun left -> expression 50 >>= fun right ->
                        return (Sum left right))}
    | "*" -> {name = "Operator *";
               lbp = 60;
               nud = None;
               led = (fun left -> expression 60 >>= fun right ->
                        return (Product left right))}
    | _ -> failwith "Unknown operator."


let create_int : string -> expr symbol =
    fun x ->
        {name = "Integer " ^ x;
          lbp = 0;
          nud = return (Integer (string_of_int x));
          led = None}


let infix : string -> int -> () = fun
    name precedence handler -> begin
        { name; precedence; parselet }
end

type 'a parselet = Prefix of token -> 'a
                  | Infix of token -> 'a -> 'a


type 'a symbol =
    { name : string;
      parselet : 'a parselet;
      precedence : int }


  {name : string;
    nud : (store -> 'a * store) option;
    led : 'a -> State.m option;
    lbp : int}


let symbol_map : (string -> expr symbol) String.Map =
    String.Map.of_list
    [("op",  create_operator);
     ("int", create_int);
     ("var", create_var)]


let create_input_state : 'a symbol list -> 'a input_state =
    fun symbols -> {symbol  = List.head symbols;
                    symbols = List.tail symbols}


let read_tokens : 'a symbol_map -> token list -> 'a input_state =
    fun symbol_map tokens ->
        let symbols = map (Map.find symbol_map) tokens in
        create_input_state symbols


let nud_error_msg = "Symbol requires left arguments."
let led_error_msg = "Symbol does not take left arguments."


let advance : (('a input_state), Identity.t) State.T.t =
    State.(get >>= fun i0 ->
        put {symbol = head i0.symbols; symbols = tail i0.symbols})


let () =
    read_tokens symbol_map tokens
