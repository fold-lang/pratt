
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt

let led_error : expr parser = error "Symbol does not take a left argument."
let nud_error : expr parser = error "Symbol does requires a left argument."

let mult_symbol =
    { lbp = 50;
      led = (fun a -> parse_expression 50 >>=
             fun b -> return (List [Atom (Symbol "*"); a; b]));
      nud = nud_error }

let grammar = function
    | {value = Symbol "*"} -> mult_symbol
    | _ -> raise (Failure "`to_do")
    
let (~>) s =
    let e = parse ~input: (Lexing.from_string s) ~grammar in
    print ("-> " ^ s);
    print (" = " ^ show_expr e)

let () =
    ~> "a"


