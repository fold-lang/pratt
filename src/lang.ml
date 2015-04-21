
open Foundation
open Parser
open Pratt
open Syntax
open Lexer
open Grammar

let unary_prefix sym lbp = rule sym ~lbp
  ~nud: (consume sym >>
         parse_nud >>|
         fun x -> App (Lit sym, [x]))

let unary_postfix sym =
  fun x -> return (App (sym, [x]))

let binary_infix sym lbp = rule sym ~lbp
  ~led: (fun x -> consume sym >> parse_expr lbp >>=
         fun y -> return (App (Lit sym, [x; y])))

let binary_infix_right sym lbp = rule sym ~lbp
  ~led: (fun x -> consume sym >> parse_expr (lbp - 1) >>=
         fun y -> return (App (Lit sym, [x; y])))

(* Try to read the next led operator, if not defined, create a seq with
   the previous expression x and the next prefix y. *)
let newline sym lbp = rule sym ~lbp: (lbp - 1)
  ~led: (fun x -> advance >> parse_led (lbp - 1) x <|> (parse_expr (lbp - 1) >>=
         fun y -> return (App (Lit (Sym ";"), [x; y]))))
  ~nud: (advance >> parse_expr (lbp - 1))

let delimiter sym lbp = rule sym ~lbp

let closed start_sym end_sym =
  let sc = Scope.define (delimiter end_sym 0) Scope.empty in
  rule start_sym
    ~nud: (consume start_sym >>
           push_scope sc >>
           parse_expr 0 >>=
           fun x -> pop_scope >>
           consume end_sym >>
           return (App (Lit start_sym, [x])))

(* The default rule is essential for the correct parsing of atoms and lists.
   This rule's nud will create atomic expressions with the undefined symbols.
   And the led will create lists of expressions.
   The parsing must (try) to continue reading the next led.
   The default rule has the greatest left binding power. *)
let default_with_led_appender sym = rule sym ~lbp: 90
    ~led: (fun x -> parse_nud >>=
           fun y -> return @ append_expr x y)
    ~nud: (consume sym >> return (Lit sym))


let default sym = rule sym ~lbp: 90
    ~nud: (consume sym >> many (parse_nud) >>= fun xs ->
           if List.length xs > 0
           then return (App (Lit sym, xs))
           else return (Lit sym))

(* TODO:
   - Prefix symbols must be created by juxtaposition and not explicitly.
   - Improve the parsing of recursive prefix expressions (eg: "f f x").
 *)
let core_lang =
  let main_scope : (expr, (expr, state) parser) Scope.t =
    Scope.empty
      |> Scope.define (unary_prefix       (Sym "f")   70)
      |> Scope.define (delimiter          (Sym "EOF")  0)
      |> Scope.define (newline            (Sym "EOL") 10)
      |> Scope.define (binary_infix       (Sym "+")   50)
      |> Scope.define (binary_infix       (Sym "-")   50)
      |> Scope.define (unary_prefix       (Sym "-")   70)
      |> Scope.define (binary_infix       (Sym "*")   60)
      |> Scope.define (binary_infix       (Sym "=")   10)
      |> Scope.define (binary_infix_right (Sym ";")   20)
      |> Scope.define (closed (Sym "(") (Sym ")"))
      |> Scope.define (closed (Sym "{") (Sym "}"))
  in grammar ~main: main_scope ~default

