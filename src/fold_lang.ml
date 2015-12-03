
open Pratt
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Parser
open Pratt.Grammar

let terminal_precedence = 90
let precedence sym =
  match sym with
  (* Match atomic symbols. *)
  | Sym "EOL" ->  9
  | Sym ";" -> 20
  (* Match symbols that can start an operator. *)
  | Sym str ->
    begin match str.[0] with
    | '=' -> 10
    | '#' -> 20
    | '+' | '-' -> 30
    | '*' | '/' -> 40
    | '(' | '{' | '[' -> 80
    | _ -> 0
  end
  | _ -> 0

(* ASSIGNMENT  = 1; *)
(* CONDITIONAL = 2; *)
(* SUM         = 3; *)
(* PRODUCT     = 4; *)
(* EXPONENT    = 5; *)
(* PREFIX      = 6; *)
(* POSTFIX     = 7; *)
(* CALL        = 8; *)

(* Common Keywords *)

let if_then_else =
  let if_sym, then_sym, else_sym, end_sym =
    Sym "if", Sym "then", Sym "else", Sym "end" in
  let scope =
    Scope.(empty |> define (delimiter then_sym)
                 |> define (delimiter else_sym)
                 |> define (delimiter end_sym)) in
  rule if_sym
    ~nud:begin
      consume if_sym >>
      push_scope scope >>
      parse_prefix 0 >>= fun condition   -> consume then_sym >>
      parse_prefix 0 >>= fun consequence ->
      (consume else_sym >>
       parse_prefix 0 >>= fun alternative -> consume end_sym >>
       pop_scope >>
       return (List [Atom if_sym; condition; consequence; alternative]))
      <|> (consume end_sym >>
           pop_scope >> return (List [Atom if_sym; condition; consequence;]))
    end

let block start_sym =
  let end_sym = Sym "end" in
  let local_scope =
    Scope.(empty |> define (delimiter end_sym)) in
  rule start_sym
    ~precedence:terminal_precedence
    ~nud:begin
      consume start_sym >>
      push_scope local_scope >>
      parse_prefix 0 >>= fun exp -> begin
        let args, body =
          match exp with
          | List [Atom (Sym ";"); List args; body] -> List args, body
          | List [Atom (Sym ";"); Atom (Sym name); body] -> List [Atom (Sym name)], body
          | _ -> raise (Failure (fmt "bad %s syntax" (show_literal start_sym))) in
        (* TODO: Add cases to catch common syntax errors. *)
        pop_scope >>
        consume end_sym >>
        return (List [Atom start_sym; args; body])
      end
    end

let quote =
  let quote_sym = Sym "`" in
  rule quote_sym
    ~precedence:90
    ~led:begin fun prev_expr ->
      consume quote_sym >>
      parse_prefix 90 >>= fun next_expr ->
      let quoted_exp = List [Atom quote_sym; next_expr] in
      return (match prev_expr with
          | List xs -> List (List.append xs [quoted_exp])
          | atom    -> List [atom; quoted_exp])
    end
    ~nud:begin
      consume quote_sym >> return (List [Atom quote_sym])
    end

let core_lang =
  let open Scope in
  let main_scope =
    empty
    |> define (delimiter          (Sym "EOF"))
    |> define end_of_line

    |> define (binary_infix       (Sym "+")   30)
    |> define (binary_infix       (Sym "-")   30)
    |> define (binary_infix       (Sym "*")   40)
    |> define (binary_infix       (Sym "/")   40)
    |> define (binary_infix       (Sym "#")   20)
    |> define (binary_infix       (Sym "=")   10)

    (* TODO: Needs testing. *)
    |> define (binary_infix       (Sym ",")   15)

    |> define (binary_infix_right (Sym ";")   20)

    |> define (group (Sym "(") (Sym ")"))
    |> define (group (Sym "{") (Sym "}"))
    |> define (group (Sym "do") (Sym "end"))

    |> define (unary_postfix      (Sym "!"))

    |> define if_then_else
    |> define quote

    |> define (block (Sym "macro"))
    |> define (block (Sym "function"))
    |> define (block (Sym "module"))
    |> define (block (Sym "interface"))
  in
    grammar ~main: main_scope ~default

