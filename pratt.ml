
open Foundation
open Lexicon
open Syntax
open Parser

(*
    then return expr
    else parse_alt rbp expr
*)

let rec parse_alt rbp left =
    parse_next rbp left <|> parse_expr 0

and parse_expr rbp =
    get >>= fun { symbol } ->
        match symbol.nud with
        | Some nud ->
            trace (format "nud: + tok = %s" (show_literal symbol.tok.value));
            advance >> nud >>= fun expr ->
                trace (format "nud: > expr = %s" (show_expr expr));
                parse_alt rbp expr
        | None ->
            trace (format "nud: - tok = %s" (show_literal symbol.tok.value));
            nud_error symbol.tok

and parse_next rbp left =
    get >>= fun { symbol } ->
        match symbol.led with
        | Some led ->
            trace (format "led: + tok = %s" (show_literal symbol.tok.value));
            trace (format "led: %% symbol.lbp = %d, rbp = %d" symbol.lbp rbp);
            if symbol.lbp > rbp
                then advance >> led left >>= fun expr ->
                    trace (format "led: > expr = %s" (show_expr expr));
                    parse_alt rbp expr
                else (trace "led: !"; return left)
        | None ->
            trace (format "led: - tok = %s" (show_literal symbol.tok.value));
            led_error symbol.tok

let infix lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun a -> parse_expr lbp >>=
                fun b -> return (Term (tok.value, [a; b])));
    nud = None }

let prefix_ lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = None;
    nud = Some (many (parse_expr lbp) >>= fun expr_list ->
                    return (Term (tok.value, expr_list))) }

let postfix lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun x -> return (Term (tok.value, [x])));
    nud = None }

let atomic lbp = fun tok ->
    let atom = (Atom tok.value) in
  { tok = tok;
    lbp = lbp;
    led = Some (function Atom head -> return (Term (head, [atom]))
                       | Term (head, args) -> return (Term (head, args @ [atom])));
    nud = Some (return atom) }

let parse_literal lit =
    get >>= fun { symbol } ->
    if (lit = symbol.tok.value)
        then advance
        else error (format "error: expected %s but got %s."
                     (show_literal lit) (show_literal symbol.tok.value))

let initial lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = None;
    nud = Some (parse_expr lbp <<
                parse_literal (Symbol ")")) }

let final lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some return;
    nud = None }

let block lbp = fun tok ->
  { tok = tok;
    lbp = 0;
    led = None;
    nud = Some (many (parse_expr 0) >>= fun xs ->
                  return (Term (tok.value, xs))) }
