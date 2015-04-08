
open Foundation
open Parser
open Pratt
open Syntax
open Lexer

let atomic token =
  { token;
    lbp = 90;
    led = None;
    nud = Some (return (Atom token.value));
    scope = None }

let delimiter token =
  { token;
    lbp = 0;
    led = None;
    nud = None;
    scope = None }

let postfix lbp token =
  { token;
    lbp = lbp;
    led = Some (fun x -> return x);
    nud = None;
    scope = None }

let infix lbp token =
  { token;
    lbp;
    led = Some (fun x -> parse_expr lbp >>=
                fun y -> return (Term (token.value, [x; y])));
    nud = None;
    scope = None }

let infix_r lbp token =
  { token;
    lbp;
    led = Some (fun x -> parse_expr (lbp - 1) >>=
                fun y -> return (Term (token.value, [x; y])));
    nud = None;
    scope = None }

let prefix lbp token =
  { token;
    lbp = lbp;
    led = None;
    nud = Some (many (parse_nud lbp) >>= fun xs ->
           return (Term (token.value, xs)));
    scope = None }

let eol token =
  { token;
    lbp = 10;
    (* Try to read the next infix operator, if not defined, create a seq with
       the previous expression x and the next prefix y. *)
    led = Some (fun x -> parse_next 9 x <|>
                        (parse_expr 9 >>= fun y -> return (Term (Symbol ";", [x; y]))));
    nud = Some (parse_expr 9);
    scope = None }

let group token =
  let scope = Grammar.Scope.singleton "`)" delimiter in
  { token;
    lbp = 0;
    led = None;
    nud = Some (push_scope scope >> parse_expr 0 >>=
                fun x -> pop_scope >> consume (Symbol ")") >> return x);
    scope = Some scope }

let block token =
  let sc = Grammar.Scope.singleton "`}" delimiter in
  { token;
    lbp = 0;
    led = None;
    nud = Some (push_scope sc >> parse_expr 0 >>=
                fun x -> pop_scope >> consume (Symbol "}") >> return x);
    scope = Some sc }

let eof = delimiter

(* TODO: Support both nud and led handlers for a symbol. *)
let core_lang =
  let open Grammar.Infix in
  let core = Grammar.Scope.empty
       +> ("`EOF", eof)
       +> ("`EOL", eol)
       +> ("`*",   infix 60)
       +> ("`=",   infix 10)
       +> ("`;",   infix_r 20)
       +> ("`+",   infix 50)
       +> ("`-",   infix 50)
       +> ("`-",   prefix 70)
       +> ("`f",   prefix 70)
       +> ("`(",   group)
       +> ("`{",   block) in
  Grammar.init ~main: core ~default: atomic

