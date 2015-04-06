
open Foundation
open Parser
open Pratt
open Syntax
open Lexer

let atomic token =
  { token;
    lbp = 1;
    led = led_error;
    nud = (consume token.value >> return (Atom token.value));
    scope = None }

let delimiter token =
  { token;
    lbp = 0;
    led = led_error;
    nud = nud_error;
    scope = None }

let postfix lbp token =
  { token;
    lbp = lbp;
    led = (fun x -> consume token.value >> return x);
    nud = nud_error;
    scope = None }

let infix lbp token =
  { token;
    lbp;
    led = (fun x -> consume token.value >> parse_expr lbp >>=
           fun y -> return (Term (token.value, [x; y])));
    nud = nud_error;
    scope = None }

let prefix lbp token =
  { token;
    lbp = lbp;
    led = led_error;
    nud = (consume token.value >> many (parse_nud lbp) >>= fun xs ->
           return (Term (token.value, xs)));
    scope = None }

let parens token =
  let scope = Grammar.Scope.singleton "`)" delimiter in
  { token;
    lbp = 0;
    led = led_error;
    nud = (consume token.value >> push_scope scope >> parse_expr 0 >>=
           fun x -> pop_scope >> consume (Symbol ")") >> return x);
    scope = Some scope }

let core_lang =
  let core = Grammar.init ~default: atomic in
  let ( $> ) g (name, handler) = Grammar.define g name handler in
  let ( <$ ) g (name, handler) = Grammar.define g name handler in
  core <$ ("`EOF", postfix 0)
       <$ ("`*", infix 60)
       <$ ("`+", infix 50)
       <$ ("`-", infix 50)
       $> ("`-", prefix 70)
       $> ("`f", prefix 70)
       $> ("`(", parens)

