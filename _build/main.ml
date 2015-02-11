
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt

let infix lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun a -> parse_expr lbp >>=
                fun b -> return (Term (tok.value, [a; b])));
    nud = None }

let prefix_parser tok =
    many (parse_one_nud 0 return) >>= fun expr_list ->
        return (Term (tok.value, expr_list))

let prefix = fun tok ->
  { tok = tok;
    lbp = 0;
    led = None;
    nud = Some (prefix_parser tok) }

let postfix lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun x -> return (Term (tok.value, [x])));
    nud = None }

let atomic = fun tok ->
  { tok = tok;
    lbp = 0;
    led = None;
    nud = Some (return (Atom tok.value)) }

let parse_literal lit =
    get >>= fun { symbol } ->
    if (lit = symbol.tok.value)
        then advance
        else error (format "error: expected %s but got %s."
                     (show_literal lit) (show_literal symbol.tok.value))

let initial = fun tok ->
  { tok = tok;
    lbp = 0;
    led = None;
    nud = Some (parse_expr 0 <<
                parse_literal (Symbol ")")) }

let __start__ = fun tok ->
  { tok = tok;
    lbp = 0;
    led = None;
    nud = Some (many (parse_expr 0) >>= fun xs ->
                  return (Term (tok.value, xs))) }

let final = fun tok ->
  { tok = tok;
    lbp = 0;
    led = Some return;
    nud = None }

let __end__ = final

module Symbol_Map = Map.Make(String)

let add_symbol name sym =
    Symbol_Map.add name sym

let map =
    Symbol_Map.empty
    |> add_symbol "`+" (infix 6)
    |> add_symbol "`*" (infix 7)
    |> add_symbol "`/" (infix 7)
    |> add_symbol "`=" (infix 1)
    |> add_symbol "`-" (infix 6)
    |> add_symbol "`!" prefix
    |> add_symbol "`f" prefix
    |> add_symbol "`g" prefix
    |> add_symbol "`h" prefix
    |> add_symbol "`++" (postfix 8)
    |> add_symbol "`(" initial
    |> add_symbol "`)" final
    |> add_symbol "(atom)" atomic
    |> add_symbol "`end" __end__

let grammar map tok =
    let tok_id = show_literal tok.value in
    let mk_sym = if Symbol_Map.mem tok_id map
        then Symbol_Map.find tok_id map
        else Symbol_Map.find "(atom)" map in
    mk_sym tok


let parse ~input ~grammar =
    let state  = { input; grammar; symbol = __start__ start_token } in
    match run (parse_expr 0) state with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)


let (~>) s =
    let e = parse ~input: (Lexing.from_string s) ~grammar: (grammar map) in
    print ((bright_blue "-> ") ^ s);
    print (" = " ^ show_expr e)

let (==) s e =
    let r = parse ~input: (Lexing.from_string s) ~grammar: (grammar map) in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (show_expr r) "::" (red "Expression"));
    if not y then
        (print_endline (format "\n  Expected: %s" (show_expr e));
       print_endline (format "    Actual: %s\n" (show_expr r)))
    else
        print_endline ""


let a         = Atom (Symbol "a")
let b         = Atom (Symbol "b")
let c         = Atom (Symbol "c")
let ( + ) a b = Term (Symbol "+", [a; b])
let ( ! ) a   = Term (Symbol "!", [a])
let ( ++ ) a  = Term (Symbol "++", [a])
let ( * ) a b = Term (Symbol "*", [a; b])
let f b       = Term (Symbol "f", [a])
let g a b c   = Term (Symbol "g", [a; b; c])
let h         = Term (Symbol "h", [])
let m xs      = Term (Symbol "module", xs)

let () =
    ""          == m [];
    "a"         == m [a];
    "!a"        == m [! a];
    "a++"       == m [(++) a];
    "a + a"     == m [a + a];
    "a + a * a" == m [(a + (a * a))];
    "f a"       == m [f a];
    "f a + a"   == m [(f a) + a];
    "f a + f a" == m [(f a) + (f a)];
    "g a b b"   == m [g a b b];
    "h"         == m [h];
    "(a + a)"   == m [a + a];
    "(((a)))"   == m [a];

    ~> "f !a - b + 1";


    (* FIXME
        ~> "f 2 f 3 + 1" *)
    (* ~> "(a)" *)


