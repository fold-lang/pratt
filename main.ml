
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt

let fold_logo =
(* "                             |\n"^ *)
blue ("     ▗▐▝        ▐▐      ▐▐   ") ^ ("|" ^ bright_white "  A modern pragmatic functional language.\n") ^
blue ("    ▐▐     ▗▗   ▐▐    ▗▗▐▐   ") ^ ("|" ^ bright_white "\n") ^
blue ("  ▝▝▐▐▝▝ ▐▐  ▐▐ ▐▐  ▐▐  ▐▐   ") ^ ("|" ^ bright_white "  Version 0.0.1-alpha+001 (2015-02-12)\n") ^
blue ("    ▐▐   ▐▐  ▐▐ ▐▐  ▐▐  ▐▐   ") ^ ("|  " ^ (underline (bright_white "http://fold-lang.org\n"))) ^
blue ("    ▐▐     ▝▝    ▝▝   ▝▝     ") ^ ("|" ^ bright_white "  Type ? for help.\n") ^
blue ("  ▗▐▝                      \n") ^
      "                           \n"  ^
 (italic "  \"Simplicity is prerequisite for reliability.\"\n") ^
      "      — Edsger W. Dijkstra"

let eol_symbol tok =
  { tok = tok;
    lbp = 0;
    led = Some return;
    nud = Some (parse_expr 0 >>= return) }

module Table = Map.Make(String)

let add_symbol name sym =
    Table.add name sym

let map =
    Table.empty
    |> add_symbol "`+" (infix 6)
    |> add_symbol "`*" (infix 7)
    |> add_symbol "`/" (infix 7)
    |> add_symbol "`=" (infix 1)
    |> add_symbol "`;" (infix 1)
    |> add_symbol "`-" (infix 6)
    |> add_symbol "`->" (infix 1)
    |> add_symbol "`++" (postfix 8)
    |> add_symbol "`!!" (postfix 8)
    |> add_symbol "`(" (initial 9)
    |> add_symbol "`)" (final 0)
    |> add_symbol "`atom" (atomic 9)
    |> add_symbol "`EOF" (final 0)
    |> add_symbol "`EOL" eol_symbol
    |> add_symbol "`module" (block 0)

let grammar map tok =
    let tok_id = show_literal tok.value in
    let mk_sym = if Table.mem tok_id map
        then Table.find tok_id map
        else Table.find "`atom" map in
    let sym = mk_sym tok in
    sym


let parse ~input ~grammar =
    let state  = { input; grammar; symbol = grammar (read_token input) } in
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

let rec eval env : expr -> int = function
    | Atom lit -> begin match lit with
        | Symbol x -> assert false
        | String x -> assert false
        | Float x -> assert false
        | Integer x -> x
    end
    | Term (Symbol "+", [e1; e2]) -> (eval env e1) + (eval env e2)
    | Term (Symbol "-", [e1; e2]) -> (eval env e1) - (eval env e2)
    | Term (Symbol "/", [e1; e2]) -> (eval env e1) / (eval env e2)
    | Term (Symbol "*", [e1; e2]) -> (eval env e1) * (eval env e2)
    | _ -> assert false

let loop () =
    while true do try
        print_string (blue "-> " ^ start_white);
        flush stdout;
        let input = Lexing.from_channel stdin in
        let e = parse ~input ~grammar: (grammar map) in
        print (green " = " ^ start_white ^ (string_of_int (eval (fun v -> 0) e)))
    with
        Failure msg -> print_endline @@ (bright_red " * Error" ^ ": " ^ msg);
                       flush stdout
    done

let () =
    print (end_color ^ "\n" ^ fold_logo ^ "\n");
    loop ()

