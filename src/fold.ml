
open Foundation
open Parser
open Syntax
open Lexer
open Pratt

module Table = Map.Make(String)

let add_symbol name sym =
    Table.add name sym

let eol_symbol tok = symbol tok
    ~lbp: 1
    ~led: (fun left -> return left)

let end_symbol tok = symbol tok
    ~lbp: 0
    ~led: return
    ~nud: (parse_expr 0)

let define_symbol tok = symbol tok
    ~lbp: 1
    ~led: (fun left ->
        many (parse_expr 0) >>= fun expr_list ->
             return (Term (tok.value, expr_list)))

let map =
    Table.empty
    |> add_symbol "`+" (infix 6)
    |> add_symbol "`-" (infix 6)
    |> add_symbol "`*" (infix 7)
    |> add_symbol "`/" (infix 7)
    |> add_symbol "`=" (infix 1)
    |> add_symbol "`;" (infix_r 1)
    |> add_symbol "`++" (postfix 8)
    |> add_symbol "`!" prefix
    |> add_symbol "`f" prefix
    |> add_symbol "`g" prefix
    (*|> add_symbol "`;" (infix 1)
    |> add_symbol "`->" (infix 1)
    |> add_symbol "`!!" (postfix 8)
    (* |> add_symbol "`if" if_symbol *)
    |> add_symbol "`(" (initial 9)
    |> add_symbol "`)" (final 0)
    |> add_symbol "`:" block_symbol *)
    |> add_symbol "`print" prefix
    |> add_symbol "`function" prefix
    |> add_symbol "`add" prefix
    |> add_symbol "`atom" (atomic 1)
    |> add_symbol "`EOF" end_symbol
    |> add_symbol "`EOL" end_symbol (* for the REPL *)


let grammar map tok =
    let tok_id = show_literal tok.value in
    let mk_sym = if Table.mem tok_id map
        then Table.find tok_id map
        else Table.find "`atom" map in
    let sym = mk_sym tok in
    sym
