
open Foundation
open Parser
open Syntax
open Lexicon
open Pratt

module Table = Map.Make(String)

let add_symbol name sym =
    Table.add name sym

let eol_symbol tok = symbol tok
    ~lbp: 1
    ~led: (fun left -> return left)
    ~nud: (return epsilon)

let eof_symbol tok = symbol tok
    ~nud: (return epsilon)

let block_symbol tok = symbol tok
    ~lbp: 1
    ~led: (fun e -> block (parse_expr 0) >>= fun expr_list ->
             return (Term (tok.value, expr_list)))


let map =
    Table.empty
    (* |> add_symbol "`+" (infix 6)
    |> add_symbol "`>" (infix 6)
    |> add_symbol "`<" (infix 6)
    |> add_symbol "`*" (infix 7)
    |> add_symbol "`/" (infix 7)
    |> add_symbol "`=" (infix 1)
    |> add_symbol "`;" (infix 1)
    |> add_symbol "`-" (infix 6)
    |> add_symbol "`->" (infix 1)
    |> add_symbol "`++" (postfix 8)
    |> add_symbol "`!!" (postfix 8)
    (* |> add_symbol "`if" if_symbol *)
    |> add_symbol "`(" (initial 9)
    |> add_symbol "`)" (final 0)
    |> add_symbol "`:" block_symbol *)
    |> add_symbol "`atom" (atomic 9)
    |> add_symbol "`EOF" eof_symbol
    |> add_symbol "`EOL" eol_symbol
    |> add_symbol "`=" block_symbol


let grammar map tok =
    let tok_id = show_literal tok.value in
    let mk_sym = if Table.mem tok_id map
        then Table.find tok_id map
        else Table.find "`atom" map in
    let sym = mk_sym tok in
    sym
