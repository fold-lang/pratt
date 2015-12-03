
open Pratt.Syntax
open Pratt.Lexer

(* TParsetree.O: Provide location info. *)

let encode_patt fold_patt : Parsetree.pattern =
  let caml_patt_desc =
    match fold_patt with
    | Atom (Sym name) ->
      Parsetree.Ppat_var (Location.mknoloc name)
    | _ -> assert false in
  Parsetree.{ ppat_desc = caml_patt_desc;
      ppat_loc = Location.none;
      ppat_attributes = [] }

let rec encode_expr fold_expr : Parsetree.expression =
  let caml_expr_desc =
    match fold_expr with
    (* Literals *)
    | Atom (Sym x) ->
      Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident x))

    | Atom (Int x) ->
      Parsetree.Pexp_constant (Asttypes.Const_int x)

    | Atom (Str x) ->
      Parsetree.Pexp_constant (Asttypes.Const_string (x, None))

    | Atom (Bool x) ->
      Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident (string_of_bool x)))

    | Atom (Char x) ->
      Parsetree.Pexp_constant (Asttypes.Const_char x)

    | Atom (Float float_val) ->
      Parsetree.Pexp_constant (Asttypes.Const_float (string_of_float float_val))

    (* Bindings *)
    | List [Atom (Sym ";"); List [Atom (Sym "="); patt; value]; body] ->
      let value_binding =
        Parsetree.{ pvb_pat = encode_patt patt;
            pvb_expr = encode_expr value;
            pvb_attributes = [];
            pvb_loc = Location.none } in
      Parsetree.Pexp_let (Asttypes.Nonrecursive, [value_binding], encode_expr body)

    (* Sequence *)
    | List [Atom (Sym ";"); exp_1; exp_2] ->
      Parsetree.Pexp_sequence (encode_expr exp_1, encode_expr exp_2)

    (* Conditionals *)
    | List [Atom (Sym "if"); cond; conseq; alt] ->
      Parsetree.Pexp_ifthenelse (encode_expr cond, encode_expr conseq, Some (encode_expr alt))

    | List [Atom (Sym "if"); cond; conseq] ->
      Parsetree.Pexp_ifthenelse (encode_expr cond, encode_expr conseq, None)

    (* Function Application *)
    | List (f :: args) ->
      let encode_arg arg = ("", encode_expr arg) in  (* No labels. *)
      Parsetree.Pexp_apply (encode_expr f, List.map encode_arg args)

    | List [] -> assert false
  in
  Parsetree.{ pexp_desc = caml_expr_desc;
              pexp_loc = Location.none;
              pexp_attributes = [] }



