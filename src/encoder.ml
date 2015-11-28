
open Syntax
open Lexer


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
    | Atom (Sym sym_name) ->
      Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident sym_name))

    | Atom (Int int_val) ->
      Parsetree.Pexp_constant (Asttypes.Const_int int_val)

    | Atom (Str str_val) ->
      Parsetree.Pexp_constant (Asttypes.Const_string (str_val, None))

    | Atom (Char char_val) ->
      Parsetree.Pexp_constant (Asttypes.Const_char char_val)

    | Atom (Float float_val) ->
      Parsetree.Pexp_constant (Asttypes.Const_float (string_of_float float_val))

    (* Bindings *)
    | Term (Atom (Sym ";"), [Term (Atom (Sym "="), [patt; value]); body]) ->
      let value_binding =
        Parsetree.{ pvb_pat = encode_patt patt;
            pvb_expr = encode_expr value;
            pvb_attributes = [];
            pvb_loc = Location.none } in
      Parsetree.Pexp_let (Asttypes.Nonrecursive, [value_binding], encode_expr body)

    (* Sequence *)
    | Term (Atom (Sym ";"), [exp_1; exp_2]) ->
      Parsetree.Pexp_sequence (encode_expr exp_1, encode_expr exp_2)

    (* Conditionals *)
    | Term (Atom (Sym "if"), [cond; conseq; alt]) ->
      Parsetree.Pexp_ifthenelse (encode_expr cond, encode_expr conseq, Some (encode_expr alt))

    | Term (Atom (Sym "if"), [cond; conseq]) ->
      Parsetree.Pexp_ifthenelse (encode_expr cond, encode_expr conseq, None)

    (* Function Application *)
    | Term (f, args) ->
      let encode_arg arg = ("", encode_expr arg) in  (* No labels. *)
      Parsetree.Pexp_apply (encode_expr f, List.map encode_arg args) in

  Parsetree.{ pexp_desc = caml_expr_desc;
      pexp_loc = Location.none;
      pexp_attributes = [] }



