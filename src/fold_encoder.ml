
open Fold_syntax
open Fold_lexer

let rec encode fold_exp : Parsetree.expression =
  let caml_exp_desc =
    match fold_exp with
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

    | List [Atom (Sym ";"); exp_1; exp_2] ->
      Parsetree.Pexp_sequence (encode exp_1, encode exp_2)

    | List [Atom (Sym "if"); cond; conseq; alt] ->
      Parsetree.Pexp_ifthenelse (encode cond, encode conseq, Some (encode alt))
    | List [Atom (Sym "if"); cond; conseq] ->
      Parsetree.Pexp_ifthenelse (encode cond, encode conseq, None)

    | List (head :: args) ->
      let encode_arg arg = ("", encode arg) in
      Parsetree.Pexp_apply (encode head, List.map encode_arg args)

    | _ -> raise (Failure "Could not encode fold expression") in
  { pexp_desc = caml_exp_desc;
    pexp_loc = Location.none;
    pexp_attributes = [] }

let implementation (lexer : Fold_lexer.lexer) =
  let exp = Pratt.init ~lexer ~grammar:Fold_lang.core_lang () in
  encode exp

