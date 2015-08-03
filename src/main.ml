
open Foundation
open Pratt
open Fold_syntax
open Fold_lexer
open Fold_parser
open Fold_lang

module Enc = Fold_encoder


let fold_logo =
blue ("     ▗▐▝        ▐▐      ▐▐   ") ^ ("|" ^ bright_white "  A modern pragmatic functional language.\n") ^
blue ("    ▐▐     ▗▗   ▐▐    ▗▗▐▐   ") ^ ("|" ^ bright_white "\n") ^
blue ("  ▝▝▐▐▝▝ ▐▐  ▐▐ ▐▐  ▐▐  ▐▐   ") ^ ("|" ^ bright_white "  Version 0.0.1-alpha+001 (2015-02-12)\n") ^
blue ("    ▐▐   ▐▐  ▐▐ ▐▐  ▐▐  ▐▐   ") ^ ("|  " ^ (underline (bright_white "http://fold-lang.org\n"))) ^
blue ("    ▐▐     ▝▝    ▝▝   ▝▝▝    ") ^ ("|" ^ bright_white "  Type ? for help.\n") ^
blue ("  ▗▐▝                      \n") ^
          "                           \n"  ^
 (italic "  \"Simplicity is prerequisite for reliability.\"\n") ^
      "      — Edsger W. Dijkstra"

let compile (typedtree, coercion) = begin
  let module_name, output_prefix = "Hello", "x" in
  let bytecode =
    (typedtree, coercion)
    |> Translmod.transl_implementation module_name
    (*|> print_if ppf Clflags.dump_rawlambda Printlambda.lambda*)
    |> Simplif.simplify_lambda
    (*|> print_if ppf Clflags.dump_lambda Printlambda.lambda*)
    |> Bytegen.compile_implementation module_name
    (*|> print_if ppf Clflags.dump_instr Printinstr.instrlist*)
  in
  let objfile = output_prefix ^ ".cmo" in
  let oc = open_out_bin objfile in
  try
    bytecode |> Emitcode.to_file oc module_name objfile;
    Warnings.check_fatal ();
    close_out oc;
    Stypes.dump (Some (output_prefix ^ ".annot"))
  with x ->
    close_out oc;
    (*remove_file objfile;*)
    raise x
end

let loop () =
    while true do try
        print_string (bright_blue "=> " ^ start_white);
        flush stdout;
        let lexer = create_lexer_with_channel "<REPL>" stdin in
        let exp = init ~lexer ~grammar: core_lang () in
        let ocaml_exp = Enc.encode exp in

        Compmisc.init_path false;
        Env.set_unit_name "Hello";
        let ocaml_exp_struct = [{ Parsetree.pstr_desc = Parsetree.Pstr_eval (ocaml_exp, []);
                                  Parsetree.pstr_loc  = Location.none }] in
        Format.printf "%a@." Pprintast.expression ocaml_exp;

        ocaml_exp_struct
        |> Typemod.type_implementation "hello" "hello.out" "Hello" (Compmisc.initial_env())
        |> compile;
        (*|> Format.printf "%a@." Printtyped.implementation_with_coercion;*)

        print (green " = " ^ start_white ^ (show_exp exp))
    with
        Failure msg -> print_endline @@ (bright_red " * Error" ^ ": " ^ msg);
                       flush stdout
    done

let () =
    print (end_color ^ "\n" ^ fold_logo ^ "\n");
    Tests.run ();
    loop ();
    ()

