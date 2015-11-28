
open Elements

open Pratt
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Parser

open Fold_lang
open Fold_encoder

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


let loop () =
  while true do try
      print_string (bright_blue "-> " ^ start_white);
      flush stdout;
      let lexer = create_lexer_with_channel "<REPL>" stdin in
      let fold_expr = init ~lexer ~grammar:core_lang in
      let caml_expr = encode_expr fold_expr in
      begin
        print ("[lisp]" ^ green " =\n" ^ start_white ^ (show_expr fold_expr));
        print_string ("[caml]" ^ green " =\n" ^ start_white);
        Format.printf "%a@." Pprintast.expression caml_expr;
      end
    with
      Failure msg -> print @@ (bright_red " * Error" ^ ": " ^ msg);
      flush stdout
  done

let () = begin
  print (end_color ^ "\n" ^ fold_logo ^ "\n");
  loop ();
end

