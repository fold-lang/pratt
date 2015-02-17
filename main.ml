
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt
open Fold


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



let rec eval env = function
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
        print_string (bright_blue "-> " ^ start_white);
        flush stdout;
        let input = Lexing.from_channel stdin in
        let e = parse ~input ~grammar: (grammar map) () in
        print (green " = " ^ start_white ^ (show_expr e))
        (* print (green " = " ^ start_white ^ (string_of_int (eval (fun v -> 0) e))) *)
    with
        Failure msg -> print_endline @@ (bright_red " * Error" ^ ": " ^ msg);
                       flush stdout
    done

let () =
    print (end_color ^ "\n" ^ fold_logo ^ "\n");
    Tests.run ();
    Test_parser.run ();
    loop ()

