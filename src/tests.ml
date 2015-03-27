
open Fold.Foundation
open Fold.Lexer
open Fold.Syntax
open Fold.Pratt
open Fold.Lang

let parse_string s =
    parse ~lexer: (lexer_with_string "Fold.Lang.Tests" s)
        ~grammar: (grammar map) ()

let (~>) s =
  try
    print ((bright_blue "-> ") ^ s);
    print (" = " ^ show_expr (parse_string s))
  with Failure msg ->
    print_endline (bright_red " * " ^ bright_white "Error" ^ ": " ^ msg);
    flush stdout

let (~~) s = ()

let (==) s e =
  try
    print_endline (format "%s %s" (bright_blue "->") (white s));
    let r = parse_string s in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s %s" i (show_expr r) "::" (red "Expr"));
    if not y then
        (print_endline (format "\n  Expected: %s" (show_expr e));
       print_endline (format "    Actual: %s\n" (show_expr r)))
    else ()
  with Failure msg ->
    print_endline (bright_red " * " ^ bright_white "Error" ^ ": " ^ msg);
    flush stdout

let sym x = Atom (Symbol x)
let str x = Atom (String x)
let int x = Atom (Integer x)
let seq e1 e2 = Term (Symbol ";", [e1; e2])
let term head args = Term (Symbol head, args)

let x           = Atom (Symbol "x")
let y           = Atom (Symbol "y")
let z           = Atom (Symbol "z")
let add x y     = Term (Symbol "+", [x; y])
let mul x y     = Term (Symbol "*", [x; y])
let f x y       = Term (Symbol "f", [x; y])
let g x         = Term (Symbol "g", [x])
let def x y     = Term (Symbol "=", [x; y])
let run () =
(*
  (* Literals and basic expressions. *)
  "x"                   == x;
  "5"                   == int 5;

  (* Arithmetic expressions. *)
  "x + y"               == (add x y);
  "x + y * z"           == (add x (mul y z));

  (* Function application. *)
  "f x y"               == (f x y);
  "f x y + z"           == (add (f x y) z);
 *)
  (* Expression grouping and precedence. *)
  "(x)"                 == x;
  "(((x)))"             == x;
  "(x + y)"             == (add x y);
  "(x + y) * z"         == (mul (add x y) z);
  "(x + (y + y)) * z"   == (mul (add x (add y y)) z);
  "(f x y)"             == (f x y);

  (* End-of-line handling. *)
  "x\ny"                == (seq x y);
  "x +\ny"              == (add x y);
  "x\n- y"              == (seq x (term "-" [y]));

  (* End-of-line handling inside expression groups. *)
  "(x +\ny)"            == (add x y);
  "(f x\ny)"            == (f x y);

  (* Expression blocks. *)
  "{2}"                 == (int 2);
  "{2 + 2}"             == (add (int 2) (int 2));
  "f x {2 + 2}"         == (f x (add (int 2) (int 2)));


  (* "x ? 1 : 0"           == (term "?" [x; int 1; int 0]);
  "if x then 1 else 0"  == (term "if" [x; int 1; int 0]);
  "x; y; z"             == (seq x (seq y z));
  "x + y; z"            == (seq (add x y) z);
  "x = f y z; 5"        == (def x (seq (f y z) (int 5))); *)

~>
"{1 + 1
}";


~>
"{-1}";

~>
"{f x
}";

~>
"(f x)";

~>
"{
f
a
b
c}";

~>
"(f
)";

~> "(x + y) 3"
