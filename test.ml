

let a         = Atom (Symbol "a")
let b         = Atom (Symbol "b")
let c         = Atom (Symbol "c")
let ( + ) a b = Term (Symbol "+", [a; b])
let ( ! ) a   = Term (Symbol "!", [a])
let ( ++ ) a  = Term (Symbol "++", [a])
let ( * ) a b = Term (Symbol "*", [a; b])
let f b       = Term (Symbol "f", [a])
let g a b c   = Term (Symbol "g", [a; b; c])
let h         = Atom (Symbol "h")
let m xs      = Term (Symbol "module", xs)

let test () =
    ""              == m [];
    "a"             == m [a];
    "!a"            == m [! a];
    "a++"           == m [(++) a];
    "a + a"         == m [a + a];
    "a + a * a"     == m [(a + (a * a))];
    "f a"           == m [f a];
    "f a + a"       == m [(f a) + a];
    "f a + f a"     == m [(f a) + (f a)];
    "g a b b"       == m [g a b b];
    "h"             == m [h];
    "(a + a)"       == m [a + a];
    "(((a)))"       == m [a];

    ~> "a = f (b + c) d";

    ~> "function sum x y = x + y"
