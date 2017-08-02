
module C = struct
  let color_format color =
    format "\027[%dm%s\027[0m"
      (match color with
       | `Black   -> 30
       | `Red     -> 31
       | `Green   -> 32
       | `Yellow  -> 33
       | `Blue    -> 34
       | `Magenta -> 35
       | `Cyan    -> 36
       | `White   -> 37)

  let blue               = color_format `Blue
  let red                = color_format `Red
  let yellow             = color_format `Yellow
  let magenta            = color_format `Magenta
  let cyan               = color_format `Cyan
  let white              = color_format `White
  let green              = color_format `Green
  let bright_white x     = format "\027[1;37m%s\027[0m" x
  let bright_blue x      = format "\027[1;34m%s\027[0m" x
  let bright_magenta x   = format "\027[1;35m%s\027[0m" x
  let violet x           = format "\027[0;34m%s\027[0m" x
  let bright_red x       = format "\027[1;31m%s\027[0m" x
  let bright_green x     = format "\027[1;32m%s\027[0m" x
  let start_bright_white = format "\027[1;37m"
  let start_white        = format "\027[37m"
  let end_color          = "\027[0m"
  let italic x           = "\027[3m" ^ x ^ "\027[0m"
  let underline x        = "\027[4m" ^ x ^ "\027[0m"
  let blink x            = "\027[5m" ^ x ^ "\027[0m"
end


module type Testable = sig
  type t
  include Equatable.Base with type t := t
  include Printable.Base with type t := t
end

type 'a testable = (module Testable with type t = 'a)

let pp (type a) (t: a testable) = let (module T) = t in T.pp

let equal (type a) (t: a testable) = let (module T) = t in T.equal


let time ?fmt f x =
  let t0 = Unix.gettimeofday () in
  let fx = f x in
  let t1 = Unix.gettimeofday () -. t0 in
  let () = match fmt with
  | Some fmt -> Printf.eprintf "%s\n" (fmt fx t1)
  | None     -> Printf.eprintf "Elapsed time: %f sec\n" t1 in
  fx

let test ~verbose ty msg ~actual ~expected () =
  let ok = equal ty actual expected in
  begin if not ok then begin
    print (Fmt.strf "  %s %s" (C.bright_red "✗") (C.bright_white msg));
    print (Fmt.strf "    - %a" (pp ty) expected);
    print (Fmt.strf "    + %a" (pp ty) actual)
  end else if verbose then
    print (format "  %s %s" (C.bright_green "✓") (C.bright_white msg))
  end;
  ok

let testable (type a) (pp: a Fmt.t) (equal: a -> a -> bool) : a testable =
  let module M = struct
    type t = a
    let pp = pp
    let equal = equal
  end in
  (module M)

let group name tests =
  print (format "━━━ %s ━━━" (C.bright_blue name));
  let t0 = Unix.gettimeofday () in
  let s, f, t =
    List.fold_left begin fun (s, f, t) test ->
        if test () then (s + 1, f, t + 1) else (s, f + 1, t + 1)
      end
      (0, 0, 0) tests in
  let t = Unix.gettimeofday () -. t0 in
  let msg =
    match s, f with
    | 1, 0 -> "Test passed"
    | s, 0 -> format "All %d tests passed" s
    | 0, 1 -> "Test failed"
    | 0, f -> format "All %d tests failed" f
    | s, f -> format "%d tests passed, %d tests failed" s f in
  print (format "  %s %s in %0.2fms\n" (C.bright_magenta "•") msg (t *. 1000.0))

let int    = testable Fmt.int (=)
let int32  = testable Fmt.int32 (=)
let int64  = testable Fmt.int64 (=)
let float  = testable Fmt.float (=)
let char   = testable Fmt.char (=)
let string = testable Fmt.string (=)
let bool   = testable Fmt.bool (=)
let unit   = testable (Fmt.unit "()") (=)


let list e =
  let rec eq l1 l2 = match (l1, l2) with
    | (x::xs, y::ys) -> equal e x y && eq xs ys
    | ([], []) -> true
    | _ -> false in
  testable (Fmt.Dump.list (pp e)) eq

let slist (type a) (a : a testable) compare =
  let l = list a in
  let eq l1 l2 = equal l (List.sort compare l1) (List.sort compare l2) in
  testable (pp l) eq

let array e =
  let eq a1 a2 =
    let (m, n) = Array.(length a1, length a2) in
    let rec go i = i = m || (equal e a1.(i) a2.(i) && go (i + 1)) in
    m = n && go 0 in
  testable (Fmt.Dump.array (pp e)) eq

let pair a b =
  let eq (a1, b1) (a2, b2) = equal a a1 a2 && equal b b1 b2 in
  testable (Fmt.Dump.pair (pp a) (pp b)) eq

let option e =
  let eq x y = match (x, y) with
    | (Some a, Some b) -> equal e a b
    | (None, None) -> true
    | _ -> false in
  testable (Fmt.Dump.option (pp e)) eq

let result a e =
  let eq x y =
    match (x, y) with
    | (Ok x, Ok y) -> equal a x y
    | (Error x, Error y) -> equal e x y
    | _ -> false in
  testable (Fmt.Dump.result ~ok:(pp a) ~error:(pp e)) eq

let of_pp pp = testable pp (=)

