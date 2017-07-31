
let calc =
  let open Pratt.Rule in
  [token              (fun t   -> Int.parse t);
   prefix     "+"     (fun a   -> +a);
   infix   30 "+"     (fun a b -> a + b);
   prefix     "-"     (fun a   -> -a);
   infix   30 "-"     (fun a b -> a - b);
   infix   40 "*"     (fun a b -> a * b);
   infix   40 "/"     (fun a b -> a / b);
   between    "(" ")" (fun a   -> a);
   delimiter  ")"]


let scanner str =
  Iter.iter (String.cuts ~sep:" " str)

let () =
  let input = scanner "(2 + 2) * 2" in
  match Pratt.(run (parse calc) input) with
  | Ok value -> Fmt.pr "result = %d\n" value
  | Error e  -> Fmt.pr "%s\n" (Pratt.error_to_string Fmt.string e)

