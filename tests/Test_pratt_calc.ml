
let iter str =
  let next i =
    try
      Some (String.get str i, (i + 1))
    with Invalid_argument _ ->
      None in
  Iter.Iter (0, next)


let digit x =
  if Char.Ascii.is_digit x then
    Some (int_of_string (String.of_char x))
  else None



let calc =
  let open Pratt.Rule in
  [token              (fun t   -> digit t);
   prefix     '+'     (fun a   -> +a);
   infix   30 '+'     (fun a b -> a + b);
   prefix     '-'     (fun a   -> -a);
   infix   30 '-'     (fun a b -> a - b);
   infix   40 '*'     (fun a b -> a * b);
   infix   40 '/'     (fun a b -> a / b);
   between    '(' ')' (fun a   -> a);
   delimiter  ')']


let () =
  let input = iter "(2+2)*2" in
  match Pratt.(run (parse calc) input) with
  | Ok value -> Fmt.pr "result = %d\n" value
  | Error e  -> Fmt.pr "%s\n" (Pratt.error_to_string Fmt.char e)


