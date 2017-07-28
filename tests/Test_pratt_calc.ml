
let digit x =
  if Char.Ascii.is_digit x then
    Some (Char.to_int x)
  else None

let calc = Pratt.Rule.[
  token              digit;
  prefix     '+'     (fun a   -> +a);
  infix   30 '+'     (fun a b -> a + b);
  prefix     '-'     (fun a   -> -a);
  infix   30 '-'     (fun a b -> a - b);
  infix   40 '*'     (fun a b -> a * b);
  infix   40 '/'     (fun a b -> a / b);
  between    '(' ')' (fun a   -> a);
]



