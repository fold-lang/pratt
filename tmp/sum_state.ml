
open Mtl
open Core.Std

type sum_store =
	{  total : int;
     numbers : int list}

module Sum_state = State_monad(struct type store = sum_store end)


let sum : ('x, 'a) Sum_state.m =
  Sum_state.(get >>= fun current ->
    let next = {
      total = current.total + (List.hd_exn current.numbers);
      numbers = (List.tl_exn current.numbers)
    } in
    put next >> unit current.total) in

	let i0 = {total = 0; numbers = [1; 2; 3; 4]} in

	let (v, s) = Sum_state.run sum i0 in
	print_endline (" v = " ^ (string_of_int v));
	print_endline ("#s = " ^ (string_of_int (List.length s.numbers)))

