
open Pratt.Env
open Pratt.Lexer
open Pratt.Syntax
open Fold.Eval

let test_atom_eval () = begin
  let open Expr in
  let env0 = Env.empty in
  assert (eval_expr env0 (int 3) = (env0, int 3));

  let env1 = Env.add "a" (int 42) env0 in
  assert (eval_expr env1 (sym "a") = (env1, int 42))
end

let test_meta_eval () = begin
  let open Expr in
  let env0 = Env.empty in
  assert (eval_expr env0 (Expr.meta (int 1) (unit ())) = (env0, int 1))
end

let () = begin
  test_atom_eval ();
  test_meta_eval ();
end

