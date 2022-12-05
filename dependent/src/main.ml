open Types
open Format
open Alpha_beta
open Typing

(* let t1 = App (Abs ("x", Type, Var "x"), Var "y")
let t2 = App (Abs ("x", Type, Var "x"), Var "z")
let t3 = App (Abs ("y", Type, Var "y"), Var "z")
let t4 = App (Abs ("y", Type, Var "x"), Var "z")

let () =
  assert (normalize [] t1 = Var "y");
  assert (normalize [] t2 <> Var "y")

let () =
  assert (alpha_convertible t2 t3);
  assert (alpha_convertible t4 t4);
  assert (alpha_convertible t3 t4 |> not)

let () =
  assert (alpha_beta_convertible t2 (Var "z"));
  assert (alpha_beta_convertible t2 (Var "y") |> not)

let () =
  (* printf "%a : %a@." expr t1 expr (infer ["y", (Nat, None)] t1) *)
  printf "%a : %a@." pp_expr t1 pp_expr (infer [] t1)
 *)
let () =
  Loop.interactive_loop ()
