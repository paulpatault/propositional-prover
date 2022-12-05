open Types
open Stlc
open Printer
open Maker

let a_to_b = mk_typ "A => B"
let a_to_a = mk_typ "A => A"
let b_to_b = mk_typ "B => B"
let b_to_c = mk_typ "B => C"

let idA = mk_term "fun (x : A) -> x"
let idB = mk_term "fun (x : B) -> x"

let compose =
  check_type []
  (mk_term "fun (f : A=>B) -> fun (g : B=>C) -> fun (x : A) -> g (f x)")
  (mk_typ "(A=>B)=>(B=>C)=>A=>C" )

let bad_type t : unit =
  try
    ignore (infer_type [] t);
    assert false
  with Type_error -> ()

let bad_type_check t ty : unit =
  try
    ignore (check_type [] t ty);
    assert false
  with Type_error -> ()

let () =
  let t1 = mk_term "fun (f : A) -> x" in
  bad_type t1;

  let t2 = mk_term "fun (f:A) -> fun (x:B) -> f x" in
  bad_type t2;

  let t3 = mk_term "fun (f:A=>B) -> fun (x:B) -> f x" in
  bad_type t3;

  let t4 = mk_term "fun (f:B=>A) -> fun (x:B) -> f x" in
  let typ_t4 = mk_typ "(B=>A)=>B=>A" in
  check_type [] t4 typ_t4;

  check_type [] idA a_to_a; (* λ(x:A).x has type A→A *)

  bad_type_check idB a_to_a;
  bad_type_check idB a_to_b;

  bad_type_check idA b_to_b; (* λ(x:A).x does not have type B→B *)

  bad_type_check (mk_term "x") (mk_typ "A") (* x does not have type A. *)
