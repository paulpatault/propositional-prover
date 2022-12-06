open Types
open Alpha_beta

exception Type_error of expr * expr * expr
exception Type_error_
exception Free_var of string

(* let (--) a ctx = alpha_beta_convertible ~ctx a
let (-->) f x = f x *)

let rec infer : context -> expr -> expr = fun ctx -> function
  | Type | Nat -> Type

  | Var v ->
      begin match List.assoc v ctx with
        | ty, expr -> ty
        | exception Not_found -> raise (Free_var v)
      end

  | App (e1, e2) as m_e ->
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      begin match t1 with
        | Pi (x, t, e) when alpha_beta_convertible ~ctx t t2 ->
            sub x e2 e
        | Abs (_, t, e) when alpha_beta_convertible ~ctx t (infer ctx t2) ->
            e
        | _ ->
            raise (Type_error (e1, t1, t2))
      end

  | Abs (v, e1, e2) as m_e ->
      let _ : expr = infer ctx e1 in
      let t2 = infer (ctx ++ (v,e1)) e2 in
      Pi (v, e1, t2)

  | Pi (v, e1, e2) as e ->
      let t1 = infer ctx e1 in
      let t2 = infer (ctx ++ (v,t1)) e2 in
      if alpha_beta_convertible ~ctx t1 Type &&
         alpha_beta_convertible ~ctx t2 Type then Type
      else raise (Type_error (e, Pi(v, t1, t2), Pi(v, Type, Type)))

  | Z -> Nat

  | S n ->
      check ctx n Nat;
      Nat

  | Ind (p, z, s, Z) ->
      let iz = infer ctx z in
      if alpha_beta_convertible ~ctx s (App (p, Z)) then
        iz
      else raise Type_error_

  | Ind (p, z, s, n) ->
      let vn = fresh () in
      let ve = fresh () in
      check ctx n Nat;
      check ctx p (Pi (vn, Nat, Type));
      check ctx z (App (p, Z));
      check ctx s (Pi (vn, Nat, Pi (ve, App (p, Var vn), App (p, S (Var vn)))));
      App (p, n)

  | Eq (e1, e2) ->
      check ctx e2 (infer ctx e1);
      Type

  | Refl e ->
      Eq (e, e)

  | J (p, r, x, y, e) ->
      let vx = fresh() in
      let vy = fresh() in
      let ve = fresh() in
      let a = infer ctx x in
      check ctx y a;
      check ctx e (Eq (x, y));
      check ctx p (Pi (vx, a, Pi (vy, a, Pi (ve, Eq(Var vx, Var vy), Type))));
      check ctx r (Pi (vx, a, App(App(App(p, Var vx), Var vx), Refl (Var vx))));
      App(App(App(p, x), y), e)

and check ctx term typ =
  let infered_ty = infer ctx term in
  if not @@ alpha_beta_convertible ~ctx infered_ty typ then
    raise (Type_error (term, infered_ty, typ))
