open Types
open Alpha_beta

exception Type_error

let rec infer : context -> expr -> expr = fun ctx -> function
  | Type -> Type

  | Var v ->
      begin match List.assoc v ctx with
        | ty, expr -> ty
        | exception Not_found ->
            Format.printf "\nProblem with the free variable [%s]\n@." v;
            raise Type_error
      end

  | App (e1, e2) as inff ->
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      begin match t1 with
        | Pi (x, t, e) when alpha_beta_convertible ~ctx t t2 ->
            sub x e2 e
        | Abs (_, t, e) when alpha_beta_convertible ~ctx t (infer ctx t2) ->
            e
        | _ ->
            Format.printf "\n%a : %a should be of type [(%a) -> ...]\n@." pp_expr e1 pp_expr t1 pp_expr t2;
            raise Type_error
      end

  | Abs (v, e1, e2) as inff->
      let _ : expr = infer ctx e1 in
      let ctx = (v, (e1, None)) :: ctx in
      let t2 = infer ctx e2 in
      Pi (v, e1, t2)

  | Pi (v, e1, e2) as e ->
      let t1 = infer ctx e1 in
      let ctx = (v, (t1, None)) :: ctx in
      let t2 = infer ctx e2 in
      if alpha_beta_convertible ~ctx t1 Type &&
         alpha_beta_convertible ~ctx t2 Type then Type
      else (Format.printf "The term %a devrait etre de type Pi(type, type)@." pp_expr e; raise Type_error)

  | Nat -> Type
  | Z -> Nat
  | S n ->
      check ctx n Nat;
      Nat

  | Ind (p, z, s, n) ->
      check ctx n Nat;
      check ctx z (App(p, Z));
      let x = fresh() in
      let ctx = (x, (Nat, None))::ctx in
      check ctx z (Pi (x, App(p, Var x), App(p, (S (Var x)))));
      App(p, n)

  | Eq (e1, e2) -> failwith "not implemented"
  | Refl e -> failwith "not implemented"
  | J (e1, e2, e3, e4, e5) -> failwith "not implemented"

and check ctx term typ =
  let infered_ty = infer ctx term in
  if not @@ alpha_beta_convertible ~ctx infered_ty typ then
    raise Type_error
