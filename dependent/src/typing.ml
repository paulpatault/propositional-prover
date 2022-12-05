open Types
open Alpha_beta

exception Type_error

(* let (--) a ctx = alpha_beta_convertible ~ctx a
let (-->) f x = f x *)

let rec infer : context -> expr -> expr = fun ctx -> function
  | Type | Nat -> Type

  | Var v ->
      begin match List.assoc v ctx with
        | ty, expr -> ty
        | exception Not_found ->
            Format.printf "\nProblem with the free variable [%s]\n@." v;
            raise Type_error
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
            Format.printf "\n%a : %a should be of type [(%a) -> ...]\n@." pp_expr e1 pp_expr t1 pp_expr t2;
            raise Type_error
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
      else (Format.printf "The term %a devrait etre de type Pi(type, type)@." pp_expr e; raise Type_error)

  | Z -> Nat

  | S n ->
      check ctx n Nat;
      Nat

  | Ind (p, z, s, Z) ->
      let iz = infer ctx z in
      if alpha_beta_convertible ~ctx s (App (p, Z)) then
      (* if s -- ctx --> s (App (p, Z)) then *)
        iz
      else raise Type_error

  | Ind (p, z, s, n) ->
      let nn = fresh () in
      check ctx n Nat;
      check ctx p (Pi (nn, Nat, Type));
      check ctx z (App (p, Z));
      check ctx s (Pi (nn, Nat, Pi ("e", App (p, Var nn), App (p, S (Var nn)))));
      App (p, n)

  | Eq (e1, e2) -> failwith "not implemented"
  | Refl e -> failwith "not implemented"
  | J (e1, e2, e3, e4, e5) -> failwith "not implemented"

and check ctx term typ =
  let infered_ty = infer ctx term in
  if not @@ alpha_beta_convertible ~ctx infered_ty typ then
    raise Type_error
