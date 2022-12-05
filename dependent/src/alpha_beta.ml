open Types
open Set

module S = Set.Make(String)

let fresh =
  let r = ref 0 in
  fun () -> incr r; Format.sprintf "x%d" !r

let rec fv = function
  | Type | Nat | Z -> S.empty
  | Var x -> S.singleton x
  | Abs (x, t1, t2) | Pi (x, t1, t2) ->
     S.union (fv t1) (S.remove x (fv t2))
  | Eq (t1, t2) | App (t1, t2) ->
     S.union (fv t1) (fv t2)
  | S t | Refl t -> fv t
  | Ind (t1, t2, t3, t4) -> failwith "todo"
  | J (t1, t2, t3, t4, t5) -> failwith "todo"

let rec has_fv v = function
  | Type | Nat | Z ->
      false
  | Var x ->
      x = v
  | Abs (x, t1, t2)  | Pi (x, t1, t2) ->
      has_fv v t1 || (x <> v && has_fv v t2)
  | Eq (t1, t2) | App (t1, t2) ->
      has_fv v t1 || has_fv v t2
  | S t | Refl t ->
      has_fv v t
  | Ind (t1, t2, t3, t4) ->
      has_fv v t1 || has_fv v t2 || has_fv v t3 || has_fv v t4
  | J (t1, t2, t3, t4, t5) ->
      has_fv v t1 || has_fv v t2 || has_fv v t3 || has_fv v t4 || has_fv v t5

let rec sub x u = function
  | Var y when y = x ->
      u
  | Var _ as t ->
      t
  | App (t1, t2) ->
      let t1 = sub x u t1 in
      let t2 = sub x u t2 in
      App (t1, t2)

  | Abs (y, t, t2) when x = y -> Abs (y, sub x u t, t2)
  | Pi  (y, t, t2) when x = y -> Pi  (y, sub x u t, t2)

  | Abs (y, t1, t2) when has_fv y u ->
      let y' = fresh () in
      let t1 = sub x u t1 in
      let t2 = sub y (Var y') t2 |> sub x u in
      Abs (y', t1, t2)

  | Pi  (y, t1, t2) when has_fv y u ->
      let y' = fresh () in
      let t1 = sub x u t1 in
      let t2 = sub y (Var y') t2 |> sub x u in
      Pi (y', t1, t2)

  | Abs (y, t1, t2) -> Abs (y, sub x u t1, sub x u t2)
  | Pi  (y, t1, t2) -> Pi  (y, sub x u t1, sub x u t2)

  | S t ->
      S (sub x u t)
  | Ind (t1, t2, t3, t4) ->
      Ind (sub x u t1, sub x u t2, sub x u t3, sub x u t4)
  | Eq (t1, t2) ->
      Eq (sub x u t1, sub x u t2)
  | Refl t ->
      Refl (sub x u t)
  | J (t1, t2, t3, t4, t5) ->
      J (sub x u t1, sub x u t2, sub x u t3, sub x u t4, sub x u t5)
  | Type | Nat | Z as t ->
      t

let rec normalize ctx = function
  | Type | Nat | Z as t -> t
  | Var v ->
      (match List.assoc_opt v ctx with
       | Some (_, Some t) -> normalize ctx t
       | _ -> Var v)
  | App (t, u) ->
      (match normalize ctx t with
       | Abs (x, _a, t)
       | Pi (x, _a, t) -> normalize ctx (sub x u t) (* ? *)
       | t -> App (t, normalize ctx u))
  | S t ->
      S (normalize ctx t)

  | Abs (x, a, t) ->
      let na = normalize ctx a in
      let nt = normalize ((x, (a, None))::ctx) t in
      Abs (x, na, nt)
  | Pi (x, a, t) ->
      let na = normalize ctx a in
      let nt = normalize ((x, (a, None))::ctx) t in
      Pi (x, na, nt)

  | Ind _ ->
      failwith "todo"
  | Eq _ ->
      failwith "todo"
  | Refl _ ->
      failwith "todo"
  | J _ ->
      failwith "todo"

let alpha_convertible : expr -> expr -> bool = fun t1 t2 ->
  let rec alpha env = function
    | Type, Type
    | Nat,  Nat
    | Z,    Z -> true
    | Var v1, Var v2 ->
      begin match List.assoc_opt v1 env with
        | Some x -> x = v2
        | None   -> true   (* cas variable libre *)
      end
    | App (e1, e2), App (e1', e2')
    | Eq (e1, e2), Eq (e1', e2') ->
      let l = alpha env (e1, e1') in
      let r = alpha env (e2, e2') in
      (* Format.printf "app : %b %b : %a %a@." l r Printer.expr e2 Printer.expr e2'; *)
      l && r
    | Abs (v, e1, e2), Abs (v', e1', e2')
    | Pi (v, e1, e2), Pi (v', e1', e2') ->
      let env = (v, v') :: env in
      let l = alpha env (e1, e1') in
      let r = alpha env (e2, e2') in
      (* Format.printf "abs : %b %b@." l r; *)
      l && r
    | S e1, S e2 | Refl e1, Refl e2 ->
      alpha env (e1, e2)
    | Ind (e1, e2, e3, e4), Ind (e1', e2', e3', e4') -> failwith "todo"
    | J (e1, e2, e3, e4, e5), J (e1', e2', e3', e4', e5') -> failwith "todo"
    | _ -> false
  in
  let s1 = fv t1 in
  let s2 = fv t2 in
  S.equal s1 s2 && alpha [] (t1, t2)

let alpha_beta_convertible ?(ctx = []) t1 t2 =
  let t1 = normalize ctx t1 in
  let t2 = normalize ctx t2 in
  alpha_convertible t1 t2

let (===) = alpha_beta_convertible

(* let _ =
  let t1 = App (Abs ("x", Type, Var "x"), Var "y") in
  let t2 = App (Abs ("x", Type, Var "x"), Var "z") in
  t1 === t2 *)