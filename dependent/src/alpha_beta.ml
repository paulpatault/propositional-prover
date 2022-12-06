open Types
open Set

module S = Set.Make(String)

let fresh =
  let r = ref 0 in
  fun () -> incr r; Format.sprintf "x'%d" !r

let (++) ctx (x, a) = (x, (a, None)) :: ctx

let rec fv = function
  | Type | Nat | Z -> S.empty
  | Var x -> S.singleton x
  | Abs (x, t1, t2) | Pi (x, t1, t2) ->
      S.union (fv t1) (S.remove x (fv t2))
  | Eq (t1, t2) | App (t1, t2) ->
      S.union (fv t1) (fv t2)
  | S t | Refl t -> fv t
  | Ind (t1, t2, t3, t4) ->
      S.union (fv t1)
        (S.union (fv t2)
          (S.union (fv t3) (fv t4)))

  | J (t1, t2, t3, t4, t5) ->
      S.union (fv t1)
        (S.union (fv t2)
          (S.union (fv t3)
            (S.union (fv t3) (fv t5))))


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
      let nt = normalize (ctx++(x,a)) t in
      Abs (x, na, nt)

  | Pi (x, a, t) ->
      let na = normalize ctx a in
      let nt = normalize (ctx++(x,a)) t in
      Pi (x, na, nt)

  | Ind (p, z, s, n) ->
      (match normalize ctx n with
       | Z   -> normalize ctx z
       | S n -> normalize ctx (App(App(s, n), Ind (p, z, s, n)))
       | _   -> Ind (normalize ctx p, normalize ctx z, normalize ctx s, n))

  | Eq (t1, t2) ->
      Eq (normalize ctx t1, normalize ctx t2)

  | Refl t ->
      Refl (normalize ctx t)

  | J (p, r, x, y, p') ->
      let p = normalize ctx p in
      let r = normalize ctx r in
      let x = normalize ctx x in
      let y = normalize ctx y in
      let p' = normalize ctx p' in

        match p' with
        | Refl z when x @~ y && z @~ x ->
            App (r, x)
        | _ ->
            J (p, r, x, y, p')

and (@~) a b = alpha_convertible a b

and alpha_convertible : expr -> expr -> bool = fun t1 t2 ->
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
        l && r
    | Abs (v, e1, e2), Abs (v', e1', e2')
    | Pi (v, e1, e2), Pi (v', e1', e2') ->
        let env = (v, v') :: env in
        let l = alpha env (e1, e1') in
        let r = alpha env (e2, e2') in
        l && r
    | S e1, S e2 | Refl e1, Refl e2 ->
        alpha env (e1, e2)

    | Ind (e1, e2, e3, e4), Ind (e1', e2', e3', e4') ->
        alpha env (e1, e1') &&
        alpha env (e2, e2') &&
        alpha env (e3, e3') &&
        alpha env (e4, e4')

    | J (e1, e2, e3, e4, e5), J (e1', e2', e3', e4', e5') ->
        alpha env (e1, e1') &&
        alpha env (e2, e2') &&
        alpha env (e3, e3') &&
        alpha env (e4, e4') &&
        alpha env (e5, e5')

    | _ -> false
  in
  let s1 = fv t1 in
  let s2 = fv t2 in
  S.equal s1 s2 && alpha [] (t1, t2)

and alpha_beta_convertible ?(ctx = []) t1 t2 =
  let t1 = normalize ctx t1 in
  let t2 = normalize ctx t2 in
  t1 @~ t2
