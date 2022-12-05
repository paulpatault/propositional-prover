open Types

(** 1.4 Type inference *)
(** 1.5 Type checking *)
(** 1.6 Type inference and checking together *)

let rec infer_type (ctx : context) = function
  | Unit -> T1

  | Trustme None -> TMagic
  | Trustme (Some t) -> t

  | Var v ->
      (try List.assoc v ctx
       with Not_found -> raise Type_error)

  | App (t, u) ->
      (match infer_type ctx t with
       | TArrow (a, b) ->
           check_type ctx u a;
           b
       | _ -> raise Type_error)

  | Abs (v, typ, t) ->
      TArrow (typ, infer_type ((v,typ)::ctx) t)

  | Pair (t, u) ->
      TPair (infer_type ctx t, infer_type ctx u)

  | Fst t ->
      (match infer_type ctx t with
       | TPair (t, _) -> t
       | _ -> raise Type_error)

  | Snd t ->
      (match infer_type ctx t with
       | TPair (_, t) -> t
       | _ -> raise Type_error)

  | Case_ (wanted, t) -> check_type ctx t T0; wanted

  | Case (t, u, v) ->
      (match infer_type ctx t with
        | TPlus (l, r) ->
            (match infer_type ctx u, infer_type ctx v with
            | TArrow (a', dest_a), TArrow (b', dest_b)
                when eq_ty l a' && eq_ty r b' && eq_ty dest_a dest_b
                -> dest_b
            (* | t1, t2 when eq_ty t1 t2 -> t1 *)
            | t1, t2 ->
                Format.printf "case(%a,%a,%a)@." Printer.term t Printer.term u Printer.term v;
                Format.printf "%a    ???    %a@." Printer.ty t1 Printer.ty t2;
                raise Type_error)
        | _ -> raise Type_error)

  | Left (ty, t) -> TPlus (infer_type ctx t, ty)

  | Right (ty, t) -> TPlus (ty, infer_type ctx t)

  | Zero -> TNat

  | Succ n -> check_type ctx n TNat; TNat

  | Fix (n, z, s) ->
     (match infer_type ctx n with
       | TNat ->
         (match infer_type ctx z, infer_type ctx s with
           | t, TArrow(TArrow(TNat, t1), t2)
           | t, TArrow(TNat, TArrow(t1, t2)) when eq_ty t t1 && eq_ty t1 t2 -> t
           | _ -> raise Type_error)
       | _ -> raise Type_error)

and check_type ctx t typ =
  let tt = infer_type ctx t in
  if not (eq_ty tt typ) then raise Type_error
