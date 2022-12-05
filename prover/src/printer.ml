open Types
open Format

(* λ 	 	 	⇒ 	 	 *)
(* Π 	 	 	¬ 	 	 *)

(* pas optimal mais permet d'avoir un peu moins de parenthèses *)

let ty_atom fmt = function
  | T0 -> fprintf fmt "⊥"
  | T1 -> fprintf fmt "⊤"
  | TMagic -> fprintf fmt "∀α.α"
  | TVar v -> fprintf fmt "%s" v
  | TNat -> fprintf fmt "nat"
  | TArrow _ | TPair _ | TPlus _ -> assert false

let rec ty' fmt = function
  | TArrow (t1,t2) -> fprintf fmt "(%a → %a)" ty' t1 ty' t2
  | TPair (t1,t2)  -> fprintf fmt "(%a ∧ %a)" ty' t1 ty' t2
  | TPlus (t1,t2)  -> fprintf fmt "(%a ∨ %a)" ty' t1 ty' t2
  | t -> ty_atom fmt t

let ty fmt = function
  | TArrow (t1,t2) -> fprintf fmt "%a → %a" ty' t1 ty' t2
  | TPair (t1,t2)  -> fprintf fmt "%a ∧ %a" ty' t1 ty' t2
  | TPlus (t1,t2)  -> fprintf fmt "%a ∨ %a" ty' t1 ty' t2
  | t -> ty_atom fmt t

let rec term fmt = function
  | Zero -> fprintf fmt "z"
  | Unit -> fprintf fmt "()"
  | Trustme None -> fprintf fmt "(magic:%a)" ty_atom TMagic
  | Trustme (Some t) -> fprintf fmt "(magic:%a)" ty t
  | Var x -> fprintf fmt "%s" x
  | App (t1, t2) -> fprintf fmt "(%a %a)" term t1 term t2
  | Abs (v, typ, t) -> fprintf fmt "(λ%s:%a. %a)" v ty typ term t
  | Pair (t1, t2) -> fprintf fmt "(%a,%a)" term t1 term t2
  | Case (t, u, v) -> fprintf fmt "case(%a,(%a),(%a))" term t term u term v
  | Fst t -> fprintf fmt "π₁(%a)" term t
  | Snd t -> fprintf fmt "π₂(%a)" term t
  | Left (ty', t) -> fprintf fmt "ι₁{%a}(%a)" ty ty' term t
  | Right (ty', t) -> fprintf fmt "ι₂{%a}(%a)" ty ty' term t
  | Case_ (ty', t) -> fprintf fmt "case{%a}(%a)" ty ty' term t
  | Succ n -> fprintf fmt "succ(%a)" term n
  | Fix (n, z, s) -> fprintf fmt "fix(%a,%a,%a)" term n term z term s

let print_term t = printf "%a@." term t

let context =
  let ppv fmt (k,v) = fprintf fmt "%s : %a" k ty v in
  let pp_sep fmt () = fprintf fmt ", " in
  pp_print_list ~pp_sep ppv

let sequent fmt (ctx, typ) =
  fprintf fmt "%a ⊢ %a" context ctx ty typ

let print_sequent = printf "%a" sequent
