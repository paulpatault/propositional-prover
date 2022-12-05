type tvar = string

type var = string

type ty =
  | T0
  | T1
  | TMagic
  | TVar of tvar
  | TArrow of ty * ty
  | TPair of ty * ty
  | TPlus of ty * ty
  | TNat

let rec eq_ty a b = match a, b with
   | TMagic, _
   | _, TMagic
   | T0, T0
   | T1, T1 -> true
   | TVar v1, TVar v2 -> v1 = v2
   | TArrow (a, b), TArrow (a', b')
   | TPlus (a, b), TPlus (a', b')
   | TPair (a, b), TPair (a', b') -> eq_ty a a' && eq_ty b b'
   | _ -> a = b

type term =
  | Unit
  | Trustme of ty option
  | Var of var
  | App of term * term
  | Abs of var * ty * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Case of term * term * term
  | Case_ of ty * term
  | Left of ty * term
  | Right of ty * term
  | Zero
  | Succ of term
  | Fix of term * term * term

type context = (var * ty) list

type sequent = context * ty

exception Type_error
