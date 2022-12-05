type var = string


type expr =
  | Type
  | Var of var
  | App of expr * expr
  | Abs of var * expr * expr
  | Pi of var * expr * expr
  | Nat
  | Z
  | S of expr
  | Ind of expr * expr * expr * expr 
  | Eq of expr * expr
  | Refl of expr
  | J of expr * expr * expr * expr * expr

type context = (var * (expr * expr option)) list

open Format

let rec pp_expr fmt = function
  | Type ->
      fprintf fmt "type"
  | Var v ->
      fprintf fmt "%s" v
  | App (e1, e2) ->
      fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | Abs (v, e1, e2) ->
      fprintf fmt "(λ%s:%a. %a)" v pp_expr e1 pp_expr e2
  | Pi (v, e1, e2) ->
      fprintf fmt "(Π%s:%a. %a)" v pp_expr e1 pp_expr e2
  | Nat ->
      fprintf fmt "nat"
  | Z ->
      fprintf fmt "Z"
  | S e ->
      fprintf fmt "S(%a)" pp_expr e
  | Ind (e1, e2, e3, e4) ->
      fprintf fmt "Ind(%a, %a, %a, %a)" pp_expr e1 pp_expr e2 pp_expr e3 pp_expr e4
  | Eq (e1, e2) ->
      fprintf fmt "%a = %a" pp_expr e1 pp_expr e2
  | Refl e ->
      fprintf fmt "refl(%a)" pp_expr e
  | J (e1, e2, e3, e4, e5) ->
      fprintf fmt "J(%a, %a, %a, %a, %a)" pp_expr e1 pp_expr e2 pp_expr e3 pp_expr e4 pp_expr e5

let context =
  let ppv fmt (k,(t, o)) =
    match o with
    | None -> fprintf fmt "%s : %a" k pp_expr t
    | Some e -> fprintf fmt "%s : %a = %a" k pp_expr t pp_expr e in
  let pp_sep fmt = fun () -> fprintf fmt ", " in
  pp_print_list ~pp_sep ppv
