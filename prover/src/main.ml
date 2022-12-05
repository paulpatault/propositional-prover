open Stlc
open Printer
open Format

(* let () =
  printf "une formule : @.";
  let lexbuf = Lexing.from_channel stdin in
  let formule_in = Uparser.term Ulexer.scan_token lexbuf in
  printf "%a" term formule_in *)


let eval_and_print ~str s =
  let lexbuf = Lexing.from_string s in
  let formule_in = Uparser.term Ulexer.scan_token lexbuf in
  let t = infer_type [] formule_in in
  printf "%s@[<2>@\nterm : %a@\ntype : %a@]@." str term formule_in ty t

let and_comm_proof =
  let str = "[AND] commutative" in
  eval_and_print ~str "fun (y : (A and B)) -> (snd y, fst y)$"

let truth = eval_and_print "fun (y : (1 => A)) -> (y) ()$"

let or_comm_proof =
  let str = "[OR] commutative" in
  let or_comm =
    "fun (p : (A or B)) ->
       case (p,
         fun (x : A) -> right{B} x,
         fun (x : B) -> left{A} x)$" in
  eval_and_print ~str or_comm

let falsity =
  let str = "preuve de faux" in
  let falsity =
    "fun (p : (A and (A => 0))) ->
      case {B} ((snd(p)) (fst(p)))
      $" in
  eval_and_print ~str falsity

let tru =
  let str = "preuve de faux" in
  let falsity =
    "fun (p : (A and (A => 0))) ->
      case {B} ((snd(p)) (fst(p)))
      $" in
  eval_and_print ~str falsity

let nat_parsing_test =
  let str = "tests nat" in
  let zz = "z$" in
  eval_and_print ~str zz;
  let sz = "s(z)$" in
  eval_and_print ~str sz;
  let sz' = "s z$" in
  eval_and_print ~str sz';
  let ssz = "s(s(z))$" in
  eval_and_print ~str ssz
