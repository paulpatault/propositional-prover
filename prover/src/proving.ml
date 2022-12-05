open Types
open Printer
open Stlc
open Maker

let channel = ref stdin
let print_req = ref false
let cmds : string list ref = ref []

let step =
  try
    ignore Sys.argv.(2);
    fun () -> ignore @@ input_line stdin
  with Invalid_argument _ ->
    fun () -> ()

let (!!) r = step (); !r

let parse_cmd cmd =
  let n = try String.index cmd ' ' with Not_found -> String.length cmd in
  let c = String.sub cmd 0 n in
  let a = String.sub cmd n (String.length cmd - n) |> String.trim in
  c, a

let parse_cli () =
  match !cmds with
  | [] ->
      let cmd = input_line !!channel in
      if !print_req then Format.printf "%s@." cmd;
      parse_cmd cmd
  | c::tl ->
      cmds := tl; parse_cmd c

let arg_for = Format.sprintf "Please provide an argument for %s."

exception E

let rec proove env a =
  Format.printf "%a\n?@." sequent (env, a);
  let cmd, arg = parse_cli () in

  match cmd, a with
  | ("back" | "undo" | "cancel"), _ -> raise E

  | "intro", TNat -> Zero

  | "intro", TArrow (a', b')->
      if arg = "" then error ~env ~a (arg_for cmd) else
      (try
        let t = proove ((arg,a')::env) b' in
        Abs (arg, a', t)
      with E -> proove env a)

  | "intro", TPair (a', b')->
      (try
        let ta = proove (env) (a') in
        let tb = proove (env) (b') in
        Pair (ta, tb)
      with E -> proove env a)

  | "intro", T1 -> Unit

  | "left", TPlus (l, r) ->
      (try
        let ll = proove env l in
        Left (r, ll)
      with E -> proove env a)

  | "right", TPlus (l, r) ->
      (try
        let rr = proove env r in
        Right (l, rr)
      with E -> proove env a)

  | "elim", _ -> (* (f : A => B, ... |- B)  '->  (... |- A) *)
      if arg = "" then error ~env ~a (arg_for cmd) else
      (try
        match List.assoc arg env with
          | T0 -> Case_ (a, Var arg)
          | TArrow (fa, fb) when fb = a ->
              let u = proove env fa in
              App (Var arg, u)
          | TPlus (l, r) ->
              let arg1, arg2 = arg ^ "1", arg ^ "2" in
              Format.printf "[Cas %s:%a]\n" arg1 Printer.ty l;
              let t1 = proove env (TArrow(l, a)) in
              Format.printf "[Cas %s:%a]\n" arg2 Printer.ty r;
              let t2 = proove env (TArrow(r, a)) in
              Case (Var arg, t1, t2)
              (* Abs(arg1, l, t1), Abs(arg2, r, t2)) *)
          | _ -> error ~env ~a "Not the right type"
      with Not_found -> error ~env ~a (Format.sprintf "Hypothesis %s doesn't exist" arg)
          | E -> proove env a)

  | "cut", _ ->
      (try
        if arg = "" then error ~env ~a (arg_for "cut") else
        let targ = mk_typ arg in
        Format.printf "[Cut sur %a]\n" Printer.ty targ;
        let l = proove env (TArrow (targ, a)) in
        Format.printf "[On prouve %a pour le cut fait plus haut]\n" Printer.ty targ;
        let r = proove env targ in
        App(l, r)
      with E -> proove env a)

  | ("fst"|"snd"), _ ->
      if arg = "" then error ~env ~a (arg_for cmd) else
      (try
        let env' = List.remove_assoc arg env in
        match List.assoc arg env with
          | TPair (l, _) when cmd = "fst" -> Fst (proove ((arg, l)::env') a)
          | TPair (_, r) when cmd = "snd" -> Snd (proove ((arg, r)::env') a)
          | _ -> error ~env ~a "Not the right type"
      with Not_found -> error ~env ~a (Format.sprintf "Hypothesis %s doesn't exist" arg)
         | E -> proove env a)

  | "exact", _ ->
      let t = mk_term arg in
      if infer_type env t = a then t
      else error ~env ~a "Not the right type."

  | "use", _ ->
      (try
        if arg = "" then error ~env ~a (arg_for cmd) else
        let f = open_in arg in
        let cmds' = ref [] in
        try
          while true do
            let c = input_line f in
            cmds' := c :: !cmds'
          done;
          assert false
        with End_of_file ->
          cmds := List.rev_append !cmds' !cmds;
          proove env a
      with Sys_error e -> error ~env ~a (Format.sprintf "Bad filename : %s" e))

  | "intro", _ ->
      error ~env ~a "Don't know how to introduce this."

  | ("trustme"|"magic"), _ -> Trustme (Some a)

  | cmd, _ ->
      error ~env ~a  (Format.sprintf "Unknown command: %s" cmd)

and error e ~env ~a = print_endline e; proove env a

let proove o =
  match o with
  | None ->
      Format.printf "Please enter the formula to prove:@.";
      let t = Maker.mk_typ @@ input_line stdin in
      Format.printf "Let's prove it.@.";
      proove [] t, t
  | Some filename ->
      channel := open_in filename;
      print_req := true;
      let t = Maker.mk_typ @@ input_line !channel in
      Format.printf "Let's prove %a.@." Printer.ty t;
      let r = proove [] t, t in
      close_in !channel;
      r
