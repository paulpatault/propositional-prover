open Types
open Parselex
open Typing
open Format
open Utils

let file = open_out "interactive.proof"

let rec read_file ?(acc=[]) f =
  let l = input_line f in
  let acc = if l = "exit" then acc else l :: acc in
  try
    read_file f ~acc
  with End_of_file -> List.rev acc

let rec loop ?(lines = []) ctx =
  try
    printf "? @?";
    flush_all ();
    let (cmd, arg), lines =
      let cmd, lines = match lines with
        | [] -> input_line stdin, []
        | e::k -> e, k in
      output_string file (cmd ^ "\n");
      printf "%s@." cmd;
      split ' ' cmd, lines
    in
    match cmd with
    | "assume" ->
        let x, sa = split ':' arg in
        let a = parse sa in
        check ctx a Type;
        printf "→ %s%s%s assumed of type %s%a%s@." bold x reset bold pp_expr a reset;
        loop (ctx ++ (x,a)) ~lines
    | "define" ->
        let x, st = split '=' arg in
        let t = parse st in
        let a = infer ctx t in
        printf "→ %s%s%s define to %s%a%s of type %s%a%s@." bold x reset bold pp_expr t reset bold pp_expr a reset;
        loop (ctx +++ (x, a, t)) ~lines
    | "context" ->
        printf "→ %a@." context ctx;
        loop ctx ~lines
    | "type" ->
        let t = parse arg in
        let a = infer ctx t in
        printf "→ %s%a%s is of type %s%a%s@." bold pp_expr t reset bold pp_expr a reset;
        loop ctx ~lines
    | "check" ->
        let t, a = split '=' arg in
        let t = parse t in
        let a = parse a in
        check ctx t a;
        printf "→ Ok.@.";
        loop ctx ~lines
    | "check_val" ->
        let t1, t2 = split '=' arg in
        let t1 = parse t1 in
        let t2 = parse t2 in
        let _ = infer ctx t1 in
        let _ = infer ctx t2 in
        if Alpha_beta.alpha_beta_convertible ~ctx t1 t2 then
          printf "→ Ok.@."
        else printf "→ Not ok.@.";
        loop ctx ~lines
    | "eval" ->
        let t = parse arg in
        let _ = infer ctx t in
        printf "→ %s%a%s@." bold pp_expr (Alpha_beta.normalize ctx t) reset;
        loop ctx ~lines
    | "read" ->
        let f = open_in arg in
        let lines = read_file f in
        loop ~lines ctx
    | "exit" -> ()
    | "" | "#" -> loop ctx ~lines
    | cmd -> printf "Unknown command: %s@." cmd
  with
  | End_of_file -> ()
  | Failure err -> printf "%s%sInternal error:%s %s.@." bold red reset err; loop ctx ~lines
  | Type_error (e1, t1, t2) ->
      printf "%s%sType Error:%s\n   the term   %s%a%s\n   has type   %s%a%s\n   instead of %s%a%s@."
        bold red reset
        bold pp_expr e1 reset
        bold pp_expr t1 reset
        bold pp_expr t2 reset;
      loop ctx ~lines
  | Type_error_ ->
      printf "%s%sType Error%s@." bold red reset;
      loop ctx ~lines
  | Free_var v ->
      printf "%s%sError: %s the variable %s%s%s is free.@." bold red reset bold v reset;
      loop ctx ~lines

let interactive_loop =
  loop [];
  printf "Bye.@."
