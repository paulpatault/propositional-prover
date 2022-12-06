open Types
open Parselex
open Typing
open Format
open Utils

let (++) ref_ctx pair = Alpha_beta.(++) !ref_ctx pair

let interactive_loop =
  let ctx = ref [] in
  let loop = ref true in
  let file = open_out "interactive.proof" in
  let split c s =
    try
      let n = String.index s c in
      String.trim (String.sub s 0 n), String.trim (String.sub s (n+1) (String.length s - (n+1)))
    with Not_found -> s, ""
  in
  while !loop do
    try
      printf "? @?";
      flush_all ();
      let cmd, arg =
        let cmd = input_line stdin in
        output_string file (cmd ^ "\n");
        printf "%s@." cmd;
        split ' ' cmd
      in
      match cmd with
      | "assume" ->
          let x, sa = split ':' arg in
          let a = parse sa in
          check !ctx a Type;
          ctx := ctx ++ (x,a);
          printf "→ %s%s%s assumed of type %s%a%s@." bold x reset bold pp_expr a reset
      | "define" ->
          let x, st = split '=' arg in
          let t = parse st in
          let a = infer !ctx t in
          ctx := (x, (a, Some t)) :: !ctx;
          printf "→ %s%s%s define to %s%a%s of type %s%a%s@." bold x reset bold pp_expr t reset bold pp_expr a reset
      | "context" ->
          printf "→ %a@." context !ctx
      | "type" ->
          let t = parse arg in
          let a = infer !ctx t in
          printf "→ %s%a%s is of type %s%a%s@." bold pp_expr t reset bold pp_expr a reset
      | "check" ->
          let t, a = split '=' arg in
          let t = parse t in
          let a = parse a in
          check !ctx t a;
          printf "→ Ok.@."
      | "eval" ->
          let t = parse arg in
          let _ = infer !ctx t in
          printf "→ %s%a%s@." bold pp_expr (Alpha_beta.normalize !ctx t) reset
      | "exit" -> loop := false
      | "" | "#" -> ()
      | cmd -> printf "Unknown command: %s@." cmd
    with
    | End_of_file -> loop := false
    | Failure err -> printf "%s%sInternal error:%s %s.@." bold red reset err
    | Type_error (e1, t1, t2) ->
        printf "%s%sType Error:%s\n   the term   %s%a%s\n   has type   %s%a%s\n   instead of %s%a%s@."
          bold red reset
          bold pp_expr e1 reset
          bold pp_expr t1 reset
          bold pp_expr t2 reset
    | Type_error_ ->  printf "%s%sType Error%s@." bold red reset
    | Free_var v -> printf "%s%sError: %s the variable %s%s%s is free.@." bold red reset bold v reset
  done;
  printf "Bye.@."
