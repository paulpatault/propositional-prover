open Types
open Parselex
open Typing
open Format

let (++) ref_ctx pair = Alpha_beta.(++) !ref_ctx pair

let interactive_loop = fun () ->
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
      print_string "? ";
      flush_all ();
      let cmd, arg =
        let cmd = input_line stdin in
        output_string file (cmd^"\n");
        print_endline cmd;
        split ' ' cmd
      in
      match cmd with
      | "assume" ->
          let x, sa = split ':' arg in
          let a = parse sa in
          check !ctx a Type;
          ctx := ctx ++ (x,a);
          printf "%s assumed of type %a@." x pp_expr a
      | "define" ->
          let x, st = split '=' arg in
          let t = parse st in
          let a = infer !ctx t in
          ctx := (x, (a, Some t)) :: !ctx;
          printf "%s define to %a of type %a@." x pp_expr t pp_expr a
      | "context" ->
          printf "%a@." context !ctx
      | "type" ->
          let t = parse arg in
          let a = infer !ctx t in
          printf "%a is of type %a@." pp_expr t pp_expr a
      | "check" ->
          let t, a = split '=' arg in
          let t = parse t in
          let a = parse a in
          check !ctx t a;
          printf "Ok.@."
      | "eval" ->
          let t = parse arg in
          let _ = infer !ctx t in
          printf "%a@." pp_expr (Alpha_beta.normalize !ctx t)
      | "exit" -> loop := false
      | "" | "#" -> ()
      | cmd -> printf "Unknown command: %s@." cmd
    with
    | End_of_file -> loop := false
    | Failure err -> printf "Error: %s.@." err
  done;
  printf "Bye.@."
