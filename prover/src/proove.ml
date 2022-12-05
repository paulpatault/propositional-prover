open Format

let () =
  let arg = try Some Sys.argv.(1) with Invalid_argument _ -> None in
  let t, typ = Proving.proove arg in

  printf "done.\nProof term is : %a\nand should be of type : %a\n" Printer.term t Printer.ty typ;
  printf "Typechecking...@.";

  try
    Stlc.check_type [] t typ;
    printf "ok.@."
  with Types.Type_error ->
    printf "not ok.@."
