let bold  = "\027[1m"
let reset = "\027[0m"
let red   = "\027[31m"

let (++) = Alpha_beta.(++)
let (+++) = Alpha_beta.(+++)

let split c s =
  try
    let n = String.index s c in
    String.trim (String.sub s 0 n), String.trim (String.sub s (n+1) (String.length s - (n+1)))
  with Not_found -> s, ""

