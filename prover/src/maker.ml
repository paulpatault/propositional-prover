let mk_typ s =
  let lexbuf = Lexing.from_string (s ^ "$") in
  Uparser.type_only Ulexer.scan_token lexbuf

let mk_term s =
  let lexbuf = Lexing.from_string (s ^ "$") in
  Uparser.term Ulexer.scan_token lexbuf

