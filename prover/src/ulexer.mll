{
  open Uparser
  exception Eof

  let keywords = Hashtbl.create 97
  let () =
    List.iter
      (fun (x,y) -> Hashtbl.add keywords x y)
      [
         "fst"   , FST;
         "snd"   , SND;
         "T"     , TRUE;
         "left"  , LEFT;
         "right" , RIGHT;
         "case"  , CASE;
         "of"    , OF;
         "absurd", ABSURD;
         "fun"   , FUN;
         "and"   , LAND;
         "or"    , LOR;
         "not"   , NOT;
         "T"     , ONE;
         "true"  , ONE;
         "zero"  , NATZERO;
         "z"     , NATZERO;
         "succ"  , SUCC;
         "s"     , SUCC;
         "nat"   , TNAT;
      ]
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
(* let num = ['2'-'9'] ['0'-'9']* *)
let ident = alpha (alpha | digit | '_' | '\'')*

rule scan_token = parse
  | [' ' '\t' '\r' '\n']+ { scan_token lexbuf }
  | ident as id     { try Hashtbl.find keywords id with Not_found -> IDENT id }
  | "1"      { ONE }
  | "0"      { ZERO }
  (* | '\n'     { EOF } *)
  | '$'      { EOF }
  | '('      { LPAR }
  | ')'      { RPAR }
  | '{'      { LBRAC }
  | '}'      { RBRAC }
  | "=>"     { TARROW }
  | "->"     { ARROW }
  | "/\\"    { LAND }
  | "\\/"    { LOR }
  | ","      { COMMA }
  | ":"      { COLON }
  | "|"      { BAR }
  | eof      { raise Eof }
  | _ as c   { failwith (Printf.sprintf "-> invalid character: %c" c) }
{
}
