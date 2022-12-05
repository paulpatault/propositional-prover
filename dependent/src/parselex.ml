open Types

let () = Printexc.record_backtrace true

exception Parse_error

let lexer =
  Genlex.make_lexer
    ["("; ","; ")"; "->"; "=>"; "fun"; "Pi"; ":"; "Type"; "Nat"; "Z"; "S"; "Ind"; "Eq"; "Refl"; "J"]

let of_tk s =
  let must_kwd s k = match Stream.next s with Genlex.Kwd k' when k' = k -> () | _ -> raise Parse_error in
  let peek_kwd s k = match Stream.peek s with Some (Genlex.Kwd k') when k' = k -> let _ = Stream.next s in true | _ -> false in
  let stream_is_empty s = try Stream.empty s; true with Stream.Failure -> false in
  let ident s = match Stream.next s with Genlex.Ident x -> x | _ -> raise Parse_error in
  let noapp = List.map (fun k -> Some (Genlex.Kwd k)) [")"; "->"; "=>"; "fun"; "Pi"; ":"; "Type"] in

  let rec e () = abs ()

  and abs () =
    if peek_kwd s "Pi" then
      (
        must_kwd s "(";
        let x = ident s in
        must_kwd s ":";
        let a = e () in
        must_kwd s ")";
        must_kwd s "->";
        let b = e () in
        Pi (x, a, b)
      )
    else if peek_kwd s "fun" then
      (
        must_kwd s "(";
        let x = ident s in
        must_kwd s ":";
        let a = e () in
        must_kwd s ")";
        must_kwd s "->";
        let t = e () in
        Abs (x, a, t)
      )
    else
      app ()

  and app () =
    let t = ref (arr ()) in
    while not (stream_is_empty s) && not (List.mem (Stream.peek s) noapp) do
      t := App (!t, base ())
    done;
    !t
  and arr () =
    let t = base () in
    if peek_kwd s "=>" then
      let u = e () in
      Pi ("_", t, u)
    else
      t

  and base () =
    match Stream.next s with
    | Genlex.Ident x -> Var x
    | Genlex.Kwd "Type" -> Type
    | Genlex.Kwd "Nat" -> Nat
    | Genlex.Kwd "Z" -> Z
    | Genlex.Kwd "S" ->
        let t = base () in
        S t
    | Genlex.Kwd "Ind" ->
        let p = base () in
        let z = base () in
        let ps = base () in
        let n = base () in
        Ind (p, z, ps, n)
    | Genlex.Kwd "Eq" ->
        let t = base () in
        let u = base () in
        Eq (t, u)
    | Genlex.Kwd "Refl" ->
        let t = base () in
        Refl t
    | Genlex.Kwd "J" ->
        let p = base () in
        let r = base () in
        let x = base () in
        let y = base () in
        let e = base () in
        J (p, r, x, y, e)
    | Genlex.Kwd "(" ->
        let t = e () in
        must_kwd s ")";
        t
    | _ -> raise Parse_error
  in
  e ()

let parse a = of_tk (lexer (Stream.of_string a))
