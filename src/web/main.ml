open Rat
open Js_of_ocaml

let parse source =
  let lexbuf = Lexing.from_string source in
  Parser.main Lexer.token lexbuf

let analyze_regex source semantics =
  let sem =
    match semantics with
    | "fullmatch" -> ParserRe.Fullmatch
    | _ -> ParserRe.Match
  in
  let parsed = parse source in
  match ParserRe.to_re ~semantics:sem parsed with
  | Error e ->
      let buf = Buffer.create 64 in
      let fmt = Format.formatter_of_buffer buf in
      ParserRe.report_conversion_error fmt e;
      Format.pp_print_flush fmt ();
      let obj = Js.Unsafe.obj [||] in
      Js.Unsafe.set obj (Js.string "error") (Js.string (Buffer.contents buf));
      obj
  | Ok regex ->
      let attack = Analysis.analyze regex in
      let vulnerable = not (Analysis.AttackFamilySet.is_empty attack) in
      let obj = Js.Unsafe.obj [||] in
      Js.Unsafe.set obj (Js.string "vulnerable") (Js.bool vulnerable);
      if vulnerable then begin
        let lang = Analysis.AttackFamilySet.to_string attack in
        Js.Unsafe.set obj (Js.string "attackLanguage") (Js.string lang);
        match
          Analysis.AttackFamilySet.get_exploit_string attack
            Utility.chr_is_printable
        with
        | None -> ()
        | Some exploit ->
            let desc = Analysis.ExploitString.to_string exploit in
            let example = Analysis.ExploitString.generate 128 exploit in
            Js.Unsafe.set obj (Js.string "exploit") (Js.string desc);
            Js.Unsafe.set obj (Js.string "example") (Js.string example)
      end;
      obj

let () =
  Js.export_all
    (object%js
       method analyze source semantics =
         analyze_regex
           (Js.to_string source)
           (Js.to_string semantics)
    end)
