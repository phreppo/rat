open Rat
open Js_of_ocaml

let has_redos_js source =
  match Analysis.has_redos source with
  | Analysis.Safe -> Js.string "Safe"
  | Analysis.Dangerous -> Js.string "Dangerous"
  | Analysis.ParseError -> Js.string "ParseError"

let () =
  Js.export_all
    (object%js method _hasRedos source = has_redos_js (Js.to_string source) end)
