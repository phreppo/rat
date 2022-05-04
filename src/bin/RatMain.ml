open Rat

module Options = struct
  let semantics = ref ParserRe.Match
  let separator = "~ ⟦🐁⟧ ~" |> Utility.blue_str
  let timeout = ref 30
  let exploit_max_len = ref 128
  let to_analyze = ref None
  let show_lang = ref false
end

exception Timeout

let set_match_semantics = function
  | "match" -> Options.semantics := Match
  | "fullmatch" -> Options.semantics := Fullmatch
  | _ -> raise (Arg.Bad "wrong argument")

let spec_list =
  [
    ( "--regex",
      Arg.String (fun s -> Options.to_analyze := Some s),
      "is the regex to analyze." );
    ( "--semantics",
      Arg.Symbol ([ "match"; "fullmatch" ], set_match_semantics),
      " is the matching semantics used for the analysis." );
    ( "--timeout",
      Arg.Set_int Options.timeout,
      "sets the timeout for the analysis in seconds." );
    ( "--exploit-max-len",
      Arg.Set_int Options.exploit_max_len,
      "sets the max length of the exploit strings." );
    ( "--show-lang",
      Arg.Set Options.show_lang,
      "prints the attack language if a regex is dangerous." );
  ]

let rec main () =
  Arg.parse spec_list (fun _ -> ignore ()) "Usage: rat [--regex REGEX]";
  match !Options.to_analyze with
  | None -> repl ()
  | Some regex -> run_analysis regex

and parse source =
  let lexbuf = Lexing.from_string source in
  try Parser.main Lexer.token lexbuf
  with exn ->
    let curr = lexbuf.lex_curr_p in
    let line = curr.pos_lnum in
    let cnum = curr.pos_cnum - curr.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Printf.eprintf "Parse error: %s: line %d, column %d, token \"%s\"\n"
      (Printexc.to_string exn) line cnum tok;
    exit 1

and repl _ =
  print_endline (Utility.yellow_str "~ rat's interactive mode ~");
  print_endline "Press Ctrl+D to exit\n";
  try
    while true do
      read_eval_print ()
    done
  with End_of_file -> print_endline "Bye!"

and read_eval_print _ =
  print_string (Utility.blue_str "🐁 → ");
  let source = read_line () in
  parse_eval_print source

and run_analysis source =
  print_endline Options.separator;
  parse_eval_print source

and parse_eval_print source =
  let regex = parse source in
  match ParserRe.to_re ~semantics:!Options.semantics regex with
  | Error e ->
      ParserRe.report_conversion_error Format.std_formatter e;
      exit 1
  | Ok regex -> (
      match analyze_with_timeout regex !Options.timeout with
      | `Timeout -> print_endline "Analysis timeout expired."
      | `Done (attack, runtime) -> print runtime attack)

(** [analyze_with_timeout regex timeout] runs the analysis on [regex] with a
    timeout of [timeout] seconds. *)
and analyze_with_timeout regex timeout =
  let _ =
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
  in
  ignore (Unix.alarm timeout);
  try
    let r = analyze_with_runtime regex in
    ignore (Unix.alarm 0);
    `Done r
  with e ->
    ignore (Unix.alarm 0);
    `Timeout

and analyze_with_runtime regex =
  let initial_time = Sys.time () in
  let attack = Analysis.analyze regex in
  let runtime = (Sys.time () -. initial_time) *. 1000.0 in
  (attack, runtime)

and print runtime attack =
  let exp_vulnerable = not (Analysis.AttackFamilySet.is_empty attack) in
  (if not exp_vulnerable then print_endline "Exponential ReDoS: false"
  else
    match get_exploit_string attack with
    (* We cannot infer a printable exploit string. *)
    | None -> print_endline "Exponential ReDoS: false"
    (* We have a printable exploit string. *)
    | Some exploit ->
        print_endline "Exponential ReDoS: true";
        (if !Options.show_lang then
         (* Print the whole language. *)
         let exp_vuln_string = Analysis.AttackFamilySet.to_string attack in
         print_endline ("Exponential Vulnerabilities: " ^ exp_vuln_string));
        print_exploit exploit);
  print_runtime runtime

(** [get_exploit_string attack] is an exploit string for [attack] such that
    every character in it is printable.*)
and get_exploit_string attack =
  Analysis.AttackFamilySet.get_exploit_string attack Utility.chr_is_printable

and print_exploit exploit =
  print_string "Exploit string: ";
  Analysis.ExploitString.to_string exploit |> print_endline;
  let exploit_example =
    Analysis.ExploitString.generate !Options.exploit_max_len exploit
  in
  print_endline ("Example: " ^ exploit_example)

and print_runtime runtime =
  print_endline ("Runtime(ms): " ^ string_of_float runtime)

let _ = main ()
