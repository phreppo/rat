type parse_error = string

let parse source : (ParserRe.t, parse_error) result =
  let lexbuf = Lexing.from_string source in
  try Ok (Parser.main Lexer.token lexbuf)
  with exn ->
    let curr = lexbuf.lex_curr_p in
    let line = curr.pos_lnum in
    let cnum = curr.pos_cnum - curr.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Error
      (Printf.sprintf "Parse error: %s: line %d, column %d, token \"%s\""
         (Printexc.to_string exn) line cnum tok)

let report_parse_error (fmt : Format.formatter) (err : parse_error) : unit =
  Format.pp_print_string fmt err
