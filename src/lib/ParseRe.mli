(** Parsing strings into {!ParserRe.t}. *)

type parse_error
(** Returned when it is not possible to parse a string into a {!ParserRe.t}. *)

val parse : string -> (ParserRe.t, parse_error) result
(** [parse s] is the regular expression represented by [s]. If it is not
    possible to parse [s], then a {!parse_error} is returned instead. *)

val report_parse_error : Format.formatter -> parse_error -> unit
(** [report_parse_error fmt e] pretty-prints [e] to [fmt]. *)
