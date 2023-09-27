open Utility.ResultSyntax

(** {1 Representation of Parser Regexes.} *)

type flags = string

type body =
  | Epsilon
  | Char of Charset.t
  | Concat of body * body
  | Alternative of body * body
  | Star of body
  | Backreference of int
  | Utf8
  | Dollar
  | WordBoundary  (** [\b] *)
  | EndOfString  (** [\Z] *)

type t = body * flags

let eps = Epsilon
let ch c = Char c
let from_char c = Char (Charset.singleton c)
let choice e1 e2 = Alternative (e1, e2)
let star e1 = match e1 with Epsilon -> Epsilon | _ -> Star e1

let concat e1 e2 =
  match (e1, e2) with
  | Epsilon, _ -> e2
  | _, Epsilon -> e1
  | _ -> Concat (e1, e2)

let plus e1 = concat e1 (star e1)
let any_char = Char Charset.all_symbols
let any_digit = Char Charset.digits
let any_non_digit = Char Charset.non_digits
let any_space = Char Charset.spaces
let any_non_space = Char Charset.non_spaces
let any_word_char = Char Charset.word_chars
let any_non_word_char = Char Charset.non_word_chars

let rec repeat r n =
  if n = 0 then eps else if n > 0 then concat r (repeat r (n - 1)) else r

(** [repeat_from_until re from to] is [re ^ from | re ^ from+1 | ... | re ^ to].
    Fails if [from] > [to]. *)
let repeat_from_until r from until =
  if from > until then
    failwith
      ("Can't build repetition: " ^ string_of_int from ^ " is bigger than "
     ^ string_of_int until)
  else
    let rec make_list from until =
      if from > until then [] else from :: make_list (from + 1) until
    in
    List.fold_left
      (fun acc elem -> choice acc (repeat r elem))
      (repeat r from)
      (make_list (from + 1) until)

let backreference i = Backreference i
let utf8 = Utf8
let dollar = Dollar
let word_boundary = WordBoundary
let end_of_string = EndOfString
let universal_lang = any_char |> star

(** {1 Conversion to Syntactic Regexes} *)

(** {2 Errors in the conversion} *)

(** Type of the conversion error. *)
type conversion_error =
  | UsedBackreference
  | UsedUtf8
  | UnkownFlag of char
  | UsedDollarMiddleOfRe
  | UsedWordBoundaryMiddleOfRe
  | UsedEndOfStringMiddleOfRe

let report_conversion_error (fmt : Format.formatter) (err : conversion_error) :
    unit =
  match err with
  | UsedBackreference ->
      Format.pp_print_string fmt "Backreferences are not supported.\n"
  | UsedUtf8 -> Format.pp_print_string fmt "UTF-8 is not supported.\n"
  | UnkownFlag flag -> Format.fprintf fmt "Flag '%c' is not supported.\n" flag
  | UsedDollarMiddleOfRe ->
      Format.pp_print_string fmt "'$' is supported only at the end of a regex."
  | UsedWordBoundaryMiddleOfRe ->
      Format.pp_print_string fmt
        "'\\b' is supported only at the end of a regex."
  | UsedEndOfStringMiddleOfRe ->
      Format.pp_print_string fmt
        "'\\Z' is supported only at the end of a regex."

(** {1 Functions to perform the conversion} *)

type semantics = Match | Fullmatch

(** [last_construct_blocks_universal_language re] is [true] iff the last
    construct in [re] blocks the matching semantics to match the universal
    language at the end of [re]. For example,
    [last_construct_blocks_universal_language "ab$"] is [true], but
    [last_construct_blocks_universal_language "$a*"] is [false]. *)
let rec last_construct_blocks_universal_language = function
  | Dollar | WordBoundary | EndOfString -> true
  | Epsilon | Char _ | Alternative _ | Star _ | Backreference _ | Utf8 -> false
  | Concat (_, re2) -> last_construct_blocks_universal_language re2

(** [remove_some_advanced_features re] removes from [re] some advanced features,
    such as [$], [\Z] and [\b]. If this is not possible returns an error (for
    example, if [$] appears in the middle of a regex), otherwise returns the new
    regular expression. *)
let remove_some_advanced_features input : (body, conversion_error) result =
  let rec remove_advanced_features_rec is_last input =
    match input with
    | Epsilon | Char _ | Backreference _ | Utf8 -> Ok input
    | Concat (re1, re2) ->
        let* re1' = remove_advanced_features_rec false re1 in
        let* re2' = remove_advanced_features_rec is_last re2 in
        Ok (concat re1' re2')
    | Alternative (re1, re2) ->
        let* re1' = remove_advanced_features_rec false re1 in
        let* re2' = remove_advanced_features_rec false re2 in
        Ok (choice re1' re2')
    | Star re ->
        let* re' = remove_advanced_features_rec false re in
        Ok (star re')
    (* If the dollar is at the end of the regex, then it is possible to safely
       remove it. *)
    | Dollar -> if is_last then Ok Epsilon else Error UsedDollarMiddleOfRe
    | WordBoundary ->
        if is_last then Ok Epsilon else Error UsedWordBoundaryMiddleOfRe
    | EndOfString ->
        if is_last then Ok Epsilon else Error UsedEndOfStringMiddleOfRe
  in
  remove_advanced_features_rec true input

(** [remove_blocking_nonregular_constructs semantics re] is [re] without some
    nonregular constructs that block the matching at the end of the regular
    expression. [semantics] is the semantics that assumed for the matching: if
    it is [Fullmatch], then the semantics is the language membership, otherwise
    it is the regular [Match] semantics of matching engines. If [semantics] is
    [Match], then the universal language is appended at the end of the regex,
    but only if the blocking constructs do not appear at the end of the
    concatenation of the regular expression. *)
let remove_blocking_nonregular_constructs semantics r =
  let block = last_construct_blocks_universal_language r in
  let* r1 = remove_some_advanced_features r in
  if semantics = Match && not block then Ok (concat r1 universal_lang)
  else Ok r1

let rec to_re_body input : (Re.t, conversion_error) result =
  match input with
  (* Unsupported constructs. *)
  | Backreference _ -> Error UsedBackreference
  | Utf8 -> Error UsedUtf8
  (* Regular constructs. *)
  | Epsilon -> Ok Re.eps
  | Char cs -> Ok (Re.ch cs)
  | Concat (r1, r2) ->
      let* r1' = to_re_body r1 in
      let* r2' = to_re_body r2 in
      Ok (Re.concat r1' r2')
  | Alternative (r1, r2) ->
      let* r1' = to_re_body r1 in
      let* r2' = to_re_body r2 in
      Ok (Re.choice r1' r2')
  | Star r ->
      let* r' = to_re_body r in
      Ok (Re.star r')
  | Dollar ->
      failwith
        "dollar construct reached [to_re_body] function. This should not be \
         possible."
  | WordBoundary ->
      failwith
        "word boundary construct reached [to_re_body] function. This should \
         not be possible."
  | EndOfString ->
      failwith
        "end of string construct reached [to_re_body] function. This should \
         not be possible."

let apply_flag (re : Re.t) = function
  (* Properly supported flags. *)
  (* Global: do not return after first match. *)
  | 'g' -> Ok re
  (* Multiline: ^ and $ match start/end of the line. Meaningless for us, since
     they cannot appear in the middle of a regex. *)
  | 'm' -> Ok re
  (* Insensitive: matching is case-insensitive. *)
  | 'i' -> Ok (Re.case_insensitive re)
  (* ASCII: match ASCII-only characters. It is the default behaviour of rat. *)
  | 'a' -> Ok re
  (* Single line: the dot character matches the newline character. It is the
     default behaviour. *)
  | 's' -> Ok re
  (* TODO: flags to support properly. *)
  | 'x' | 'U' | 'H' | 'R' | 'P' | 'O' -> Ok re
  | flag -> Error (UnkownFlag flag)

let rec apply_flags_list re flags =
  match flags with
  | [] -> Ok re
  | flag :: flags' ->
      let* re' = apply_flag re flag in
      apply_flags_list re' flags'

let apply_flags (re : Re.t) (flags : flags) : (Re.t, conversion_error) result =
  apply_flags_list re (Utility.explode flags)

let to_re ?(semantics = Match) (input : t) : (Re.t, conversion_error) result =
  let r, modifiers = input in
  let* r1 = remove_blocking_nonregular_constructs semantics r in
  let* r2 = to_re_body r1 in
  let* r3 = apply_flags r2 modifiers in
  Ok r3
