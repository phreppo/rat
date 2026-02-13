open Rat
open Analysis

let attack_lang_from prefix pump suffix =
  AttackFamilySet.singleton { prefix; pump = ExtRe.plus pump; suffix }
  |> AttackFamilySet.to_lang

(* atoms *)
let empty = ExtRe.empty
let a = ExtRe.chr (Charset.singleton 'a')
let aa = ExtRe.concat a a
let b = ExtRe.chr (Charset.singleton 'b')
let ab = ExtRe.concat a b
let c = ExtRe.chr (Charset.singleton 'c')
let up_a = ExtRe.chr (Charset.singleton 'A')
let a_or_b = ExtRe.alternative a b
let a_star = ExtRe.star a
let b_star = ExtRe.star b
let c_star = ExtRe.star c
let ab_star = ExtRe.star ab
let a_or_b_star = ExtRe.star a_or_b
let a_plus = ExtRe.plus a
let aa_plus = ExtRe.plus aa
let b_plus = ExtRe.plus b
let ab_plus = ExtRe.plus ab
let ab_a_or_b_star = ExtRe.concat ab a_or_b_star
let a_b_star = ExtRe.concat a b_star
let a_b_star_c_star = ExtRe.concat a_b_star c_star
let a_c_star = ExtRe.concat a c_star
let a_c_star_b_star = ExtRe.concat a_c_star b_star
let a_or_A = ExtRe.alternative a up_a
let a_or_A_star = ExtRe.star a_or_A
let a_or_A_plus = ExtRe.plus a_or_A

let a_b_star_c_star_or_a_c_star_b_star =
  ExtRe.alternative a_b_star_c_star a_c_star_b_star

let a_b_star_c_star_or_a_c_star_b_star_star =
  ExtRe.star a_b_star_c_star_or_a_c_star_b_star

let any_char = ExtRe.chr Charset.all_symbols
let universal = ExtRe.universal
let one = ExtRe.chr (Charset.singleton '1')
let any_digit = ExtRe.chr Charset.digits
let any_digit_star = ExtRe.star any_digit
let bslash = ExtRe.chr (Charset.singleton '\\')
let bslash_star = ExtRe.star bslash
let bslash_plus = ExtRe.plus bslash
let any_non_digit = ExtRe.chr Charset.non_digits
let any_non_digit_star = ExtRe.star any_non_digit
let any_non_digit_plus = ExtRe.plus any_non_digit
let dot_char = ExtRe.chr (Charset.singleton '.')
let dash_char = ExtRe.chr (Charset.singleton '-')
let slash_char = ExtRe.chr (Charset.singleton '/')
let underscore = ExtRe.chr (Charset.singleton '_')
let space = ExtRe.chr (Charset.singleton ' ')
let any_space = ExtRe.chr Charset.spaces
let any_space_star = ExtRe.star any_space
let any_non_space = ExtRe.chr Charset.non_spaces
let any_non_space_star = ExtRe.star any_non_space
let any_word_char = ExtRe.chr Charset.word_chars
let any_word_char_star = ExtRe.star any_word_char
let any_non_word_char = ExtRe.chr Charset.non_word_chars
let any_non_word_char_star = ExtRe.star any_non_word_char
let upj = ExtRe.chr (Charset.singleton 'J')
let upj_star = ExtRe.star upj
let upj_plus = ExtRe.plus upj

let dot_or_dash_or_slash =
  ExtRe.alternative dot_char dash_char
  |> ExtRe.alternative slash_char
  |> ExtRe.star

let dot_or_dash_or_slash_star = ExtRe.star dot_or_dash_or_slash
let lbrack = ExtRe.chr (Charset.singleton '{')
let rbrack = ExtRe.chr (Charset.singleton '}')

let a_lbrack_1_a_rbrack =
  ExtRe.concat a
    (ExtRe.concat lbrack (ExtRe.concat one (ExtRe.concat a rbrack)))

let a_lbrack_1_a_rbrack_star = ExtRe.star a_lbrack_1_a_rbrack
let a_lbrack_1_a_rbrack_plus = ExtRe.plus a_lbrack_1_a_rbrack

(******************************************************************************)
(*                          Test cases                                        *)
(******************************************************************************)

(* empty regex *)
let empty_src = ""
let empty_att = ExtRe.empty

(* hello|ciao *)
let hello_or_ciao_src = "hello|ciao"
let hello_or_ciao_att = ExtRe.empty

(* a* *)
let a_star_src = "a*"
let a_star_att = ExtRe.empty

(* (a|a)* *)
let a_or_a_star_src = "(a|a)*"
let a_or_a_star_att = attack_lang_from a_star a_plus (ExtRe.compl a_star)
let a_or_a_star_att_match = ExtRe.empty

(* (a|aa)* *)
let a_or_aa_star_src = "(a|aa)*"
let a_or_aa_star_att = attack_lang_from a_star aa_plus (ExtRe.compl a_star)
let a_or_aa_star_att_match = ExtRe.empty

(* (a|a)*b *)
let a_or_a_star_b_src = "(a|a)*b"

let a_or_a_star_b_att =
  attack_lang_from a_star a_plus (ExtRe.compl (ExtRe.concat a_star b))

let a_or_a_star_b_att_match =
  attack_lang_from a_star a_plus
    (ExtRe.compl (universal |> ExtRe.concat b |> ExtRe.concat a_star))

(* (a|b)* *)
let a_or_b_star_src = "(a|b)*"
let a_or_b_star_att = ExtRe.empty

(* (ba* )* *)
let b_a_star_star_src = "(ba*)*"
let b_a_star_star_att = ExtRe.empty

(* (a|b|ab)* *)
let a_or_b_or_ab_star_src = "(a|b|ab)*"

let a_or_b_or_ab_star_att =
  attack_lang_from a_or_b_star ab_a_or_b_star (ExtRe.compl a_or_b_star)

let a_or_b_or_ab_star_att_match = ExtRe.empty

(* a** *)
let a_star_star_src = "(a*)*"
let a_star_star_att = attack_lang_from a_star aa_plus (ExtRe.compl a_star)

(* a*+ *)
let a_star_plus_src = "(a*)+"
let a_star_plus_att = attack_lang_from a_star aa_plus (ExtRe.compl a_star)

(* (ab*c*|ac*b* )* *)
let a_b_star_c_star_or_a_c_star_b_star_star_src = "(ab*c*|ac*b*)*"

let a_b_star_c_star_or_a_c_star_b_star_star_att =
  let pump =
    ExtRe.alternative
      (ExtRe.concat a_b_star a_b_star_c_star_or_a_c_star_b_star_star)
      (ExtRe.concat a_c_star a_b_star_c_star_or_a_c_star_b_star_star)
  in
  attack_lang_from a_b_star_c_star_or_a_c_star_b_star_star pump
    (ExtRe.compl a_b_star_c_star_or_a_c_star_b_star_star)

(* (ab*b*c)* *)
let a_bstar_bstar_c_star_src = "(ab*b*c)*"

let a_bstar_bstar_c_star_att =
  let a_bstar_bstar_c_star =
    ExtRe.star (ExtRe.concat a (ExtRe.concat b_star (ExtRe.concat b_star c)))
  in
  let pump = ExtRe.concat a (ExtRe.concat b_plus c) in
  attack_lang_from a_bstar_bstar_c_star pump (ExtRe.compl a_bstar_bstar_c_star)

(* (.)* *)
let any_star_src = "(.)*"
let any_star_att = ExtRe.empty

(* (.* )*b *)
let any_star_star_src = "(.*)*b"

let any_star_star_att =
  attack_lang_from universal
    (ExtRe.concat (ExtRe.concat any_char any_char) universal)
    (ExtRe.compl b)

(* (a|.)* *)
let a_or_any_star_src = "(a|.)*"
let a_or_any_star_att = ExtRe.empty

(* (a|.)*b *)
let a_or_any_star_b_src = "(a|.)*b"

let a_or_any_star_b_att =
  attack_lang_from universal (ExtRe.concat a universal) (ExtRe.compl b)

(* (.|.)*b *)
let any_or_any_star_b_src = "(.|.)*b"

let any_or_any_star_b_att =
  attack_lang_from universal (ExtRe.concat any_char universal) (ExtRe.compl b)

(* (\d|1)* *)
let any_dig_or_1_star_src = "(\\d|1)*"

let any_dig_or_1_star_att =
  attack_lang_from any_digit_star
    (ExtRe.concat one any_digit_star)
    (ExtRe.compl any_digit_star)

(* (\d|\d)* *)
let any_dig_or_any_dig_star_src = "(\\d|\\d)*"

let any_dig_or_any_dig_star_att =
  attack_lang_from any_digit_star
    (ExtRe.concat any_digit any_digit_star)
    (ExtRe.compl any_digit_star)

(* (a?)* *)
let a_qmark_star_src = "(a?)*"
let a_qmark_star_att = ExtRe.empty

(* (a|a?)* *)
let a_or_a_qmark_star_src = "(a|a?)*"
let a_or_a_qmark_star_att = attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* (\\|\\?)* *)
let bslash_or_bslash_qmark_star_src = "(\\\\|\\\\?)*"

let bslash_or_bslash_qmark_star_att =
  attack_lang_from bslash_star bslash_plus (ExtRe.compl bslash_star)

(* (#|3?)* *)
let sharp_or_3_qmark_star_src = "(#|3?)*"
let sharp_or_3_qmark_star_att = ExtRe.empty

(* ( |\?)* *)
let space_or_qmark_star_src = "( |\\?)*"
let space_or_qmark_star_att = ExtRe.empty

(* (a|[ab])* *)
let a_or_a_or_b_set_src = "(a|[ab])*"

let a_or_a_or_b_set_att =
  attack_lang_from a_or_b_star
    (ExtRe.concat a a_or_b_star)
    (ExtRe.compl a_or_b_star)

(* (\[|[ab])* *)
let sqlpar_or_a_or_b_set_src = "(\\[|[ab])*"
let sqlpar_or_a_or_b_set_att = ExtRe.empty

(* (\D|\D)* *)
let any_non_digit_or_any_non_digit_star_src = "(\\D|\\D)*"

let any_non_digit_or_any_non_digit_star_att =
  attack_lang_from any_non_digit_star any_non_digit_plus
    (ExtRe.compl any_non_digit_star)

(* ([^]|[^^])* *)
let circ_set_or_not_circ_set_src = "([^]|[^^])*"
let circ_set_or_not_circ_set_att = ExtRe.empty

(* ([\^-^]|[^^])* *)
let circ_to_circ_set_or_not_circ_set_src = "([\\^-^]|[^^])*"
let circ_to_circ_set_or_not_circ_set_att = ExtRe.empty

(* ([a-b]|a)* *)
let a_to_b_or_a_src = "([a-b]|a)*"

let a_to_b_or_a_att =
  attack_lang_from a_or_b_star
    (ExtRe.concat a a_or_b_star)
    (ExtRe.compl a_or_b_star)

(* ([--.]|\\.)* *)
let dash_to_slash_or_dot_star_src = "([--/]|\\.)*"

let dash_to_dot_or_dot_star_att =
  attack_lang_from dot_or_dash_or_slash_star
    (ExtRe.concat dot_char dot_or_dash_or_slash_star)
    (ExtRe.compl dot_or_dash_or_slash_star)

(* (\d|[0-34-9])* *)
let any_dig_or_zero_to_nine_star_src = "(\\d|[0-34-9])*"

let any_dig_or_zero_to_nine_star_att =
  attack_lang_from any_digit_star
    (ExtRe.concat any_digit any_digit_star)
    (ExtRe.compl any_digit_star)

(* (\s| )* *)
let any_space_or_space_star_src = "(\\s| )*"

let any_space_or_space_star_att =
  attack_lang_from any_space_star
    (ExtRe.concat space any_space_star)
    (ExtRe.compl any_space_star)

(* (\S|a)* *)
let any_non_space_or_a_star_src = "(\\S|a)*"

let any_non_space_or_a_star_att =
  attack_lang_from any_non_space_star
    (ExtRe.concat a any_non_space_star)
    (ExtRe.compl any_non_space_star)

(* ([\w]|_)* *)
let any_word_char_or_underscore_star_src = "([\\w]|_)*"

let any_word_char_or_underscore_star_att =
  attack_lang_from any_word_char_star
    (ExtRe.concat underscore any_word_char_star)
    (ExtRe.compl any_word_char_star)

(* ([\wa]|a)* *)
let any_word_char_a_or_a_star_src = "([\\wa]|a)*"

let any_word_char_a_or_a_star_att =
  attack_lang_from any_word_char_star
    (ExtRe.concat a any_word_char_star)
    (ExtRe.compl any_word_char_star)

(* ([\W]|_)* *)
let any_non_word_char_or_underscore_star_src = "([\\W]|_)*"
let any_non_word_char_or_underscore_star_att = ExtRe.empty

(* ([\w]|_)* *)
let any_word_char_star_src = "(\\w)*"
let any_word_char_star_att = ExtRe.empty

(* (\x61|a)* *)
let hex_a_or_a_star_src = "(\\x61|a)*"
let hex_a_or_a_star_att = attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* (\x4A|\4a|J)* *)
let hex_upj_or_hex_upj_star_src = "(\\x4a|\\x4A|J)*"

let hex_upj_or_hex_upj_star_att =
  attack_lang_from upj_star upj_plus (ExtRe.compl upj_star)

(* ^(a|a)*$ *)
let circ_a_or_a_star_dollar_src = "^(a|a)*$"

let circ_a_or_a_star_dollar_att =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

let circ_a_or_a_star_dollar_att_match =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* ^(a|a)* *)
let circ_a_or_a_star_src = "^(a|a)*"
let circ_a_or_a_star_att = attack_lang_from a_star a_plus (ExtRe.compl a_star)
let circ_a_or_a_star_att_match = ExtRe.empty

(* ^(a|a)*$ *)
let a_or_a_star_dollar_src = "(a|a)*$"
let a_or_a_star_dollar_att = attack_lang_from a_star a_plus (ExtRe.compl a_star)

let a_or_a_star_dollar_att_match =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* (a|a{1})* *)
let a_or_a_one_times_star_src = "(a|a{1})*"

let a_or_a_one_times_star_att =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* (a|a{2})* *)
let a_or_a_two_times_star_src = "(a|a{2})*"

let a_or_a_two_times_star_att =
  attack_lang_from a_star aa_plus (ExtRe.compl a_star)

(* (a{1,2})* *)
let a_one_or_two_times_star_src = "(a{1,2})*"

let a_one_or_two_times_star_att =
  attack_lang_from a_star aa_plus (ExtRe.compl a_star)

(* (a|a{1a})* *)
let a_or_error_star_src = "(a|a{1a})*"
let a_or_error_star_att = ExtRe.empty

(* (a\{1a\}|a{1a})* *)
let a_lbrack_1_a_rbrack_or_same_star_src = "(a{1a}|a{1a})*"

let a_lbrack_1_a_rbrack_or_same_star_att =
  attack_lang_from a_lbrack_1_a_rbrack_star a_lbrack_1_a_rbrack_plus
    (ExtRe.compl a_lbrack_1_a_rbrack_star)

(* (a{1,})* *)
let a_1_or_more_times_star_src = "(a{1,})*"

let a_1_or_more_times_star_att =
  attack_lang_from a_star aa_plus (ExtRe.compl a_star)

(* /empty_flags/ *)
let empty_flags_src = "/empty_flags/"
let empty_flags_att = ExtRe.empty

(* /(a|a)*/g *)
let a_or_a_star_global_src = "/(a|a)*/g"
let a_or_a_star_global_att = attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* /(a|A)*/gmi *)
let a_or_A_star_case_insensitive_src = "/(a|A)*/gmi"

let a_or_A_star_case_insensitive_att =
  attack_lang_from a_or_A_star a_or_A_plus (ExtRe.compl a_or_A_star)

(* /(a|a)*/gmi *)
let a_or_a_star_case_insensitive_src = "/(a|a)*/i"

let a_or_a_star_case_insensitive_att =
  attack_lang_from a_or_A_star a_or_A_plus (ExtRe.compl a_or_A_star)

(* (a|b|ab)*c *)
let a_or_b_or_ab_star_c_src = "(a|b|ab)*c"

let a_or_b_or_ab_star_c_att =
  attack_lang_from a_or_b_star ab_a_or_b_star
    (ExtRe.compl (ExtRe.concat a_or_b_star c))

let a_or_b_or_ab_star_c_att_match =
  attack_lang_from a_or_b_star ab_a_or_b_star
    (ExtRe.compl (universal |> ExtRe.concat c |> ExtRe.concat a_or_b_star))

(* ^(a|a)*\b *)
let circ_a_or_a_star_word_boundary_src = "^(a|a)*\\b"

let circ_a_or_a_star_word_boundary_att =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

let circ_a_or_a_star_word_boundary_att_match =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

(* ^(a|a)*\Z *)
let circ_a_or_a_star_end_of_string_src = "^(a|a)*\\Z"

let circ_a_or_a_star_end_of_string_att =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

let circ_a_or_a_star_end_of_string_att_match =
  attack_lang_from a_star a_plus (ExtRe.compl a_star)

let fullmatch_test_pairs =
  [ (empty_src, empty_att)
  ; (b_a_star_star_src, b_a_star_star_att)
  ; (hello_or_ciao_src, hello_or_ciao_att)
  ; (a_star_src, a_star_att)
  ; (a_or_aa_star_src, a_or_aa_star_att)
  ; (a_or_a_star_src, a_or_a_star_att)
  ; (a_or_b_star_src, a_or_b_star_att)
  ; (a_or_b_or_ab_star_src, a_or_b_or_ab_star_att)
  ; (a_star_star_src, a_star_star_att)
  ; (a_star_plus_src, a_star_plus_att)
  ; (a_b_star_c_star_or_a_c_star_b_star_star_src
    ,a_b_star_c_star_or_a_c_star_b_star_star_att)
  ; (a_bstar_bstar_c_star_src, a_bstar_bstar_c_star_att)
  ; (any_star_src, any_star_att)
  ; (any_star_src, any_star_att)
  ; (any_star_star_src, any_star_star_att)
  ; (a_or_any_star_src, a_or_any_star_att)
  ; (a_or_any_star_b_src, a_or_any_star_b_att)
  ; (any_or_any_star_b_src, any_or_any_star_b_att)
  ; (any_dig_or_1_star_src, any_dig_or_1_star_att)
  ; (any_dig_or_any_dig_star_src, any_dig_or_any_dig_star_att)
  ; (a_or_a_qmark_star_src, a_or_a_qmark_star_att)
  ; (a_qmark_star_src, a_qmark_star_att)
  ; (bslash_or_bslash_qmark_star_src, bslash_or_bslash_qmark_star_att)
  ; (sharp_or_3_qmark_star_src, sharp_or_3_qmark_star_att)
  ; (space_or_qmark_star_src, space_or_qmark_star_att)
  ; (a_or_a_or_b_set_src, a_or_a_or_b_set_att)
  ; (sqlpar_or_a_or_b_set_src, sqlpar_or_a_or_b_set_att)
  ; (sqlpar_or_a_or_b_set_src, sqlpar_or_a_or_b_set_att)
  ; (any_non_digit_or_any_non_digit_star_src
    ,any_non_digit_or_any_non_digit_star_att)
  ; (circ_set_or_not_circ_set_src, circ_set_or_not_circ_set_att)
  ; (circ_to_circ_set_or_not_circ_set_src, circ_to_circ_set_or_not_circ_set_att)
  ; (a_to_b_or_a_src, a_to_b_or_a_att)
  ; (dash_to_slash_or_dot_star_src, dash_to_dot_or_dot_star_att)
  ; (any_dig_or_zero_to_nine_star_src, any_dig_or_zero_to_nine_star_att)
  ; (any_space_or_space_star_src, any_space_or_space_star_att)
  ; (any_non_space_or_a_star_src, any_non_space_or_a_star_att)
  ; (any_word_char_or_underscore_star_src, any_word_char_or_underscore_star_att)
  ; (any_word_char_a_or_a_star_src, any_word_char_a_or_a_star_att)
  ; (any_non_word_char_or_underscore_star_src
    ,any_non_word_char_or_underscore_star_att)
  ; (any_word_char_star_src, any_word_char_star_att)
  ; (hex_a_or_a_star_src, hex_a_or_a_star_att)
  ; (hex_upj_or_hex_upj_star_src, hex_upj_or_hex_upj_star_att)
  ; (circ_a_or_a_star_dollar_src, circ_a_or_a_star_dollar_att)
  ; (circ_a_or_a_star_src, circ_a_or_a_star_att)
  ; (a_or_a_star_dollar_src, a_or_a_star_dollar_att)
  ; (a_or_a_one_times_star_src, a_or_a_one_times_star_att)
  ; (a_or_a_two_times_star_src, a_or_a_two_times_star_att)
  ; (a_one_or_two_times_star_src, a_one_or_two_times_star_att)
  ; (a_or_error_star_src, a_or_error_star_att)
  ; (a_lbrack_1_a_rbrack_or_same_star_src, a_lbrack_1_a_rbrack_or_same_star_att)
  ; (a_1_or_more_times_star_src, a_1_or_more_times_star_att)
  ; (empty_flags_src, empty_flags_att)
  ; (a_or_a_star_global_src, a_or_a_star_global_att)
  ; (a_or_A_star_case_insensitive_src, a_or_A_star_case_insensitive_att)
  ; (a_or_a_star_case_insensitive_src, a_or_a_star_case_insensitive_att)
  ; (a_or_a_star_b_src, a_or_a_star_b_att)
  ; (a_or_b_or_ab_star_c_src, a_or_b_or_ab_star_c_att)
  ; (circ_a_or_a_star_word_boundary_src, circ_a_or_a_star_word_boundary_att)
  ; (circ_a_or_a_star_end_of_string_src, circ_a_or_a_star_end_of_string_att)
  ] [@@ocamlformat "disable"]

let match_test_pairs =
  [ (empty_src, empty_att)
  ; (hello_or_ciao_src, hello_or_ciao_att)
  ; (b_a_star_star_src, b_a_star_star_att)
  ; (a_star_src, a_star_att)
  ; (a_or_a_star_src, a_or_a_star_att_match)
  ; (a_or_aa_star_src, a_or_aa_star_att_match)
  ; (a_or_b_star_src, a_or_b_star_att)
  ; (a_or_b_or_ab_star_src, a_or_b_or_ab_star_att_match)
  ; (a_or_a_star_b_src, a_or_a_star_b_att_match)
  ; (a_or_b_or_ab_star_c_src, a_or_b_or_ab_star_c_att_match)
  ; (a_or_a_star_dollar_src, a_or_a_star_dollar_att_match)
  ; (circ_a_or_a_star_dollar_src, a_or_a_star_dollar_att_match)
  ; (circ_a_or_a_star_src, circ_a_or_a_star_att_match)
  ; (circ_a_or_a_star_word_boundary_src, circ_a_or_a_star_word_boundary_att_match)
  ; (circ_a_or_a_star_end_of_string_src, circ_a_or_a_star_end_of_string_att_match)
  ] [@@ocamlformat "disable"]

let testable_extre = Alcotest.testable ExtRe.pp ExtRe.lang_eq

let make_test ~semantics couple =
  let src, expected_attack_language = couple in
  match ParseRe.parse src with
  | Error e ->
      ParseRe.report_parse_error Format.err_formatter e;
      Alcotest.fail "Parsing of regex was not supposed to fail"
  | Ok regex -> (
      match ParserRe.to_re ~semantics regex with
      | Error e ->
          ParserRe.report_conversion_error Format.err_formatter e;
          Alcotest.fail "Conversion of regex was not supposed to fail"
      | Ok regex ->
          let attack_families = analyze regex in
          let attack_lang = AttackFamilySet.to_lang attack_families in
          Alcotest.test_case src `Quick (fun _ ->
              Alcotest.check testable_extre src expected_attack_language
                attack_lang))

let () =
  let open Alcotest in
  run "Analysis tests"
    [
      ( "Fullmatch",
        List.map (make_test ~semantics:Fullmatch) fullmatch_test_pairs );
      ("Match", List.map (make_test ~semantics:Match) match_test_pairs);
    ]
