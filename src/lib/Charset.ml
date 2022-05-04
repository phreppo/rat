include Set.Make (struct
  type t = char

  let compare = Char.compare
end)

let all_symbols = of_list Utility.all_chars
let digits = of_list [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
let uppercase_alpha = of_list (Utility.chars_range 65 90)
let lowercase_alpha = of_list (Utility.chars_range 97 122)
let alpha = union lowercase_alpha uppercase_alpha
let underscore = singleton '_'

let word_chars =
  union lowercase_alpha uppercase_alpha |> union underscore |> union digits

(* 12 and 16 correspond to \f and \v (Formfeed Page Break and Vertical Tab) *)
let spaces = of_list [ ' '; '\t'; '\n'; '\r'; Char.chr 12; Char.chr 16 ]
let complement = diff all_symbols
let non_digits = complement digits
let non_spaces = complement spaces
let non_word_chars = complement word_chars

let range lb ub =
  let lb_int, ub_int = (Char.code lb, Char.code ub) in
  let rec int_range current ub =
    if current = ub then [ current ] else current :: int_range (current + 1) ub
  in
  if lb_int > ub_int then
    failwith
      "lower bound must be lower or equal to the upper bound in character range"
  else int_range lb_int ub_int |> List.map Char.chr |> of_list

let add_upper_lower_case c s =
  s |> add (Char.uppercase_ascii c) |> add (Char.lowercase_ascii c)

let case_insensitive s =
  let alpha = filter Utility.chr_is_letter s in
  let alpha' = fold add_upper_lower_case alpha empty in
  union s alpha'

let to_string charset =
  if cardinal charset = List.length Utility.all_chars then "."
  else if cardinal charset = 1 then choose charset |> Char.escaped
  else if equal charset digits then "\\d"
  else if equal charset lowercase_alpha then "[a-z]"
  else if equal charset uppercase_alpha then "[A-Z]"
  else if equal charset alpha then "[a-zA-Z]"
  else if equal charset (union alpha digits) then "[a-zA-Z0-9]"
  else if equal charset word_chars then "\\w"
  else if equal charset spaces then "\\s"
  else if equal charset non_digits then "\\D"
  else if equal charset non_spaces then "\\S"
  else if equal charset non_word_chars then "\\W"
  else
    let els = elements charset in
    let elements_strings = List.map Char.escaped els in
    let elements_strings_joined = String.concat "" elements_strings in
    String.concat "" [ "["; elements_strings_joined; "]" ]

let pprint charset = to_string charset |> print_string
