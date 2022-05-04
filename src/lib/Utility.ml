let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

let red = "\027[31m"
let green = "\027[32m"
let yellow = "\027[33m"
let blue = "\027[34m"
let reset = "\027[0m"
let red_str s = red ^ s ^ reset
let green_str s = green ^ s ^ reset
let yellow_str s = yellow ^ s ^ reset
let blue_str s = blue ^ s ^ reset

let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let to_chars = List.map Char.chr
let explode s = List.init (String.length s) (String.get s)
let chars_range lb ub = range lb ub |> to_chars
let all_chars = chars_range 0 255
let eps_error_msg = "got (eps, re') from head re, tail re with re' != eps"

let concat_error_msg =
  "got (Concat (re', re''), _) from (head re, tail re): head re should never \
   result in a concatenation"

let chr_is_printable c = (Char.code c <= 126 && Char.code c >= 32) || c = '\t'

let chr_is_letter c =
  (Char.code c <= Char.code 'Z' && Char.code c >= Char.code 'A')
  || (Char.code c <= Char.code 'z' && Char.code c >= Char.code 'a')

module OptionSyntax = struct
  let ( let* ) = Option.bind
end

module ResultSyntax = struct
  let ( let* ) = Result.bind
end
