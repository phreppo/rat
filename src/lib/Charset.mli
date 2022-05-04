(** Set of characters.

    Adds to the standard [Set] some utility functions to manipulate specifically
    sets of characters. *)

include Set.S with type elt = char

(** {1 Operations} *)

val to_string : t -> string
(** [to_string s] is the string representing [s]. *)

val pprint : t -> unit
(** [pprint s] pretty printys [s]. *)

val complement : t -> t
(** [complement s] is all characters not in [s]. *)

val range : char -> char -> t
(** [range lb ub] is [{lb, lb + 1, ..., ub}]. If [lb > ub] fails. *)

val case_insensitive : t -> t
(** [case_insensitive s] is [s ∪ s'], where [s'] is the set of alphabetic
    characters in [s] with the uppercase and lowercase value. *)

(** {1 Values} *)

val all_symbols : t
(** [all_symbols] is all characters. *)

val digits : t
(** [digits] is [{0, ..., 9}]. *)

val uppercase_alpha : t
(** [uppercase_alpha] is [{A, ..., Z}]. *)

val lowercase_alpha : t
(** [lowercase_alpha] is [{a, ..., z}]. *)

val underscore : t
(** [underscore] is [{_}]. *)

val word_chars : t
(** [word_chars] is [uppercase_alpha ∪ lowercase_alpha ∪ underscore ∪ digits]. *)

val spaces : t
(** [spaces] is all space characters. *)

val non_digits : t
(** [non_digits] is [all_symbols \ digits]. *)

val non_spaces : t
(** [non_spaces] is [all_symbols \ spaces]. *)

val non_word_chars : t
(** [non_word_chars] is [all_symbols \ word_chars]. *)
