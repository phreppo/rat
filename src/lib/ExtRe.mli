(** Extended regular expressions.

    Extended regular expressions are regular expressions that support as
    syntactic constructs the intersection and the complement. This makes it
    possible to create the intersection and the complement in [O(1)]. Since in
    the analysis those operations are used frequently, it is convenient to
    represent them syntactically.

    Extended regular expressions support operations on the language that they
    recognize, most notably the language inclusion. *)

type t

val compare : t -> t -> int
(** [compare re1 re2] is the syntactical comparison of [re1] and [re2]. *)

val eq : t -> t -> bool
(** [eq re1 re2] is [true] iff [re1 = re2]. *)

val to_string : t -> string
(** [to_string re] is the string representation of [re]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt re] pretty-prints [re] with [fmt]. *)

(** {1 Smart Constructors} *)

val empty : t
(** [empty] is the regular expression accepting the empty set of words. *)

val eps : t
(** [eps] is the regular expression accepting the empty word. *)

val chr : Charset.t -> t
(** [chr chrs] is the regular expression accepting the set of characters [chrs].*)

val alternative : t -> t -> t
(** [alternative re1 re2] is [re1 | re2]. *)

val concat : t -> t -> t
(** [concat re1 re2] is [re1 re2]. *)

val star : t -> t
(** [star re] is [re*]. *)

val plus : t -> t
(** [plus re] is [re+]. *)

val inter : t -> t -> t
(** [inter re1 re2] is the syntactical intersection of [re1] and [re2]. *)

val compl : t -> t
(** [compl re] is the syntactical complement of [re]. *)

(** {1 Common Regular Expressions} *)

val any_char : t
(** [any_char] is the regular expression accepting any character. *)

val universal : t
(** [universal] is the regular expression accepting any word. *)

(** {1 Language Operations} *)

val lang_leq : t -> t -> bool
(** [lang_leq re1 re2] is [true] iff the language recognized by [re1] is a
    subset of the language recognized by [re2]. *)

val lang_eq : t -> t -> bool
(** [lang_leq re1 re2] is [true] iff the language recognized by [re1] equals the
    language recognized by [re2]. *)

val lang_universal : t -> bool
(** [lang_universal re] is [true] iff [re] recognizes the universal language. *)

val lang_empty : t -> bool
(** [lang_empty re] is [true] iff [re] recozgnies the empty language. *)

val simplify : t -> t
(** [simplify re] is a regular expression [re'] that recognizes the same
    language as [re], but is more readable. It is currently implemented by
    building the corresponding minimal NFA, which is then translated back to a
    regular expression. It is for the moment particularly slow and should be
    optimized. *)

val simplify_if_empty_lang : t -> t
(** [simplify_if_empty re] is [re] if the language of [re] is not empty,
    otherwise it is [∅]. *)

val get_word : t -> (char -> bool) -> string option
(** [get_word predicate re] returns a word in the language of [re] such that for
    each character [c] in [get_word predicate re], [predicate c = true]. If such
    word does not exist, returns [None]. *)
