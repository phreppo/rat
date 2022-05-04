open Utils

module Make (S : SIG.STATE) (A : SIG.COMPARABLE) : sig
  module StatesSet : Set.S with type elt = S.t
  module Algebra : Set.S with type elt = A.t

  type dfa

  type regexp =
    | L of A.t  (** Letter *)
    | S of regexp  (** Star *)
    | C of (regexp * regexp)  (** Concatenation *)
    | A of (regexp * regexp)  (** Disjonction *)
    | V of string  (** Variables, should not be used outside of module *)
    | E  (** Epsilon *)
    | N  (** Empty *)

  val normalize : regexp -> regexp
  val automata_of_regexp : Algebra.t -> regexp -> dfa
  val regexp_of_automata : dfa -> regexp
  val print : Format.formatter -> dfa -> unit
  val print_regexp : Format.formatter -> regexp -> unit
  val pp_print_regexp : Format.formatter -> regexp -> unit
  val delta : S.t -> A.t -> dfa -> S.t
  val get_states : dfa -> StatesSet.t
  val get_algebra : dfa -> Algebra.t
  val change_algebra : dfa -> Algebra.t -> dfa
  val top : Algebra.t -> dfa
  val bottom : Algebra.t -> dfa
  val from_word : Algebra.t -> A.t list -> dfa
  val concat : dfa -> dfa -> dfa
  val star : dfa -> dfa
  val join : dfa -> dfa -> dfa
  val widening : int -> dfa -> dfa -> dfa
  val meet : dfa -> dfa -> dfa

  val complementary : dfa -> dfa
  (** [complementary d] is a dfa that recognizes the complement language of [d].
      Makes [d] co-acceptant. *)

  val complementary' : dfa -> dfa
  (** [compalementary' d] is a dfa that recognizes the complement language od
      [d]. Does not make [d] co-acceptant, and it is usually slower than
      [complementary d]. *)

  val diff : dfa -> dfa -> dfa
  val is_bottom : dfa -> bool
  val leq : dfa -> dfa -> bool
  val print_dot : Format.formatter -> dfa -> unit
  val div : dfa -> dfa -> dfa
  val minimization : dfa -> dfa

  val get_word : dfa -> (A.t -> bool) -> A.t list option
  (** [get_word d p] is a word recognized by [d] in which each symbol in the
      algebra respects [p]. A precondition is that [d] is not co-acceptant. If
      there exists no such word, returns [None]. *)
end
