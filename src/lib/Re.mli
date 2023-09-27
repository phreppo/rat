(** Regular expressions.

    Regular expressions implement the traditional regexes. They support only
    regular constructs, and they are the input of the analysis. As described in
    the paper, they support a transition relation that describes the next state
    in the matching engine. See the paper for further information. *)

(** Type of the regular expressions. *)
type t =
  | Epsilon
  | Char of Charset.t
  | Concat of t * t
  | Alternative of t * t
  | Star of bool * t (* bool is true if the star can be expanded. *)

val compare : t -> t -> int
val to_string : t -> string

(** The transition relation type. *)
type transition =
  | None
  | Match of Charset.t * t
  | LeftOrRight of t * t
  | ExpandOrNot of t * t

(** {1 Smart Constructors} *)

val eps : t
(** [eps] is the regular expression accepting the empty word. *)

val ch : Charset.t -> t
(** [ch chrs] is the regular expression accepting the set of characters [chrs].*)

val choice : t -> t -> t
(** [choice re1 re2] is [re1 | re2]. *)

val concat : t -> t -> t
(** [concat re1 re2] is [re1 re2]. *)

val star : ?expandible:bool -> t -> t
(** [star expandible re] is [re*]. If [expandible] is [false], the star cannot
    be expanded again (see the paper for details). *)

(** {1 Utility} *)

val head : t -> t
(** [head re] is the first constructor in the top-level concatentaion of [re]. *)

val tail : t -> t
(** [tail re] is [re] without [head re]. *)

val case_insensitive : t -> t
(** [case_insensitive re] is the case-insensitive version of [re]. *)

(** {1 Transition Relation Operations} *)

val refesh_stars : t -> t
(** [refresh_stars re] is [re] with all stars marked as expandable. *)

val next : t -> transition
(** [next re] is the transition that [re] can perform. *)

(** Set of regular expressions. *)
module ReSet : sig
  include Set.S with type elt = t

  val to_string : t -> string
end
