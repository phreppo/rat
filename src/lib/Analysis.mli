(** Analysis of ReDoS attacks. *)

(** Describes a language of dangerous words for a regular expression. The family
    of attack words is described by a prefix, a pump and a suffix. If [f] is a
    family with prefix [pre], pump [pump] and suffix [suf], then the
    corresponding attack language is [pre ⋅ pump* ⋅ suf]. *)
module AttackFamily : sig
  type t = { prefix : ExtRe.t; pump : ExtRe.t; suffix : ExtRe.t }
end

(** Is a string that can exploit a ReDoS vulnerability for a regular expression.
*)
module ExploitString : sig
  type t

  val from_attack_family : AttackFamily.t -> (char -> bool) -> t option
  (** [from_attack_family f] an exploit string recognized by [f]. *)

  val generate : int -> t -> string
  (** [generate n e] is the exploit string represented by [e] with length at
      most [n]. *)

  val to_string : t -> string
  (** [to_string e] is the string representation of [e]. *)
end

(** Set of attack families, the result of the analysis. *)
module AttackFamilySet : sig
  include Set.S with type elt = AttackFamily.t

  val to_string : t -> string
  (** [to_string fs] is the string representation of [fs]. *)

  val to_lang : t -> ExtRe.t
  (** [to_lang fs] is the regular expression recognizing the language described
      by the set of attack families. *)

  val get_exploit_string : t -> (char -> bool) -> ExploitString.t option
  (** [get_exploit_string fs pred] is an exploit string from the attack family
      such that each character in the exploit string respect the predicate
      [pred]. If such string does not exist return [None]. *)

  val simplify : t -> t
  (** [simplify fs] is a set of attack families that recognizes the same attack
      language as [fs], but should be more readable. It is very slow, because
      the corresponding minimal NFA is build first. Should be used only to print
      the result. *)
end

val analyze : Re.t -> AttackFamilySet.t
(** [analyze r] analyzes the regular expression [r] and returns the set of
    families that are dangerous for [r]. *)

type redos_result = Safe | Dangerous | ParseError

val has_redos : string -> redos_result
(** [has_redos s] analyzes the regular expression [s] and returns if it is safe,
    dangerous or if it is a regular expression that we cannot analyse. *)
