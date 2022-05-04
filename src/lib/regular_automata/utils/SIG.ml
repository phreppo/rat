module type STATE = sig
  type t

  val fresh : unit -> t
  val print : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val restart : unit -> unit
end

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end

module type COMPARABLE_WITNESS = sig
  type t

  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
  val witness : t
end
