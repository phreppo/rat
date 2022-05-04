open Utils

module Make : functor (Comp : Utils.SIG.COMPARABLE) -> sig
  type t

  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
  val mem_symbol : Comp.t -> t -> bool
  val fold : (Comp.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (Comp.t -> unit) -> t -> unit
  val union : t -> t -> t
  val add : Comp.t -> t -> t
  val empty : t
  val diff : t -> t -> t
  val is_empty : t -> bool
  val subset : t -> t -> bool
  val of_list : Comp.t list -> t
  val to_list : t -> Comp.t list
end
