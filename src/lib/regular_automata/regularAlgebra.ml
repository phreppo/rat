open Utils

module Make (A : SIG.COMPARABLE) = struct
  type u = A.t

  module SA = Set.Make (struct
    type t = u

    let compare = A.compare
  end)

  type t = SA.t

  let compare = SA.compare
  let print = ToolBox.print_set_inline A.print SA.elements
  let mem_symbol s sa = SA.mem s sa
  let fold f sa acc = SA.fold f sa acc
  let union = SA.union
  let add = SA.add
  let empty = SA.empty
  let iter = SA.iter
  let of_list = SA.of_list
  let to_list = SA.elements
  let diff = SA.diff
  let is_empty = SA.is_empty
  let subset = SA.subset
end
