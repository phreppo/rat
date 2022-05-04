module Make (S : SIG.COMPARABLE) (T : SIG.COMPARABLE) = struct
  module M = Map.Make (S)

  type t = T.t M.t

  let mem = M.mem
  let empty = M.empty

  let add (a : S.t) (f : unit -> T.t) (u : t) =
    if M.mem a u then u else M.add a (f ()) u

  let get (a : S.t) (f : unit -> T.t) (u : t) : t * T.t =
    if M.mem a u then (u, M.find a u)
    else
      let x = f () in
      (M.add a x u, x)

  let apply (a : S.t) (u : t) = M.find a u

  let print fmt (u : t) =
    Format.fprintf fmt "@[<v>{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (x, y) -> Format.fprintf fmt "%a ~> %a" S.print x T.print y))
      (M.bindings u)
end
