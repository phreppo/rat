let debug fmt =
  Format.kasprintf
    (fun str -> Format.printf "\027[1;3m[Debug]\027[0m @.@[%s@]@.@." str)
    fmt

module Make (S : SIG.COMPARABLE) = struct
  module SM = Map.Make (S)
  module SS = Set.Make (S)

  type t = S.t SM.t

  (* let print fmt (u : t) = *)
  (*   Format.fprintf fmt "@[{%a}@]"  *)
  let ( &= ) a b = S.compare a b = 0

  let print_state_set fmt (ss : SS.t) : unit =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         S.print)
      (SS.elements ss)

  let print_map fmt (ss : SS.t SM.t) : unit =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt (x, y) ->
           Format.fprintf fmt "%a->%a" S.print x print_state_set y))
      (SM.bindings ss)

  let print_bare fmt u =
    Format.fprintf fmt "{@[<v>%a@]}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (x, y) -> Format.fprintf fmt "%a -> %a" S.print x S.print y))
      (SM.bindings u)

  let replace (a : S.t) (v : S.t) (u : t) =
    if SM.mem a u then u |> SM.remove a |> SM.add a v else u |> SM.add a v

  let all (u : t) = SM.fold (fun k _ acc -> k :: acc) u []

  let find (a : S.t) (u : t) =
    (* let () = debug "%a %a" S.print a print_bare u in *)
    let rec aux a l =
      let x = SM.find a u in
      if x &= a then (a, l) else aux x (a :: l)
    in
    let a, l = aux a [] in
    let u = List.fold_left (fun acc x -> replace x a acc) u l in
    (* let () = debug "found : %a and produced %a" S.print a print_bare u in *)
    (a, u)

  let union (a : S.t) (b : S.t) (u : t) : t =
    let a, u = find a u in
    let b, u = find b u in
    let rep = replace a b u in
    rep

  let is_equiv a b u =
    let a, u = find a u in
    let b, u = find b u in
    (a &= b, u)

  let new_partition (l : S.t list) : t =
    List.fold_left (fun acc x -> SM.add x x acc) SM.empty l

  let to_map u =
    let sm =
      SM.fold
        (fun k _ (acc, nu) ->
          let a, nu = find k nu in
          if SM.mem a acc then
            let y = SM.find a acc in
            (acc |> SM.remove a |> SM.add a (SS.add k y), nu)
          else (SM.add a (SS.singleton k) acc, nu))
        u (SM.empty, u)
    in
    (* let () = debug "@[<v>to_map : %a@,%a@]" print_bare u print_map (fst sm)
       in *)
    sm

  let to_list u =
    let sm, nu = to_map u in
    (SM.fold (fun k s acc -> SS.elements s :: acc) sm [], nu)

  let nb_class (u : t) = u |> to_list |> fst |> List.length

  let print (fmt : Format.formatter) (u : t) : unit =
    let sm, _ = to_map u in
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt (_, k) ->
           Format.fprintf fmt "{%a}"
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
                S.print)
             (SS.elements k)))
      (SM.bindings sm)

  let from_list l =
    let all =
      List.fold_left
        (fun acc x -> List.fold_left (fun acc x -> x :: acc) acc x)
        [] l
    in
    let p = new_partition all in
    (* let () = debug "p : %a" print p in *)
    let do_one acc x =
      (* let () = debug "do_one called on %a %a" print acc print_bare acc in *)
      match x with
      | t :: r -> List.fold_left (fun acc y -> union t y acc) acc r
      | [] -> acc
    in
    List.fold_left do_one p l
end
