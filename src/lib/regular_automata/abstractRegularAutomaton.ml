open Utils

let debug fmt = ToolBox.debug "regular automaton" fmt
let pp_sep_comma fmt () = Format.fprintf fmt ","

module Make (S : SIG.STATE) (A : SIG.COMPARABLE) = struct
  module Rename = Map.Make (S)
  (** automata renaming *)

  type rename = S.t Rename.t

  let print_rename = ToolBox.print_map S.print S.print Rename.bindings

  type state = S.t

  let state_generator () =
    let () = S.restart () in
    fun () -> S.fresh ()

  let var_of_state x =
    Printf.sprintf "v_%s" (ToolBox.format_to_string S.print x)

  let print_state = S.print

  (** Transition *)
  module DetTrans = Map.Make (struct
    type t = A.t * state

    let compare = ToolBox.pair_compare A.compare S.compare
  end)

  type trans = S.t DetTrans.t

  let print_dettrans =
    ToolBox.print_map
      (ToolBox.print_pair A.print print_state)
      print_state DetTrans.bindings

  (** Set of states *)
  module StatesSet = Set.Make (struct
    type t = state

    let compare = S.compare
  end)

  type states = StatesSet.t

  let print_states = ToolBox.print_set_inline S.print StatesSet.elements

  module Algebra = Set.Make (A)
  (** algebra *)

  let print_algebra = ToolBox.print_set_inline A.print Algebra.elements

  let print_state_list fmt l =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:pp_sep_comma print_state)
      l

  let print_transition_list fmt l =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:pp_sep_comma (fun fmt (s, c, s') ->
           Format.fprintf fmt "(%a,%a,%a)" print_state s A.print c print_state
             s'))
      l

  type dfa = {
    states : states;
    algebra : Algebra.t;
    start : state;
    final : states;
    trans : trans;
    hole : state;
    hole_coreach : bool;
  }

  let print fmt (x : dfa) =
    Format.fprintf fmt
      "@[<v 2>{@,\
       @[<v>states: %a@,\
       algebra: %a@,\
       start: %a@,\
       final: %a@,\
       trans: %a@,\
       hole: %a;@,\
       hole_coreach: %a@]@,\
       }@]"
      print_states x.states print_algebra x.algebra print_state x.start
      print_states x.final print_dettrans x.trans print_state x.hole
      Format.pp_print_bool x.hole_coreach

  let fold_trans_nh f u acc =
    DetTrans.fold (fun (letter, q) q' acc -> f (q, letter, q') acc) u.trans acc

  let delta (q : state) (a : A.t) (u : dfa) : state =
    try DetTrans.find (a, q) u.trans with Not_found -> u.hole

  let get_states x = x.states

  let rec create_fresh_state (s : StatesSet.t) (f : unit -> S.t) =
    let n = f () in
    if StatesSet.mem n s then create_fresh_state s f else n

  let get_algebra x = x.algebra

  module RSS = Rewrite.Make (S) (S)

  let make_non_coreachable_hole (u : dfa) =
    if u.hole_coreach then
      let all_states = get_states u in
      let a = get_algebra u in
      let () = S.restart () in
      let new_hole = S.fresh () in
      let r = RSS.empty in
      let n_states_minus_hole, r =
        StatesSet.fold
          (fun q (n_states', r) ->
            let r, q' = RSS.get q S.fresh r in
            (StatesSet.add q' n_states', r))
          u.states (StatesSet.empty, r)
      in
      let n_states = StatesSet.add new_hole n_states_minus_hole in
      let n_start = RSS.apply u.start r in
      let n_final = StatesSet.map (fun x -> RSS.apply x r) u.final in
      let n_trans =
        Algebra.fold
          (fun letter trans ->
            StatesSet.fold
              (fun q trans ->
                let q' = delta q letter u in
                let nq = RSS.apply q r in
                let nq' = RSS.apply q' r in
                DetTrans.add (letter, nq) nq' trans)
              all_states trans)
          a DetTrans.empty
      in
      {
        states = n_states;
        algebra = a;
        start = n_start;
        final = n_final;
        trans = n_trans;
        hole = new_hole;
        hole_coreach = false;
      }
    else u

  type nd_trans = StatesSet.t DetTrans.t
  (** Non deterministic automaton *)

  let print_nd_trans =
    ToolBox.print_map
      (ToolBox.print_pair A.print print_state)
      print_states DetTrans.bindings

  module PartS = struct
    type t = states

    let print = print_states
    let compare = StatesSet.compare
  end

  module RPart = Rewrite.Make (PartS) (S)

  type nfa = {
    nd_states : StatesSet.t;
    nd_algebra : Algebra.t;
    nd_init : StatesSet.t;
    nd_trans : nd_trans;
    nd_finals : StatesSet.t;
  }

  let print_nfa fmt (x : nfa) =
    Format.fprintf fmt
      "@[<v 2>{@,\
       @[<v>nd_states: %a@,\
       nd_algebra: %a@,\
       nd_init: %a@,\
       nd_finals: %a@,\
       nd_trans: %a@]@,\
       }@]"
      print_states x.nd_states print_algebra x.nd_algebra print_states x.nd_init
      print_states x.nd_finals print_nd_trans x.nd_trans

  let delta_nfa q a (u : nfa) =
    try DetTrans.find (a, q) u.nd_trans with Not_found -> StatesSet.empty

  let nd_add_trans (s, c, s') tr =
    if DetTrans.mem (c, s) tr then
      let t = DetTrans.find (c, s) tr in
      tr |> DetTrans.add (c, s) (StatesSet.add s' t)
    else tr |> DetTrans.add (c, s) (StatesSet.singleton s')

  let nd_fold_trans f x acc =
    DetTrans.fold
      (fun (a, q) v acc ->
        StatesSet.fold (fun q' acc -> f (q, a, q') acc) v acc)
      x acc

  let nd_apply_rename r x =
    let rename x = try Rename.find x r with Not_found -> x in
    {
      x with
      nd_states = StatesSet.map rename x.nd_states;
      nd_trans =
        DetTrans.fold
          (fun (a, s) ss acc ->
            DetTrans.add (a, rename s) (StatesSet.map rename ss) acc)
          x.nd_trans DetTrans.empty;
      nd_init = StatesSet.map rename x.nd_init;
      nd_finals = StatesSet.map rename x.nd_finals;
    }

  let determinize (u : nfa) : dfa =
    let ng = state_generator () in
    let a = u.nd_algebra in
    let rec go (todo : (states * state) list) (seen : StatesSet.t)
        (finals : StatesSet.t) trans r =
      match todo with
      | (ps, p) :: q ->
          let todo, seen, trans, finals, r =
            Algebra.fold
              (fun letter (todo, seen, trans, finals, r) ->
                let dirs =
                  StatesSet.fold
                    (fun st acc -> StatesSet.union (delta_nfa st letter u) acc)
                    ps StatesSet.empty
                in
                let r, dir = RPart.get dirs ng r in
                let ntrans =
                  if not (StatesSet.is_empty dirs) then
                    DetTrans.add (letter, p) dir trans
                  else trans
                in
                if StatesSet.mem dir seen then (todo, seen, ntrans, finals, r)
                else if
                  not (StatesSet.is_empty (StatesSet.inter dirs u.nd_finals))
                then
                  ( (dirs, dir) :: todo,
                    StatesSet.add dir seen,
                    ntrans,
                    StatesSet.add dir finals,
                    r )
                else
                  ( (dirs, dir) :: todo,
                    StatesSet.add dir seen,
                    ntrans,
                    finals,
                    r ))
              a
              (q, seen, trans, finals, r)
          in
          go todo seen finals trans r
      | [] -> (seen, trans, finals, r)
    in
    let inits = u.nd_init in
    let r, init = RPart.get inits ng RPart.empty in
    let r, hole = RPart.get StatesSet.empty ng r in
    let finals =
      if not (StatesSet.is_empty (StatesSet.inter inits u.nd_finals)) then
        StatesSet.singleton init
      else StatesSet.empty
    in
    let seen, trans, finals, r =
      go
        [ (inits, init) ]
        (StatesSet.of_list [ init; hole ])
        finals DetTrans.empty r
    in
    {
      states = seen;
      algebra = u.nd_algebra;
      start = init;
      final = finals;
      trans;
      hole;
      hole_coreach = false;
    }

  let non_determinize u =
    let u = make_non_coreachable_hole u in
    {
      nd_states = u.states;
      nd_algebra = u.algebra;
      nd_init = StatesSet.singleton u.start;
      nd_finals = u.final;
      nd_trans = DetTrans.map StatesSet.singleton u.trans;
    }

  let apply_rename (r : rename) (u : dfa) =
    let map x = try Rename.find x r with Not_found -> x in
    let nhole = map u.hole in
    let is_hole x = S.compare nhole x = 0 in
    {
      u with
      states = StatesSet.map map u.states;
      start = map u.start;
      final = StatesSet.map map u.final;
      trans =
        fold_trans_nh
          (fun (q, a, q') acc ->
            let qq' = map q' in
            if is_hole qq' then acc
            else
              let qq = map q in
              DetTrans.add (a, qq) qq' acc)
          u DetTrans.empty;
    }

  let apply_rename_det_to_non_det (r : rename) (u : dfa) =
    let b = u.hole_coreach in
    let map x = try Rename.find x r with Not_found -> x in
    (* over approximation: *)
    let will_hole_be_coreachable =
      b
      ||
      let hd = map u.hole in
      Rename.exists (fun k a -> S.compare k u.hole <> 0 && S.compare a hd = 0) r
    in
    let nhole = map u.hole in
    let is_hole x = S.compare nhole x = 0 in
    {
      nd_states = StatesSet.map map u.states;
      nd_init = StatesSet.singleton (map u.start);
      nd_finals = StatesSet.map map u.final;
      nd_algebra = u.algebra;
      nd_trans =
        (if not will_hole_be_coreachable then
         fold_trans_nh
           (fun (q, a, q') acc ->
             let qq' = map q' in
             if is_hole qq' then acc
             else
               let qq = map q in
               nd_add_trans (qq, a, qq') acc)
           u DetTrans.empty
        else
          StatesSet.fold
            (fun q acc ->
              Algebra.fold
                (fun a acc ->
                  let q' = delta q a u in
                  let qq' = map q' in
                  let qq = map q in
                  nd_add_trans (qq, a, qq') acc)
                u.algebra acc)
            (get_states u) DetTrans.empty);
    }

  let mini (u : dfa) max_split =
    let max_split = max_split + 1 in
    (* let () = debug "%d" max_split in *)
    let alg = get_algebra u in
    let module UF = UnionFind.Make (S) in
    let module TransInv = Map.Make (S) in
    let add_trans_inv q q' tinv =
      try
        let s = TransInv.find q' tinv in
        let news = StatesSet.add q s in
        TransInv.add q' news tinv
      with Not_found -> TransInv.add q' (StatesSet.singleton q) tinv
    in
    let split_one_class_on_letter (letter : A.t) (cl : states) (uf : UF.t)
        max_split =
      let tinv, uf =
        StatesSet.fold
          (fun q (tinv, uf) ->
            let q' = delta q letter u in
            let rq', uf = UF.find q' uf in
            let tinv = add_trans_inv q q' tinv in
            (tinv, uf))
          cl (TransInv.empty, uf)
      in
      let nb, ncl_list, reste =
        TransInv.fold
          (fun _ ncl (nb, ncl_list, reste) ->
            if nb >= max_split - 1 then (nb, ncl_list, StatesSet.union ncl reste)
            else (nb + 1, ncl :: ncl_list, reste))
          tinv (0, [], StatesSet.empty)
      in
      if StatesSet.is_empty reste then (ncl_list, uf, nb)
      else (reste :: ncl_list, uf, nb + 1)
    in
    let exception Found of states list * UF.t * int in
    let split_one_classe (cl : states) (uf : UF.t) max_split =
      try
        let uf =
          Algebra.fold
            (fun letter uf ->
              let sp, uf, nb =
                split_one_class_on_letter letter cl uf max_split
              in
              if nb > 1 then raise (Found (sp, uf, nb)) else uf)
            alg uf
        in
        ([ cl ], uf, 1)
      with Found (sp, uf, nb) -> (sp, uf, nb)
    in
    let aux (l : states list) (nb_class : int) =
      let uf = UF.from_list (List.map StatesSet.elements l) in
      List.fold_left
        (fun (ncl_all, nb_class, uf, one_split) cl ->
          let cl', uf, nb = split_one_classe cl uf (max_split - nb_class + 1) in
          (cl' @ ncl_all, nb_class - 1 + nb, uf, one_split || nb > 1))
        ([], nb_class, uf, false) l
    in
    let rec fix l nb =
      let l, nb, _, b = aux l nb in
      if b && nb < max_split then fix l nb else (l, not b, nb)
    in
    let build_rewrite l =
      List.fold_left
        (fun acc cl ->
          match cl with
          | p :: q -> List.fold_left (fun acc q -> Rename.add q p acc) acc q
          | [] -> acc)
        Rename.empty l
    in
    let term, nterm =
      StatesSet.fold
        (fun q (term, nterm) ->
          if StatesSet.mem q u.final then (StatesSet.add q term, nterm)
          else (term, StatesSet.add q nterm))
        (StatesSet.remove u.hole u.states)
        (StatesSet.empty, StatesSet.empty)
    in
    let part, stable, _ = fix [ StatesSet.singleton u.hole; term; nterm ] 3 in
    let r = build_rewrite (List.map StatesSet.elements part) in
    if stable then apply_rename r u
    else
      let nd_u = apply_rename_det_to_non_det r u in
      nd_u |> determinize

  let minimization (u : dfa) =
    let n = StatesSet.cardinal (get_states u) in
    mini u n

  let change_algebra (u : dfa) (a : Algebra.t) =
    let u = make_non_coreachable_hole u in
    let u_algebra = get_algebra u in
    let to_remove = Algebra.diff u_algebra a in
    if Algebra.is_empty to_remove then { u with algebra = a }
    else
      {
        u with
        algebra = a;
        trans =
          DetTrans.filter
            (fun (a, q) q' -> not (Algebra.mem a to_remove))
            u.trans;
      }

  module S_square = struct
    type t = state * state

    let compare = ToolBox.pair_compare S.compare S.compare
    let print = ToolBox.print_pair S.print S.print
  end

  module S_S_square = Set.Make (S_square)

  let print_s_sq = ToolBox.print_set S_square.print S_S_square.elements

  module RP = Rewrite.Make (S_square) (S)

  let unify_algebra u u' =
    let a = get_algebra u in
    let a' = get_algebra u' in
    let na = Algebra.union a a' in
    let u = change_algebra u na in
    let u' = change_algebra u' na in
    (u, u')

  let product (u : dfa) (u' : dfa) (hole_filter : bool -> bool -> bool)
      (final_filter : bool -> bool -> bool) =
    let () = S.restart () in
    let r = RP.empty in
    let u = make_non_coreachable_hole u in
    let u' = make_non_coreachable_hole u' in
    let is_hole u q = S.compare q u.hole = 0 in
    let u, u' = unify_algebra u u' in
    let na = get_algebra u' in
    let rec aux trans r (todo : (S_square.t * S.t) list) (seen : states) =
      match todo with
      | ((q, q'), qq) :: rl ->
          let trans, r, todo, seen =
            Algebra.fold
              (fun a ((trans, r, todo, seen) as acc) ->
                let qdir = delta q a u in
                let qdir' = delta q' a u' in
                if hole_filter (not (is_hole u qdir)) (not (is_hole u' qdir'))
                then
                  let r, dir = RP.get (qdir, qdir') S.fresh r in
                  let todo, seen =
                    if StatesSet.mem dir seen then (todo, seen)
                    else (((qdir, qdir'), dir) :: todo, StatesSet.add dir seen)
                  in
                  let trans = DetTrans.add (a, qq) dir trans in
                  (trans, r, todo, seen)
                else acc)
              na (trans, r, rl, seen)
          in
          aux trans r todo seen
      | [] -> (trans, r, seen)
    in
    let r, n_start = RP.get (u.start, u'.start) S.fresh r in
    let r, n_hole = RP.get (u.hole, u'.hole) S.fresh r in
    let trans, r, seen =
      aux DetTrans.empty r
        [ ((u.start, u'.start), n_start) ]
        (StatesSet.singleton n_start)
    in
    let rep =
      {
        states = StatesSet.add n_hole seen;
        algebra = na;
        start = n_start;
        final =
          RP.M.fold
            (fun (q, q') nq finals ->
              if
                final_filter (StatesSet.mem q u.final)
                  (StatesSet.mem q' u'.final)
              then StatesSet.add nq finals
              else finals)
            r StatesSet.empty;
        trans;
        hole = n_hole;
        hole_coreach = false;
      }
    in
    rep

  let div (u : dfa) (u' : dfa) =
    let () = S.restart () in
    let r = RP.empty in
    let u = make_non_coreachable_hole u in
    let u' = make_non_coreachable_hole u' in
    let is_hole u q = S.compare q u.hole = 0 in
    let u, u' = unify_algebra u u' in
    let na = get_algebra u' in
    let rec aux trans r (todo : (S_square.t * S.t) list) (seen : states) =
      match todo with
      | ((q, q'), qq) :: rl ->
          let trans, r, todo, seen =
            Algebra.fold
              (fun a ((trans, r, todo, seen) as acc) ->
                let qdir = delta q a u in
                let qdir' = delta q' a u' in
                if (not (is_hole u qdir)) || not (is_hole u' qdir') then
                  let r, dir = RP.get (qdir, qdir') S.fresh r in
                  let todo, seen =
                    if StatesSet.mem dir seen then (todo, seen)
                    else (((qdir, qdir'), dir) :: todo, StatesSet.add dir seen)
                  in
                  let trans = DetTrans.add (a, qq) dir trans in
                  (trans, r, todo, seen)
                else acc)
              na (trans, r, rl, seen)
          in
          aux trans r todo seen
      | [] -> (trans, r, seen)
    in
    let r, n_start = RP.get (u.start, u'.start) S.fresh r in
    let r, n_hole = RP.get (u.hole, u'.hole) S.fresh r in
    let trans, r, seen =
      aux DetTrans.empty r
        [ ((u.start, u'.start), n_start) ]
        (StatesSet.singleton n_start)
    in
    let rep =
      {
        nd_states = StatesSet.add n_hole seen;
        nd_algebra = na;
        nd_init =
          RP.M.fold
            (fun (q, q') q'' acc ->
              if StatesSet.mem q' u'.final then StatesSet.add q'' acc else acc)
            r StatesSet.empty;
        nd_trans = DetTrans.map (fun q -> StatesSet.singleton q) trans;
        nd_finals =
          RP.M.fold
            (fun (q, q') q'' acc ->
              if StatesSet.mem q u.final then StatesSet.add q'' acc else acc)
            r StatesSet.empty;
      }
      |> determinize
    in
    (* debug "%a@." print rep ; *)
    rep

  let top (a : Algebra.t) =
    let () = S.restart () in
    let s0 = S.fresh () in
    let s1 = S.fresh () in
    {
      states = StatesSet.of_list [ s0; s1 ];
      algebra = a;
      start = s0;
      final = StatesSet.singleton s0;
      trans =
        Algebra.fold (fun a acc -> DetTrans.add (a, s0) s0 acc) a DetTrans.empty;
      hole = s1;
      hole_coreach = false;
    }

  let bottom (a : Algebra.t) =
    let () = S.restart () in
    let s0 = S.fresh () in
    let s1 = S.fresh () in
    {
      states = StatesSet.of_list [ s0; s1 ];
      algebra = a;
      start = s0;
      final = StatesSet.singleton s1;
      trans = DetTrans.empty;
      hole = s0;
      hole_coreach = false;
    }

  let join u u' = product u u' ( || ) ( || ) |> minimization

  let widening p u u' =
    let pdt = product u u' ( || ) ( || ) in
    (* let () = debug "pdt: %a" print pdt in *)
    mini pdt p

  let meet u u' = product u u' ( && ) ( && ) |> minimization

  let complementary u =
    let st = get_states u in
    let rep =
      {
        u with
        final = StatesSet.diff st u.final;
        hole_coreach = not u.hole_coreach;
      }
    in
    rep

  let diff u v = meet u (complementary v)
  let complementary' u = diff (top u.algebra) u

  let is_bottom u =
    let u = make_non_coreachable_hole u in
    let a = get_algebra u in
    let exception NB in
    let next todo q seen =
      Algebra.fold
        (fun x ((todo_next, seen) as acc) ->
          try
            let q' = DetTrans.find (x, q) u.trans in
            if StatesSet.mem q' u.final then raise NB
            else if StatesSet.mem q' seen then acc
            else (q' :: todo_next, StatesSet.add q' seen)
          with Not_found -> acc)
        a (todo, seen)
    in
    let rec reach todo seen =
      match todo with
      | q :: todo_r ->
          let todo, seen = next todo_r q seen in
          reach todo seen
      | [] -> true
    in
    try
      if StatesSet.mem u.start u.final then raise NB
      else reach [ u.start ] (StatesSet.singleton u.start)
    with NB -> false

  let leq u u' = is_bottom (diff u u')

  let from_word (sa : Algebra.t) (l : A.t list) =
    let ng = state_generator () in
    let a = ng () in
    let t, l, alpha, states =
      List.fold_left
        (fun (acc, last, alpha, states) x ->
          let s' = ng () in
          ( DetTrans.add (x, last) s' acc,
            s',
            Algebra.add x alpha,
            StatesSet.add s' states ))
        (DetTrans.empty, a, Algebra.empty, StatesSet.singleton a)
        l
    in
    let hole = ng () in
    if not (Algebra.subset alpha sa) then
      failwith
        "[abstractRegularAutomaton.from_word] some letters are not contained \
         in the objective alphabet"
    else
      {
        states = StatesSet.add hole states;
        algebra = sa;
        start = a;
        final = StatesSet.singleton l;
        trans = t;
        hole;
        hole_coreach = false;
      }

  let bi_rename (u : dfa) (u' : dfa) =
    let ng = state_generator () in
    let s = get_states u in
    let s' = get_states u' in
    let r =
      StatesSet.fold
        (fun x acc ->
          let x' = ng () in
          Rename.add x x' acc)
        s Rename.empty
    in
    let r' =
      StatesSet.fold
        (fun x acc ->
          let x' = ng () in
          Rename.add x x' acc)
        s' Rename.empty
    in
    (apply_rename r u, apply_rename r' u')

  let concat (u : dfa) (u' : dfa) =
    let u, u' = unify_algebra u u' in
    let u, u' = bi_rename u u' in
    let u = non_determinize u in
    let u' = non_determinize u' in
    let t' =
      nd_fold_trans
        (fun (s, c, s') acc ->
          let acc = nd_add_trans (s, c, s') acc in
          if StatesSet.mem s' u.nd_finals then
            StatesSet.fold
              (fun i' acc -> nd_add_trans (s, c, i') acc)
              u'.nd_init acc
          else acc)
        (DetTrans.merge
           (fun k v v' ->
             match (v, v') with
             | _, None -> v
             | None, _ -> v'
             | _ -> assert false)
           u.nd_trans u'.nd_trans)
        DetTrans.empty
    in
    let nd =
      {
        nd_states = StatesSet.union u.nd_states u'.nd_states;
        nd_init =
          (if StatesSet.is_empty (StatesSet.inter u.nd_init u.nd_finals) then
           u.nd_init
          else StatesSet.union u.nd_init u'.nd_init);
        nd_algebra = u.nd_algebra;
        nd_trans = t';
        nd_finals = u'.nd_finals;
      }
    in
    nd |> determinize

  let star (u : dfa) =
    let u = non_determinize u in
    let g = state_generator () in
    let new_state = g () in
    let r =
      StatesSet.fold (fun s r -> Rename.add s (g ()) r) u.nd_states Rename.empty
    in
    let u = nd_apply_rename r u in
    let t' =
      nd_fold_trans
        (fun (s, c, s') acc ->
          if StatesSet.mem s' u.nd_finals then
            StatesSet.fold
              (fun i acc -> nd_add_trans (s, c, i) acc)
              u.nd_init acc
          else acc)
        u.nd_trans u.nd_trans
    in
    {
      u with
      nd_states = StatesSet.add new_state u.nd_states;
      nd_init = StatesSet.add new_state u.nd_init;
      nd_finals = StatesSet.add new_state u.nd_finals;
      nd_trans = t';
    }
    |> determinize

  type var = string

  let print_var = Format.pp_print_string

  module VMap = Map.Make (struct
    type t = var

    let compare = compare
  end)

  exception NA

  type regexp =
    | L of A.t
    | S of regexp
    | C of (regexp * regexp)
    | A of (regexp * regexp)
    | V of var
    | E
    | N

  let is_u_var_free (u : regexp) =
    let rec aux u cont =
      match u with
      | L _ | E | N -> cont true
      | S u -> aux u cont
      | C (u, v) | A (u, v) -> aux u (fun a -> aux v (fun b -> cont (a && b)))
      | V _ -> false
    in
    aux u (fun x -> x)

  let rewrite (u : regexp) =
    match u with
    | C (A (a, b), c) -> Some (A (C (a, c), C (b, c)))
    | A (C (a, S b), E) when a = b -> Some (S b)
    | C (c, A (a, b)) -> Some (A (C (c, a), C (c, b)))
    | C (x, E) | C (E, x) | A (x, N) | A (N, x) -> Some x
    | S (S x) -> Some (S x)
    | S E | S N -> Some E
    | A (E, S x) | A (S x, E) -> Some (S x)
    | _ -> None

  let visitor (u : regexp) =
    match u with
    | S x -> ([ x ], fun l -> S (List.hd l))
    | C (a, b) -> ([ a; b ], fun l -> C (List.nth l 0, List.nth l 1))
    | A (a, b) -> ([ a; b ], fun l -> A (List.nth l 0, List.nth l 1))
    | _ -> ([], fun l -> u)

  let rec apply_one_rewrite (u : regexp) =
    match rewrite u with
    | None ->
        let l, f = visitor u in
        let l, b =
          List.fold_left
            (fun (accl, accb) x ->
              let y, b = apply_one_rewrite x in
              (y :: accl, accb || b))
            ([], false) l
        in
        (f (List.rev l), b)
    | Some x -> (x, true)

  type dnf = regexp list

  let make_conjunction u v =
    List.fold_left
      (fun acc x -> List.fold_left (fun acc y -> C (x, y) :: acc) acc v)
      [] u

  let expr_precedence = function
    | S _ -> 99
    | C (_, _) -> 6
    | A (_, _) -> 5
    | _ -> 100

  let rec print_regexp fmt (u : regexp) =
    match u with
    | L x -> Format.fprintf fmt "%a" A.print x
    | S v ->
        Format.fprintf fmt "%a*"
          (ToolBox.print_paren
             (expr_precedence u > expr_precedence v)
             (* true *)
             print_regexp)
          v
    | C (v, w) ->
        Format.fprintf fmt "%a%a"
          (ToolBox.print_paren
             (expr_precedence u > expr_precedence v)
             (* true *)
             print_regexp)
          v
          (ToolBox.print_paren
             (expr_precedence u > expr_precedence w)
             (* true *)
             print_regexp)
          w
    | A (v, w) ->
        Format.fprintf fmt "%a|%a"
          (ToolBox.print_paren
             (expr_precedence u > expr_precedence v)
             (* true *)
             print_regexp)
          v
          (ToolBox.print_paren
             (expr_precedence u > expr_precedence w)
             (* true *)
             print_regexp)
          w
    | V v -> Format.fprintf fmt "%a" print_var v
    | E -> Format.fprintf fmt "ε" (* "([]?)" *)
    | N -> Format.fprintf fmt "∅"

  (* "([])" *)

  let rec make_dnf_regexp (u : regexp) =
    match u with
    | N -> []
    | E -> [ E ]
    | C (v, w) -> make_conjunction (make_dnf_regexp v) (make_dnf_regexp w)
    | A (v, w) -> make_dnf_regexp v @ make_dnf_regexp w
    | V v -> [ V v ]
    | L x -> [ L x ]
    | S u -> (
        let u' = make_dnf_regexp u in
        let regopt =
          List.fold_left
            (fun acc x ->
              match acc with
              | Some acc -> Some (C (acc, S x))
              | None -> Some (S x))
            None u'
        in
        match regopt with None -> [ E ] | Some x -> [ S x ])

  let normalize (u : regexp) =
    let x = make_dnf_regexp u in
    let x = ToolBox.rm_doub compare x in
    let u =
      let uopt =
        List.fold_left
          (fun acc x ->
            match acc with Some y -> Some (A (y, x)) | None -> Some x)
          None x
      in
      match uopt with None -> N | Some x -> x
    in
    let rec aux u =
      let u, b = apply_one_rewrite u in
      if b then aux u else u
    in
    aux u

  let pp_print_regexp fmt u =
    let u = normalize u in
    Format.fprintf fmt "%a" print_regexp u

  let to_string u =
    let u = normalize u in
    let rec aux u =
      match u with
      | L x -> ToolBox.format_to_string A.print x
      | S v ->
          let vs = aux v in
          Printf.sprintf "(%s)*" vs
      | C (v, w) ->
          let vs = aux v in
          let ws = aux w in
          Format.sprintf "(%s.%s)" vs ws
      | A (v, w) ->
          let vs = aux v in
          let ws = aux w in
          Format.sprintf "(%s+%s)" vs ws
      | V v -> ToolBox.format_to_string print_var v
      | E -> "eps"
      | N -> "nul"
    in
    aux u

  type system = (var * regexp) list
  type systemvm = (var * (regexp option * regexp VMap.t)) list
  type lr = Left | Right

  let print_systemvm x =
    ToolBox.print_list
      (fun fmt (v, (uo, m)) ->
        match uo with
        | Some u ->
            Format.fprintf fmt "(%s -> (%a + %a))" v print_regexp u
              (ToolBox.print_map_inline Format.pp_print_string print_regexp
                 VMap.bindings)
              m
        | None ->
            Format.fprintf fmt "(%s -> %a)" v
              (ToolBox.print_map_inline Format.pp_print_string print_regexp
                 VMap.bindings)
              m)
      x

  let print_system fmt (s : system) : unit =
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (v, u) ->
           Format.fprintf fmt "%a ?= %a" print_var v print_regexp u))
      s

  let add_vm (c, v) (c', v') =
    let c'' =
      match (c, c') with
      | None, _ -> c'
      | _, None -> c
      | Some a, Some b -> Some (A (a, b))
    in
    ( c'',
      VMap.merge
        (fun k g d ->
          match (g, d) with
          | Some g, Some d -> Some (A (g, d))
          | None, _ -> d
          | _, None -> g)
        v v' )

  let mul_c_vm e (c, v) =
    let c = match c with None -> None | Some st -> Some (C (e, st)) in
    let v = VMap.map (fun x -> C (e, x)) v in
    (c, v)

  let mul_c_vm_right e (c, v) =
    let c = match c with None -> None | Some st -> Some (C (st, e)) in
    let v = VMap.map (fun x -> C (x, e)) v in
    (c, v)

  let rec get_coeff (u : regexp) =
    match u with
    | N -> (Some N, VMap.empty)
    | E -> (Some E, VMap.empty)
    | L a -> (Some (L a), VMap.empty)
    | S v -> (Some (S v), VMap.empty)
    | V v -> (None, VMap.singleton v E)
    | C (v, w) -> (
        let (c, v), (c', v') = (get_coeff v, get_coeff w) in
        let vc =
          if VMap.is_empty v then v'
          else if VMap.is_empty v' then v
          else raise NA
        in
        match (c, c') with
        | None, None -> (None, vc)
        | Some e, None -> (None, VMap.map (fun e' -> C (e, e')) vc)
        | None, Some e -> raise NA
        | Some e, Some e'' ->
            (Some (C (e, e'')), VMap.map (fun e' -> C (e, e')) vc))
    | A (v, w) ->
        let (c, v), (c', v') = (get_coeff v, get_coeff w) in
        add_vm (c, v) (c', v')

  let rec subst x e u =
    match u with
    | N -> u
    | V v when x = v -> e
    | V v -> u
    | L a -> u
    | E -> u
    | S u -> S (subst x e u)
    | C (v, w) -> C (subst x e v, subst x e w)
    | A (v, w) -> A (subst x e v, subst x e w)

  let add_coeff c c' =
    match (c, c') with
    | None, _ -> c'
    | _, None -> c
    | Some x, Some y -> Some (A (x, y))

  let product_lr lr a c = match lr with Left -> C (a, c) | Right -> C (c, a)

  let subst_ne lr x (c', vm') (c, vm) =
    try
      let cx = VMap.find x vm in
      let vm' = VMap.map (fun e' -> product_lr lr cx e') vm' in
      let c' =
        match c' with None -> None | Some cc -> Some (product_lr lr cx cc)
      in
      add_vm (c', vm') (c, VMap.remove x vm)
    with Not_found -> (c, vm)

  let vm_to_regexp lr (c, vm) =
    match c with
    | Some x -> VMap.fold (fun v k acc -> A (product_lr lr k (V v), acc)) vm x
    | None -> (
        try
          let v, k = VMap.choose vm in
          let vm = VMap.remove v vm in
          VMap.fold
            (fun v k acc -> A (product_lr lr k (V v), acc))
            vm (product_lr lr k (V v))
        with Not_found -> N)

  let print_systemm lr fmt l =
    let l' = List.map (fun (a, b) -> (a, vm_to_regexp lr b)) l in
    print_system fmt l'

  let system_simplify (l : systemvm) : systemvm =
    List.map
      (fun (q, (c, vm)) ->
        (q, (ToolBox.map_if_not_none normalize c, VMap.map normalize vm)))
      l

  let arden_solve lr l =
    let rec aux1 l b =
      match l with
      | [] -> b
      | (v, (l', vm)) :: q ->
          let to_subst =
            if VMap.mem v vm then
              let k = VMap.find v vm in
              let vm' = VMap.remove v vm in
              mul_c_vm (S k) (l', vm')
            else (l', vm)
          in
          let q = List.map (fun (y, z) -> (y, subst_ne lr v to_subst z)) q in
          aux1 q ((v, to_subst) :: b)
    in
    let rec aux2 l b =
      match l with
      | (v, e) :: q ->
          let q = List.map (fun (y, z) -> (y, subst_ne lr v e z)) q in
          aux2 q ((v, e) :: b)
      | [] -> b
    in
    let l = aux1 l [] in
    let l = aux2 l [] in
    l

  let regexp_of_automata (a : dfa) =
    let a = make_non_coreachable_hole a in
    let vos = var_of_state in
    let sys =
      fold_trans_nh
        (fun (s, c, s') acc ->
          if VMap.mem (vos s) acc then
            let y = VMap.find (vos s) acc in
            let r = add_vm y (None, VMap.singleton (vos s') (L c)) in
            acc |> VMap.remove (vos s) |> VMap.add (vos s) r
          else
            let r = (None, VMap.singleton (vos s') (L c)) in
            acc |> VMap.add (vos s) r)
        a VMap.empty
    in
    let sys =
      StatesSet.fold
        (fun s acc ->
          if VMap.mem (vos s) acc then
            let c, y = VMap.find (vos s) acc in
            let r = (add_coeff c (Some E), y) in
            acc |> VMap.remove (vos s) |> VMap.add (vos s) r
          else
            let r = (Some E, VMap.empty) in
            acc |> VMap.add (vos s) r)
        a.final sys
    in
    let sys =
      StatesSet.fold
        (fun s acc ->
          match VMap.find_opt (vos s) acc with
          | Some _ -> acc
          | None -> VMap.add (vos s) (None, VMap.empty) acc)
        a.states sys
    in
    let s = arden_solve Left (VMap.bindings sys) in
    try
      let x, a = List.find (fun (x, _) -> x = vos a.start) s in
      let rep = vm_to_regexp Left a in
      if not (is_u_var_free rep) then (
        Format.printf "sol of system: %a is %a: not variable free"
          (print_systemm Left) (VMap.bindings sys) print_regexp rep;
        assert false);
      rep
    with Not_found -> N

  type word = A.t list

  let compare_word = ToolBox.list_compare A.compare

  let print_word fmt w =
    Format.fprintf fmt "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "·")
         A.print)
      w

  type card = Infinite | Finite of word list
  type accepts = AI | AF of word | AN

  let cardinality (u : dfa) =
    let v = regexp_of_automata u in
    let v = normalize v in
    let rec disj_discovery (v : regexp) =
      match v with
      | N | L _ | C _ | S _ | E -> [ v ]
      | V _ -> failwith "[cardinality] regexp with variables"
      | A (e, e') -> disj_discovery e @ disj_discovery e'
    in
    let vl = disj_discovery v in
    let rec accepts_one (v : regexp) =
      match v with
      | A _ | V _ -> failwith "[cardinality] add or var occured"
      | N -> AN
      | E -> AF []
      | L x -> AF [ x ]
      | C (e, f) -> (
          match (accepts_one e, accepts_one f) with
          | AN, _ -> AN
          | _, AN -> AN
          | AI, _ -> AI
          | _, AI -> AI
          | AF l, AF l' -> AF (l @ l'))
      | S e -> (
          match accepts_one e with
          | AN -> AN
          | AI -> AI
          | AF [] -> AF []
          | AF _ -> AI)
    in
    let rep =
      List.fold_left
        (fun acc x ->
          match (acc, accepts_one x) with
          | Infinite, _ -> Infinite
          | _, AI -> Infinite
          | _, AN -> acc
          | Finite wl, AF w ->
              if ToolBox.mem_compare compare_word w wl then Finite wl
              else Finite (w :: wl))
        (Finite []) vl
    in
    match rep with Infinite -> None | Finite wl -> Some (List.length wl)

  let is_cardinality_one (u : dfa) =
    match cardinality u with
    | None -> false
    | Some x when x <> 1 -> false
    | _ -> true

  (* Below is an regexp -> automaton transformation using Brzozowski derivative
     coupled with a notion of weak equivalence over regular expressions
     [https://www.cs.kent.ac.uk/people/staff/sao/documents/jfp09.pdf]. *)
  let tab = Hashtbl.create 12
  let add_cmp r1 r2 cmp = Hashtbl.add tab (r1, r2) cmp

  let syntactic_cmp_eq (r1 : regexp) (r2 : regexp) =
    try Hashtbl.find tab (r1, r2)
    with Not_found ->
      let rec aux tdl =
        match tdl with
        | [] -> 0
        | p :: tdl -> (
            match p with
            | L a, L a' ->
                let x = A.compare a a' in
                if x = 0 then aux tdl else x
            | S x, S x' -> aux ((x, x') :: tdl)
            | A (x, y), A (x', y') | C (x, y), C (x', y') ->
                aux ((x, x') :: (y, y') :: tdl)
            | E, E | N, N -> aux tdl
            | N, _ -> -1
            | _, N -> 1
            | E, _ -> -1
            | _, E -> 1
            | L _, _ -> -1
            | _, L _ -> 1
            | S _, _ -> -1
            | _, S _ -> 1
            | A _, _ -> -1
            | _, A _ -> 1
            | C _, _ -> -1
            | _, C _ -> -1
            | _ -> failwith "[synctactic_eq] variable in regexp")
      in
      let r = aux [ (r1, r2) ] in
      add_cmp r1 r2 r;
      r

  let normalize_eq (r : regexp) =
    let fstrules (r : regexp) =
      match r with
      | C (N, _) | C (_, N) -> Some N
      | C (E, r) | C (r, E) | A (N, r) | A (r, N) -> Some r
      | S N -> Some E
      | S E -> Some E
      | S (S r) -> Some (S r)
      | A (A (r, r'), r'') -> Some (A (r, A (r', r'')))
      | A (r, r') when syntactic_cmp_eq r r' = 0 -> Some r
      | _ -> None
    in
    let rec apply_one_rewrite (u : regexp) =
      match fstrules u with
      | None ->
          let l, f = visitor u in
          let l, b =
            List.fold_left
              (fun (accl, accb) x ->
                let y, b = apply_one_rewrite x in
                (y :: accl, accb || b))
              ([], false) l
          in
          (f (List.rev l), b)
      | Some x -> (x, true)
    in
    let rec aux u =
      let u, b = apply_one_rewrite u in
      if b then aux u else u
    in
    aux r

  let normalize_eq2 (u : regexp list) =
    let x = List.map make_dnf_regexp u |> List.flatten in
    let rec aux u =
      let u, b = apply_one_rewrite u in
      if b then aux u else u
    in
    let x = List.map aux x in
    let x = ToolBox.rm_doub syntactic_cmp_eq x in
    x

  let regexp_of_regexp_list l =
    match l with
    | [] -> N
    | p :: q -> List.fold_left (fun acc x -> A (acc, x)) p l

  let weak_eq (r1 : regexp list) (r2 : regexp list) =
    let r1, r2 = (normalize_eq2 r1, normalize_eq2 r2) in
    List.length r1 = List.length r2
    && List.for_all
         (fun x -> List.exists (fun y -> syntactic_cmp_eq x y = 0) r2)
         r1

  let unit_function rl =
    let rec aux r =
      match r with
      | E -> E
      | L _ | N -> N
      | C (r, s) -> ( match (aux r, aux s) with E, E -> E | _ -> N)
      | A (r, s) -> failwith "[aux] addition in regex"
      | S _ -> E
      | V _ -> failwith "[aux] variable in regexp"
    in
    if List.exists (fun x -> aux x = E) rl then E else N

  let derivate (a : A.t) (rl : regexp list) =
    let rec aux r =
      match r with
      | L x when A.compare x a = 0 -> E
      | L _ | N | E -> N
      | C (r, s) -> A (C (aux r, s), C (unit_function [ r ], aux s))
      | S r -> C (aux r, S r)
      | V _ -> failwith "[unit_function] variable in regexp"
      | A _ -> failwith "[unit_function] addition in regexp"
    in
    List.map aux rl |> normalize_eq2

  module RWEQMap = struct
    let empty = []
    let add a b x = (a, b) :: x

    let rec find a x =
      match x with
      | [] -> raise Not_found
      | (p, b) :: q -> if weak_eq a p then b else find a q

    let fold f s acc = List.fold_left (fun acc (x, y) -> f x y acc) acc s
  end

  let automata_of_regexp_bis (sa : Algebra.t) (r : regexp) =
    Hashtbl.reset tab;
    let r = normalize_eq2 [ r ] in
    let assoc = RWEQMap.empty in
    let () = S.restart () in
    let init = S.fresh () in
    let assoc = RWEQMap.add r init assoc in
    let hole = S.fresh () in
    let assoc = RWEQMap.add [] hole assoc in
    let rec aux todo trans (assoc : (regexp list * state) list) finals =
      match todo with
      | (p, id) :: q ->
          let trans, assoc, todo, finals =
            Algebra.fold
              (fun l
                   (trans, (assoc : (regexp list * state) list), todos, finals) ->
                let x = derivate l p in
                try
                  let xid = RWEQMap.find x assoc in
                  if xid = hole then (trans, assoc, todos, finals)
                  else
                    let finals =
                      if unit_function x = E then StatesSet.add xid finals
                      else finals
                    in
                    (DetTrans.add (l, id) xid trans, assoc, todos, finals)
                with Not_found ->
                  let xid = S.fresh () in
                  let finals =
                    if unit_function x = E then StatesSet.add xid finals
                    else finals
                  in
                  ( DetTrans.add (l, id) xid trans,
                    RWEQMap.add x xid assoc,
                    (x, xid) :: todos,
                    finals ))
              sa (trans, assoc, q, finals)
          in
          aux todo trans assoc finals
      | [] -> (trans, assoc, finals)
    in
    let trans, assoc, finals =
      aux
        [ (r, init) ]
        DetTrans.empty assoc
        (if unit_function r = E then StatesSet.singleton init
        else StatesSet.empty)
    in
    {
      states =
        RWEQMap.fold
          (fun _ id acc -> StatesSet.add id acc)
          assoc StatesSet.empty;
      algebra = sa;
      start = init;
      final = finals;
      trans;
      hole;
      hole_coreach = false;
    }
    |> minimization

  let rec automata_of_regexp_no_mini (sa : Algebra.t) (r : regexp) =
    match r with
    | L a -> from_word sa [ a ]
    | S r' ->
        let a = automata_of_regexp_no_mini sa r' in
        star a
    | C (a, b) ->
        let a = automata_of_regexp_no_mini sa a in
        let b = automata_of_regexp_no_mini sa b in
        concat a b
    | A (a, b) ->
        let a = automata_of_regexp_no_mini sa a in
        let b = automata_of_regexp_no_mini sa b in
        join a b
    | V _ -> failwith "[automata_of_regexp_no_mini] variable in regexp"
    | E -> from_word sa []
    | N -> bottom Algebra.empty

  let automata_of_regexp (aa : Algebra.t) x =
    x
    |> automata_of_regexp_no_mini aa
    |> (fun y -> change_algebra y aa)
    |> minimization

  let equ a b = leq a b && leq b a

  let default_eq a b =
    let aa = get_algebra a and bb = get_algebra b in
    Algebra.compare aa bb = 0 && equ a b

  let derivative (u : dfa) (a : A.t) =
    try
      let q = DetTrans.find (a, u.start) u.trans in
      { u with start = q }
    with Not_found -> bottom u.algebra

  let head (u : dfa) =
    let u = make_non_coreachable_hole u in
    let a = u.algebra in
    Algebra.fold (fun x acc -> (x, derivative u x) :: acc) a []

  let fresh_rename (u : dfa) (f : unit -> S.t) =
    let module MS = Map.Make (S) in
    let memo = MS.empty in
    let get_name x memo =
      try (MS.find x memo, memo)
      with Not_found ->
        let nx = f () in
        (nx, MS.add x nx memo)
    in
    let nstates, memo =
      StatesSet.fold
        (fun x (res, memo) ->
          let x, memo = get_name x memo in
          (StatesSet.add x res, memo))
        u.states (StatesSet.empty, memo)
    in
    let nstart, memo = get_name u.start memo in
    let nfinal, memo =
      StatesSet.fold
        (fun x (res, memo) ->
          let x, memo = get_name x memo in
          (StatesSet.add x res, memo))
        u.final (StatesSet.empty, memo)
    in
    let ntrans, memo =
      DetTrans.fold
        (fun (s, q) q' (res, memo) ->
          let q, memo = get_name q memo in
          let q', memo = get_name q' memo in
          (DetTrans.add (s, q) q' res, memo))
        u.trans (DetTrans.empty, memo)
    in
    let nhole, memo = get_name u.hole memo in
    {
      u with
      states = nstates;
      start = nstart;
      final = nfinal;
      trans = ntrans;
      hole = nhole;
    }

  let integrate (u : dfa) (a : A.t) =
    let u = make_non_coreachable_hole u in
    let () = S.restart () in
    let new_start = create_fresh_state u.states S.fresh in
    {
      u with
      states = StatesSet.add new_start u.states;
      start = new_start;
      trans = DetTrans.add (a, new_start) u.start u.trans;
    }

  let print_dot (fmt : Format.formatter) (x : dfa) : unit =
    let lstates = StatesSet.elements x.states in
    let ltrans = DetTrans.bindings x.trans in
    Format.fprintf fmt "@[<v 2>digraph{%a;@,@,%a%s}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
         (fun fmt q ->
           if S.compare q x.start = 0 then
             if StatesSet.mem q x.final then
               Format.fprintf fmt "%a [shape=doublecircle, color=red]" S.print q
             else Format.fprintf fmt "%a [color=red]" S.print q
           else if StatesSet.mem q x.final then
             Format.fprintf fmt "%a [shape=doublecircle]" S.print q
           else Format.fprintf fmt "%a " S.print q))
      lstates
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
         (fun fmt ((a, q), q') ->
           Format.fprintf fmt "%a -> %a [label=\"%a\"]" S.print q S.print q'
             A.print a))
      ltrans
      (if ltrans = [] then "" else ";")

  (* Code below has not been very tested but initial experimental results
     suggested that it was slower to transform a regular expression to an dfa
     than the previous method *)

  type norm_nfa = {
    nnfa_states : StatesSet.t;
    nnfa_start : S.t;
    nnfa_stop : S.t;
    nnfa_trans : (S.t * A.t option * S.t) list;
    nnfa_algebra : Algebra.t;
  }

  let print_dot_nnfa (fmt : Format.formatter) (x : norm_nfa) : unit =
    let lstates = StatesSet.elements x.nnfa_states in
    Format.fprintf fmt "@[<v 2>digraph{%a;@,@,%a%s}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
         (fun fmt q ->
           if S.compare q x.nnfa_start = 0 then
             if S.compare q x.nnfa_stop = 0 then
               Format.fprintf fmt "%a [shape=doublecircle, color=red]" S.print q
             else Format.fprintf fmt "%a [color=red]" S.print q
           else if S.compare q x.nnfa_stop = 0 then
             Format.fprintf fmt "%a [shape=doublecircle]" S.print q
           else Format.fprintf fmt "%a " S.print q))
      lstates
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
         (fun fmt (q, a, q') ->
           Format.fprintf fmt "%a -> %a [label=\"%a\"]" S.print q S.print q'
             (fun fmt a ->
               match a with
               | None -> Format.fprintf fmt "ε"
               | Some a -> Format.fprintf fmt "%a" A.print a)
             a))
      x.nnfa_trans
      (if x.nnfa_trans = [] then "" else ";")

  let log_dot_nfa x f =
    let fx = open_out f in
    let ffx = Format.formatter_of_out_channel fx in
    print_dot_nnfa ffx x;
    Format.pp_print_flush ffx ();
    close_out fx

  let print_norm_nfa fmt x =
    Format.fprintf fmt
      "@[<v 2>{@,\
       @[<v>states: %a@,\
       algebra: %a@,\
       start: %a@,\
       final: %a@,\
       trans: @[<v>%a@]@]@,\
       }@]"
      print_states x.nnfa_states print_algebra x.nnfa_algebra print_state
      x.nnfa_start print_state x.nnfa_stop
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (x, l, y) ->
           match l with
           | None -> Format.fprintf fmt "%a ~> %a" S.print x S.print y
           | Some a ->
               Format.fprintf fmt "%a ~%a~> %a" S.print x A.print a S.print y))
      x.nnfa_trans

  let apply_rename_norm_nfa r u =
    let rename_find x r = try Rename.find x r with Not_found -> x in
    {
      u with
      nnfa_states = StatesSet.map (fun x -> rename_find x r) u.nnfa_states;
      nnfa_start = rename_find u.nnfa_start r;
      nnfa_stop = rename_find u.nnfa_stop r;
      nnfa_trans =
        List.map
          (fun (x, a, y) -> (rename_find x r, a, rename_find y r))
          u.nnfa_trans;
    }

  let bi_rename_norm_nfa g u v =
    let r1 =
      StatesSet.fold
        (fun x acc -> Rename.add x (g ()) acc)
        u.nnfa_states Rename.empty
    in
    let r2 =
      StatesSet.fold
        (fun x acc -> Rename.add x (g ()) acc)
        v.nnfa_states Rename.empty
    in
    (apply_rename_norm_nfa r1 u, apply_rename_norm_nfa r2 v)

  let rename_norm_nfa g u =
    let r1 =
      StatesSet.fold
        (fun x acc -> Rename.add x (g ()) acc)
        u.nnfa_states Rename.empty
    in
    apply_rename_norm_nfa r1 u

  let nnfa_join (u : norm_nfa) (v : norm_nfa) =
    let () = S.restart () in
    let u, v = bi_rename_norm_nfa (fun () -> S.fresh ()) u v in
    let new_start = S.fresh () in
    let new_stop = S.fresh () in
    let res =
      {
        nnfa_states =
          StatesSet.union u.nnfa_states v.nnfa_states
          |> StatesSet.union (StatesSet.of_list [ new_start; new_stop ]);
        nnfa_start = new_start;
        nnfa_stop = new_stop;
        nnfa_trans =
          [
            (new_start, None, u.nnfa_start);
            (new_start, None, v.nnfa_start);
            (u.nnfa_stop, None, new_stop);
            (v.nnfa_stop, None, new_stop);
          ]
          @ u.nnfa_trans @ v.nnfa_trans;
        nnfa_algebra = Algebra.union u.nnfa_algebra v.nnfa_algebra;
      }
    in
    res

  let letter (a : A.t) =
    let () = S.restart () in
    let start = S.fresh () in
    let stop = S.fresh () in
    {
      nnfa_start = start;
      nnfa_stop = stop;
      nnfa_trans = [ (start, Some a, stop) ];
      nnfa_states = StatesSet.of_list [ start; stop ];
      nnfa_algebra = Algebra.of_list [ a ];
    }

  let epsilon () =
    let () = S.restart () in
    let start = S.fresh () in
    let stop = S.fresh () in
    {
      nnfa_start = start;
      nnfa_stop = stop;
      nnfa_trans = [ (start, None, stop) ];
      nnfa_states = StatesSet.of_list [ start; stop ];
      nnfa_algebra = Algebra.of_list [];
    }

  let empty () =
    let () = S.restart () in
    let start = S.fresh () in
    let stop = S.fresh () in
    {
      nnfa_start = start;
      nnfa_stop = stop;
      nnfa_trans = [];
      nnfa_states = StatesSet.of_list [ start; stop ];
      nnfa_algebra = Algebra.of_list [];
    }

  let nnfa_star (u : norm_nfa) =
    let () = S.restart () in
    let start = S.fresh () in
    let stop = S.fresh () in
    (* log_dot_nfa u "output/sin.dot"; *)
    let u = rename_norm_nfa S.fresh u in
    let res =
      {
        u with
        nnfa_states =
          StatesSet.union (StatesSet.of_list [ start; stop ]) u.nnfa_states;
        nnfa_start = start;
        nnfa_stop = stop;
        nnfa_trans =
          (start, None, u.nnfa_start)
          :: (u.nnfa_stop, None, stop)
          :: (u.nnfa_stop, None, u.nnfa_start)
          :: (u.nnfa_start, None, u.nnfa_stop)
          :: u.nnfa_trans;
      }
    in
    (* log_dot_nfa res "output/sout.dot"; *)
    res

  let nnfa_concat (u : norm_nfa) (v : norm_nfa) =
    let () = S.restart () in
    let u, v = bi_rename_norm_nfa S.fresh u v in
    let r = Rename.singleton v.nnfa_start u.nnfa_stop in
    let v = apply_rename_norm_nfa r v in
    {
      nnfa_states = StatesSet.union u.nnfa_states v.nnfa_states;
      nnfa_start = u.nnfa_start;
      nnfa_stop = v.nnfa_stop;
      nnfa_trans = u.nnfa_trans @ v.nnfa_trans;
      nnfa_algebra = Algebra.union u.nnfa_algebra v.nnfa_algebra;
    }

  let overload_epsilons (u : norm_nfa) =
    let exception Found of S.t * A.t option * S.t in
    let find_possible_new_epsilon u =
      List.iter
        (fun (x, l, y) ->
          List.iter
            (fun (x', l', y') ->
              if S.compare x' y = 0 then
                match (l, l') with
                | Some a, None | None, Some a ->
                    if
                      List.exists
                        (fun (x'', l'', y'') ->
                          S.compare x x'' = 0
                          && S.compare y' y'' = 0
                          &&
                          match l'' with
                          | None -> false
                          | Some b -> A.compare b a = 0)
                        u.nnfa_trans
                    then ()
                    else raise (Found (x, Some a, y'))
                | None, None ->
                    if
                      List.exists
                        (fun (x'', l'', y'') ->
                          S.compare x x'' = 0
                          && S.compare y' y'' = 0
                          && match l'' with None -> true | Some b -> false)
                        u.nnfa_trans
                    then ()
                    else raise (Found (x, None, y'))
                | _ -> ())
            u.nnfa_trans)
        u.nnfa_trans
    in
    let rec aux u =
      try
        find_possible_new_epsilon u;
        u
      with Found (x, l, y) ->
        aux { u with nnfa_trans = (x, l, y) :: u.nnfa_trans }
    in
    aux u

  let to_dfa (a : Algebra.t) (u : norm_nfa) =
    let u = overload_epsilons u in
    let nd_init =
      List.fold_left
        (fun acc (x, l, y) ->
          if l = None && S.compare x u.nnfa_start = 0 then StatesSet.add y acc
          else acc)
        (StatesSet.singleton u.nnfa_start)
        u.nnfa_trans
    in
    let nd_finals =
      List.fold_left
        (fun acc (x, l, y) ->
          if l = None && S.compare y u.nnfa_stop = 0 then StatesSet.add y acc
          else acc)
        (StatesSet.singleton u.nnfa_stop)
        u.nnfa_trans
    in
    let nd_trans =
      List.fold_left
        (fun acc (x, l, y) ->
          match l with None -> acc | Some a -> nd_add_trans (x, a, y) acc)
        DetTrans.empty u.nnfa_trans
    in
    let nd =
      {
        nd_states = u.nnfa_states;
        nd_init;
        nd_finals;
        nd_trans;
        nd_algebra = a;
      }
    in
    nd |> determinize |> minimization

  let automata_of_regexp_ter (a : Algebra.t) r =
    let rec aux r =
      match r with
      | L a -> letter a
      | N -> empty ()
      | E -> epsilon ()
      | A (a, b) -> nnfa_join (aux a) (aux b)
      | C (a, b) -> nnfa_concat (aux a) (aux b)
      | S a -> nnfa_star (aux a)
      | V _ -> failwith "[automata_of_regexp_ter] variable in regexp"
    in
    let part = aux r in
    to_dfa a part

  (* Code below has not been tested a lot, but initial experimental results
     suggested that it produced bigger regular expression than solving the
     gaussian system *)
  type rnfa = {
    rnfa_states : StatesSet.t;
    rnfa_start : StatesSet.t;
    rnfa_stop : StatesSet.t;
    rnfa_trans : (S.t * regexp * S.t) list;
    rnfa_algebra : Algebra.t;
  }

  let print_rnfa fmt x =
    Format.fprintf fmt
      "@[<v 2>{@,\
       @[<v>states: %a@,\
       algebra: %a@,\
       start: %a@,\
       final: %a@,\
       trans: @[<v>%a@]@]@,\
       }@]"
      print_states x.rnfa_states print_algebra x.rnfa_algebra print_states
      x.rnfa_start print_states x.rnfa_stop
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (x, r, y) ->
           Format.fprintf fmt "%a ~%a~> %a" print_state x print_regexp r
             print_state y))
      x.rnfa_trans

  let apply_rename_norm_rnfa r u =
    let rename_find x r = try Rename.find x r with Not_found -> x in
    {
      u with
      rnfa_states = StatesSet.map (fun x -> rename_find x r) u.rnfa_states;
      rnfa_start = StatesSet.map (fun x -> rename_find x r) u.rnfa_start;
      rnfa_stop = StatesSet.map (fun x -> rename_find x r) u.rnfa_stop;
      rnfa_trans =
        List.map
          (fun (x, a, y) -> (rename_find x r, a, rename_find y r))
          u.rnfa_trans;
    }

  let regexp_of_rnfa x =
    let g = state_generator () in
    let new_start = g () in
    let new_stop = g () in
    let r =
      StatesSet.fold
        (fun s r -> Rename.add s (g ()) r)
        x.rnfa_states Rename.empty
    in
    let x = apply_rename_norm_rnfa r x in
    let x =
      {
        x with
        rnfa_start = StatesSet.singleton new_start;
        rnfa_stop = StatesSet.singleton new_stop;
        rnfa_trans =
          x.rnfa_trans
          |> StatesSet.fold (fun x acc -> (new_start, E, x) :: acc) x.rnfa_start
          |> StatesSet.fold (fun x acc -> (x, E, new_stop) :: acc) x.rnfa_stop;
        rnfa_states =
          StatesSet.union
            (StatesSet.of_list [ new_start; new_stop ])
            x.rnfa_states;
      }
    in
    let remove_state s x =
      let trans' =
        List.filter
          (fun (s1, _, s2) -> S.compare s s1 != 0 && S.compare s s2 != 0)
          x.rnfa_trans
      in
      let new_trans =
        List.fold_left
          (fun acc (s1, a, s2) ->
            List.fold_left
              (fun acc (s3, b, s4) ->
                List.fold_left
                  (fun acc (s5, c, s6) ->
                    if
                      S.compare s1 s != 0
                      && S.compare s2 s = 0
                      && S.compare s3 s = 0
                      && S.compare s4 s = 0
                      && S.compare s5 s = 0
                      && S.compare s6 s != 0
                    then (s1, C (a, C (S b, c)), s6) :: acc
                    else acc)
                  acc x.rnfa_trans)
              acc x.rnfa_trans)
          [] x.rnfa_trans
      in
      let new_trans2 =
        List.fold_left
          (fun acc (s1, a, s2) ->
            List.fold_left
              (fun acc (s5, c, s6) ->
                if
                  S.compare s1 s != 0
                  && S.compare s2 s = 0
                  && S.compare s5 s = 0
                  && S.compare s6 s != 0
                then (s1, C (a, c), s6) :: acc
                else acc)
              acc x.rnfa_trans)
          [] x.rnfa_trans
      in
      {
        x with
        rnfa_trans = trans' @ new_trans @ new_trans2;
        rnfa_states = StatesSet.remove s x.rnfa_states;
      }
    in
    let rec aux x =
      let middle_state =
        StatesSet.diff x.rnfa_states (StatesSet.union x.rnfa_start x.rnfa_stop)
      in
      if StatesSet.is_empty middle_state then x
      else aux (remove_state (StatesSet.choose middle_state) x)
    in
    let x = aux x in
    List.fold_left
      (fun acc (s1, l, s2) ->
        assert (S.compare s1 new_start = 0 && S.compare s2 new_stop = 0);
        A (l, acc))
      N x.rnfa_trans

  let rnfa_of_dfa (d : dfa) =
    let d = make_non_coreachable_hole d in
    {
      rnfa_algebra = d.algebra;
      rnfa_states = d.states;
      rnfa_start = StatesSet.singleton d.start;
      rnfa_stop = d.final;
      rnfa_trans =
        DetTrans.fold (fun (l, q) q' acc -> (q, L l, q') :: acc) d.trans [];
    }

  let regexp_of_dfa (d : dfa) = d |> rnfa_of_dfa |> regexp_of_rnfa

  let rec get_word (x : dfa) predicate : Algebra.elt list option =
    if x.hole_coreach then
      failwith "cannot infer word from automata with coreaching codition";
    let start = x.start in
    let explored = StatesSet.singleton start in
    let queue = Queue.create () in
    let x' =
      {
        x with
        trans =
          DetTrans.filter (fun (sym, state) value -> predicate sym) x.trans;
      }
    in
    (* we add the first state with the empty word. *)
    Queue.add (start, []) queue;
    bfs x' queue explored

  (** Breadth-first search for a final state. *)
  and bfs (dfa : dfa) queue (explored : states) =
    if Queue.is_empty queue then None
    else
      let state, word = Queue.take queue in
      let reached_final_state =
        if dfa.hole_coreach then not (StatesSet.mem state dfa.final)
        else StatesSet.mem state dfa.final
      in
      (* Found a final state: we can return the word. *)
      if reached_final_state then Some word
        (* Else we add the successors what we did not explore to the queue and
           we do a recursive call. *)
      else bfs_rec dfa queue explored state word

  and bfs_rec dfa queue explored state word =
    let trans_from_current_state =
      DetTrans.to_seq dfa.trans
      |> Seq.filter (fun ((_, from), _) -> S.compare from state = 0)
    in
    let destination_from_current_state =
      Seq.map
        (fun ((symbol, _), dest) -> (dest, symbol))
        trans_from_current_state
    in
    let unexplored_destinations =
      Seq.filter
        (fun (dest, sym) -> not (StatesSet.mem dest explored))
        destination_from_current_state
    in
    add_successors unexplored_destinations word queue;
    let new_explored =
      add_to_explored (List.of_seq unexplored_destinations) explored
    in
    bfs dfa queue new_explored

  and add_successors successors word queue =
    Seq.iter
      (fun (dest, symbol) -> Queue.add (dest, word @ [ symbol ]) queue)
      successors

  and add_to_explored successors explored =
    match successors with
    | [] -> explored
    | (dest, _) :: t ->
        let explored' = StatesSet.add dest explored in
        add_to_explored t explored'
end
