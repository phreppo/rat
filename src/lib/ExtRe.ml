(** Extended regualar expressions. *)
type t =
  | Empty
  | Epsilon
  | Char of Charset.t
  | Concat of t list (* minimum two elements. *)
  | Alternative of t list (* sorted, with minimum two elements. *)
  | Star of t
  | Inter of t list (* sorted, with minimum two elements. *)
  | Compl of t

let rec compare re1 re2 =
  match (re1, re2) with
  | Empty, Empty -> 0
  | Empty, _ -> -1
  | _, Empty -> 1
  | Epsilon, Epsilon -> 0
  | Epsilon, _ -> -1
  | _, Epsilon -> 1
  | Char chrs1, Char chrs2 -> Charset.compare chrs1 chrs2
  | Char _, _ -> -1
  | _, Char _ -> 1
  | Concat [], Concat [] -> 0
  | Concat _, Concat [] -> 1
  | Concat [], Concat _ -> -1
  | Concat (x :: xs), Concat (y :: ys) ->
      compare_elements x y xs ys (fun element -> Concat element)
  | Concat _, _ -> -1
  | _, Concat _ -> 1
  | Star r, Star r' -> compare r r'
  | Star _, _ -> -1
  | _, Star _ -> 1
  | Alternative [], Alternative [] -> 0
  | Alternative _, Alternative [] -> 1
  | Alternative [], Alternative _ -> -1
  | Alternative (x :: xs), Alternative (y :: ys) ->
      compare_elements x y xs ys (fun element -> Alternative element)
  | Alternative _, _ -> -1
  | _, Alternative _ -> 1
  | Inter [], Inter [] -> 0
  | Inter _, Inter [] -> 1
  | Inter [], Inter _ -> -1
  | Inter (x :: xs), Inter (y :: ys) ->
      compare_elements x y xs ys (fun element -> Inter element)
  | Inter _, _ -> -1
  | _, Inter _ -> 1
  | Compl re1, Compl re2 -> compare re2 re1

and compare_elements x y xs ys constructor =
  let comparison = compare x y in
  if comparison = 0 then compare (constructor xs) (constructor ys)
  else comparison

let eq re1 re2 = compare re1 re2 = 0
let empty_str_repr = "\195\152"
let epsilon_str_repr = "\xce\xb5"
let neg_str_repr = "\xc2\xac"

let rec to_string re =
  match re with
  | Empty -> empty_str_repr
  | Epsilon -> epsilon_str_repr
  | Char a -> Charset.to_string a
  | Concat rs -> list_of_res_to_string "" rs
  | Star r -> "(" ^ to_string r ^ ")*"
  | Alternative rs -> list_of_res_to_string " ∪ " rs
  | Inter rs -> list_of_res_to_string " ∩ " rs
  | Compl r -> neg_str_repr ^ to_string r

and list_of_res_to_string separator res =
  let inner = String.concat "" (List.map to_string res) in
  "(" ^ inner ^ ")"

let pp fmt re = Format.pp_print_string fmt (to_string re)

(** {1 Smart Constructors.} *)

let empty = Empty
let eps = Epsilon
let chr c = if Charset.is_empty c then Empty else Char c

let inter re1 re2 =
  if re1 == re2 || re1 = re2 then re1
  else
    match (re1, re2) with
    | Inter re1', Inter re2' -> Inter (List.sort_uniq compare (re1' @ re2'))
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Inter re1', _ -> Inter (List.sort_uniq compare (re2 :: re1'))
    | _, Inter re2' -> Inter (List.sort_uniq compare (re1 :: re2'))
    | _, _ -> (
        match List.sort_uniq compare [ re1; re2 ] with
        | [ r ] -> r
        | rs -> Inter rs)

let alternative re1 re2 =
  if re1 == re2 || re1 = re2 then re1
  else
    match (re1, re2) with
    | Alternative re1', Alternative re2' ->
        Alternative (List.sort_uniq compare (re1' @ re2'))
    | Empty, _ -> re2
    | _, Empty -> re1
    | Alternative re1', _ -> Alternative (List.sort_uniq compare (re2 :: re1'))
    | _, Alternative re2' -> Alternative (List.sort_uniq compare (re1 :: re2'))
    | _ -> (
        match List.sort_uniq compare [ re1; re2 ] with
        | [ r ] -> r
        | rs -> Alternative rs)

let concat re1 re2 =
  match (re1, re2) with
  | Concat rs, Concat ts -> Concat (rs @ ts)
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Epsilon, _ -> re2
  | _, Epsilon -> re1
  | Concat re1', _ -> Concat (re1' @ [ re2 ])
  | _, Concat re2' -> Concat (re1 :: re2')
  | _ -> Concat [ re1; re2 ]

let star = function
  | Star r' -> Star r'
  | Epsilon -> Epsilon
  | Empty -> Epsilon (* The star of empty is epsilon. *)
  | _ as r -> Star r

let plus re = concat re (star re)
let any_char = chr Charset.all_symbols
let universal = any_char |> star

let compl = function
  | Compl re -> re
  | Empty -> universal
  | _ as r -> if r == universal || r = universal then Empty else Compl r

(** {1 Inclusion Algorithm.}

    This algorithm is taken from the paper "A parametric abstract domain for
    lattice-valued regular expressions." by Midtgaard, Jan, Flemming Nielson,
    and Hanne Riis Nielson (2016). See the paper for the details. *)

(** [nullable re] is [true] iff [re] recognizes the empty word. *)
let rec nullable = function
  | Empty -> false
  | Epsilon -> true
  | Char _ -> false
  | Star _ -> true
  | Concat res -> List.for_all nullable res
  | Alternative rs -> List.exists nullable rs
  | Inter res -> List.for_all nullable res
  | Compl re' -> not (nullable re')

(** Set of sets of chars. *)
module CharsetSet = struct
  include Set.Make (struct
    type t = Charset.t

    let compare = Charset.compare
  end)

  (** [partition chars] is [{chars, all_chars - chars}]. *)
  let partition c =
    let c_complement =
      List.fold_left
        (fun rest m -> if Charset.mem m c then rest else Charset.add m rest)
        Charset.empty Utility.all_chars
    in
    let part = add c empty in
    if Charset.is_empty c_complement then part else add c_complement part

  let fold_partition f acc ms = fold (fun eq acc' -> f acc' eq) ms acc

  (** overlays two partitions of sets of sets of characters. *)
  let overlay cs cs' =
    (* TODO: refactor this function.*)
    let reptbl = Hashtbl.create (List.length Utility.all_chars) in
    let () =
      iter
        (fun eq_class ->
          let rep = Charset.min_elt eq_class in
          Charset.iter (fun e -> Hashtbl.add reptbl e rep) eq_class)
        cs'
    in
    fold
      (fun eq acc ->
        let _, acc'' =
          Charset.fold
            (fun elem (rest, acc') ->
              let same, rest' =
                Charset.partition
                  (fun elem' ->
                    Hashtbl.find reptbl elem = Hashtbl.find reptbl elem')
                  rest
              in
              if Charset.is_empty same then (rest', acc')
              else (rest', add same acc'))
            eq (eq, acc)
        in
        acc'')
      cs empty

  (** [any_char_singleton] is the singleton containing the set of all chars. *)
  let any_char_singleton = partition Charset.all_symbols
end

(** [derivative a re] is the derivative of [re] with respect to the symbols [a]. *)
let rec derivative a = function
  | Epsilon -> Empty
  | Char b -> if Charset.subset a b then Epsilon else Empty
  | Empty -> Empty
  | Concat [] -> failwith "internal, empty concatenation in d"
  | Concat [ r ] -> derivative a r
  | Concat [ r; s ] ->
      if nullable r then
        alternative (concat (derivative a r) s) (derivative a s)
      else concat (derivative a r) s
  | Concat (r :: rs) ->
      if nullable r then
        alternative
          (concat (derivative a r) (Concat rs))
          (derivative a (Concat rs))
      else concat (derivative a r) (Concat rs)
  | Star r' as r -> concat (derivative a r') r
  | Alternative [] -> failwith "internal, empty alternative in d"
  | Alternative (r :: rs) ->
      List.fold_right
        (fun r acc -> alternative (derivative a r) acc)
        rs (derivative a r)
  | Inter [] -> failwith "internal, empty intersection in d"
  | Inter (r :: rs) ->
      List.fold_right
        (fun r acc -> inter (derivative a r) acc)
        rs (derivative a r)
  | Compl r' -> compl (derivative a r')

let rec range = function
  | Empty -> CharsetSet.any_char_singleton
  | Epsilon -> CharsetSet.any_char_singleton
  | Char a -> CharsetSet.partition a
  | Concat [] -> CharsetSet.any_char_singleton
  | Concat (r :: rs) ->
      if nullable r then CharsetSet.overlay (range r) (range (Concat rs))
      else range r
  | Star r' -> range r'
  | Alternative [] -> failwith "internal, empty union in range"
  | Alternative (r :: rs) ->
      List.fold_left
        (fun acc r -> CharsetSet.overlay (range r) acc)
        (range r) rs
  | Inter [] -> failwith "internal, empty intersection in range"
  | Inter (r :: rs) ->
      List.fold_left
        (fun acc r -> CharsetSet.overlay (range r) acc)
        (range r) rs
  | Compl r' -> range r'

exception NotLeq

let lang_leq re1 re2 =
  (* TODO: refactor this function.*)
  let repr m = Charset.singleton (Charset.min_elt m) in
  let cache = Hashtbl.create 50 in
  let rec memoleq re1 re2 =
    try Hashtbl.find cache (re1, re2)
    with Not_found -> (
      if re1 == re2 || re1 = re2 (* address or structural equality *) then
        (* a little optimization of reflexive queries *)
        Hashtbl.add cache (re1, re2) () (* add elem to model simulation / gfp *)
      else
        match (nullable re1, nullable re2) with
        | true, false -> raise NotLeq
        | true, true | false, _ ->
            let partition = CharsetSet.overlay (range re1) (range re2) in
            Hashtbl.add cache (re1, re2) ();
            (* add elem to model simulation / gfp *)
            CharsetSet.fold_partition
              (fun () eqclass ->
                let a = repr eqclass in
                let re1' = derivative a re1 in
                (* a is a repr. of equiv.class *)
                let re2' = derivative a re2 in
                memoleq re1' re2')
              () partition)
  in
  try
    memoleq re1 re2;
    true
  with NotLeq -> false

let lang_eq re1 re2 = lang_leq re1 re2 && lang_leq re2 re1
let lang_universal re = lang_leq universal re
let lang_empty re = lang_leq re empty
let simplify_if_empty_lang re = if lang_empty re then empty else re

(** {1 Word Generation} *)

module Aut = RegularAutomata.Instances.CharAutomaton

(** Algebra that must be used in the automata. *)
let algebra = Aut.Algebra.of_list Utility.all_chars

let rec to_dfa (re : t) : Aut.dfa =
  match re with
  | Empty -> Aut.N |> Aut.automata_of_regexp algebra
  | Epsilon -> Aut.E |> Aut.automata_of_regexp algebra
  | Char chars -> chars_to_dfa chars
  | Concat rs -> combine Aut.concat rs
  | Alternative rs -> combine Aut.join rs
  | Star re' -> to_dfa re' |> Aut.star
  | Inter rs -> combine Aut.meet rs
  | Compl re' -> to_dfa re' |> Aut.complementary'

and chars_to_dfa chars =
  (* First we translate to the alternative of the characters. *)
  match Charset.choose_opt chars with
  | None -> failwith "empty set of characters in regex"
  | Some c ->
      let other_chars = Charset.remove c chars in
      (* Translate as follows: {a,b,c} -> (a|b|c). *)
      Charset.fold (fun chr acc -> Aut.A (L chr, acc)) other_chars (Aut.L c)
      |> Aut.automata_of_regexp algebra

(** [combine f rs] first translates [rs] to a list of dfas and then combines
    them frmo left to right with [f].*)
and combine func rs =
  let dfas = List.map to_dfa rs in
  match dfas with
  | [] -> failwith "empty concatenation in regex"
  | fst_dfa :: dfas' -> List.fold_left func fst_dfa dfas'

let get_word re predicate =
  let dfa = to_dfa re in
  match Aut.get_word dfa predicate with
  | Some w -> List.to_seq w |> String.of_seq |> Option.some
  | None -> None

let rec aut_re_to_ext_re (re : Aut.regexp) =
  match re with
  | L c -> Char (Charset.singleton c)
  | S re1 -> star (aut_re_to_ext_re re1)
  | C (re1, re2) -> concat (aut_re_to_ext_re re1) (aut_re_to_ext_re re2)
  | A (re1, re2) -> alternative (aut_re_to_ext_re re1) (aut_re_to_ext_re re2)
  | E -> eps
  | N -> empty
  | V _ ->
      failwith
        "Impossible to convert variables in automata regular expressions to \
         extended regular expressions."

let simplify re =
  let open Aut in
  to_dfa re |> minimization |> regexp_of_automata |> aut_re_to_ext_re
