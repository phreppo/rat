open Re
module U = Utility

(* Set of regexes used by the algorithm *)
module RS = Re.ReSet

(* Atomic element of the result of the analysis *)
module AttackFamily = struct
  type t = { prefix : ExtRe.t; pump : ExtRe.t; suffix : ExtRe.t }

  let compare af1 af2 =
    let prefix_compare = ExtRe.compare af1.prefix af2.prefix in
    let pump_compare = ExtRe.compare af1.pump af2.pump in
    let suffix_compare = ExtRe.compare af1.suffix af2.suffix in
    if prefix_compare <> 0 then prefix_compare
    else if pump_compare <> 0 then pump_compare
    else suffix_compare

  let to_string attack =
    U.blue_str " prefix: "
    ^ ExtRe.to_string attack.prefix
    ^ U.blue_str " pump: "
    ^ ExtRe.to_string attack.pump
    ^ U.blue_str " suffix: "
    ^ ExtRe.to_string attack.suffix

  let to_lang attack_family =
    let prefix_pump = ExtRe.concat attack_family.prefix attack_family.pump in
    let prefix_pump_suffix = ExtRe.concat prefix_pump attack_family.suffix in
    prefix_pump_suffix

  let is_empty attack =
    List.exists ExtRe.lang_empty [ attack.prefix; attack.pump; attack.suffix ]

  let map f a = { prefix = f a.prefix; pump = f a.pump; suffix = f a.suffix }
  let simplify = map ExtRe.simplify
end

(** Attack string from a AttackFamily. *)
module ExploitString = struct
  type t = { prefix : string; pump : string; suffix : string }

  let from_attack_family family predicate =
    let open Utility.OptionSyntax in
    if AttackFamily.is_empty family then Option.None
    else
      let* prefix = ExtRe.get_word family.prefix predicate in
      let* pump = ExtRe.get_word family.pump predicate in
      let* suffix = ExtRe.get_word family.suffix predicate in
      Some { prefix; pump; suffix }

  let generate max_len e =
    let res = ref e.prefix in
    let suffix_len = String.length e.pump + String.length e.suffix in
    while String.length !res <= max_len - suffix_len do
      res := !res ^ e.pump
    done;
    res := !res ^ e.suffix;
    !res

  let to_string exploit =
    String.concat ""
      [
        "{\n  prefix = '";
        exploit.prefix;
        "'\n  pump = '";
        exploit.pump;
        "'\n  suffix = '";
        exploit.suffix;
        "'\n}";
      ]
end

(*** Result of the analysis. *)
module AttackFamilySet = struct
  include Set.Make (AttackFamily)

  let to_string attack_family_set =
    if is_empty attack_family_set then "∅"
    else
      let rstring =
        elements attack_family_set
        |> List.map AttackFamily.to_string
        |> String.concat "\n  "
      in
      "{\n  " ^ rstring ^ "\n}"

  let to_lang attack_family_set =
    fold
      (fun elem acc -> ExtRe.alternative (AttackFamily.to_lang elem) acc)
      attack_family_set ExtRe.empty

  let remove_empty = filter (fun attack -> not (AttackFamily.is_empty attack))

  (** Returns an exploit string from the attack family such that each character
      in the exploit string respect the predicate.*)
  let get_exploit_string attack_family_set predicate : ExploitString.t option =
    let not_empty_attack_family_set = remove_empty attack_family_set in
    match choose_opt not_empty_attack_family_set with
    | None -> None
    | Some attack_family ->
        ExploitString.from_attack_family attack_family predicate

  let simplify = map AttackFamily.simplify
end

let rec to_ext_regex = function
  | Epsilon -> ExtRe.eps
  | Char c -> ExtRe.chr c
  | Alternative (l, r) -> ExtRe.alternative (to_ext_regex l) (to_ext_regex r)
  | Concat (l, r) -> ExtRe.concat (to_ext_regex l) (to_ext_regex r)
  | Star (_, e) -> ExtRe.star (to_ext_regex e)

let rec leaves e =
  match next e with
  | None -> RS.empty
  | Match _ -> RS.singleton e
  | LeftOrRight (l, r) -> RS.union (leaves l) (leaves r)
  | ExpandOrNot (l, r) -> RS.union (leaves l) (leaves r)

let common_words_in_leaves left_leaves right_leaves =
  let cartesian =
    U.cartesian (RS.elements left_leaves) (RS.elements right_leaves)
    |> List.map (fun el -> (to_ext_regex (fst el), to_ext_regex (snd el)))
  in
  List.fold_left
    (fun acc elem ->
      let left, right = elem in
      let lr_intersection = ExtRe.inter left right in
      if ExtRe.lang_empty lr_intersection then acc
      else ExtRe.alternative acc lr_intersection)
    ExtRe.empty cartesian

(** [m2 e] returns the language of words that can possibly be matched in at
    least two ways in the expression e *)
let rec m2 e = m2_rec e RS.empty ExtRe.eps

and m2_rec e explored pref =
  if compare e eps = 0 then ExtRe.empty else m2_not_eps e explored pref

and m2_not_eps e explored pref =
  if RS.mem e explored then m2_already_explored e explored pref
  else m2_new_expression e explored pref

and m2_already_explored e explored pref =
  (* If we have already explored the expression e we can prune it. This will
     occur only for kleene stars to avoid infinite loops. *)
  let tail = tail e in
  let head = head e in
  let pref = ExtRe.concat pref (to_ext_regex head) in
  m2_rec tail explored pref

and m2_new_expression e explored pref =
  match next e with
  | Match (a, e') ->
      let new_pref = ExtRe.concat pref (ExtRe.chr a) in
      m2_rec e' explored new_pref
  | LeftOrRight (l, r) -> m2_choice l r explored pref
  | ExpandOrNot (l, r) -> m2_choice l r (RS.add e explored) pref
  | None -> ExtRe.empty

and m2_choice l r explored pref =
  let attack_for_choice = common_words_in_leaves (leaves l) (leaves r) in
  let attack_for_choice_with_pref = ExtRe.concat pref attack_for_choice in
  let attack_left = m2_rec l explored pref in
  let attack_right = m2_rec r explored pref in
  ExtRe.alternative attack_for_choice_with_pref
    (ExtRe.alternative attack_left attack_right)

(** [exp_attack_families r] returns the families of exponentially attack words
    for the regex [r]. *)
let rec exp_attack_families r =
  exp_attack_rec ExtRe.eps ExtRe.eps r |> AttackFamilySet.remove_empty

and exp_attack_rec pref suff e =
  match e with
  | Epsilon -> AttackFamilySet.empty
  | Char _ -> AttackFamilySet.empty
  | Alternative (l, r) -> exp_attack_rec_alternative pref suff l r
  | Concat (l, r) -> exp_attack_rec_concat pref suff l r
  | Star (_, e') -> exp_attack_rec_star pref suff e e'

and exp_attack_rec_alternative pref suff l r =
  AttackFamilySet.union
    (exp_attack_rec pref suff l)
    (exp_attack_rec pref suff r)

and exp_attack_rec_concat pref suff l r =
  AttackFamilySet.union
    (exp_attack_rec pref (ExtRe.concat (to_ext_regex r) suff) l)
    (exp_attack_rec (ExtRe.concat pref (to_ext_regex l)) suff r)

and exp_attack_rec_star pref suff e e' =
  let pref = ExtRe.concat pref (to_ext_regex e) in
  let suff = ExtRe.concat (to_ext_regex e) suff in
  let negated_suff = ExtRe.compl suff in
  let pump = m2 e in
  let attack_family =
    AttackFamilySet.singleton { prefix = pref; pump; suffix = negated_suff }
  in
  let attack_e' = exp_attack_rec pref suff e' in
  AttackFamilySet.union attack_family attack_e'

let analyze r = exp_attack_families r
