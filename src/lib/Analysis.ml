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

(** [remove_eps r] returns a regular expressions [r'] that accepts the same
    language as [r] without [Epsilon]. *)
let rec remove_eps r =
  match (head r, tail r) with
  | Epsilon, _ | Star (false, _), _ -> ExtRe.empty
  | (Char _ as a), r1 ->
      ExtRe.concat (to_ext_regex a) (refesh_stars r1 |> to_ext_regex)
  | Alternative (r1, r2), r3 ->
      ExtRe.alternative
        (remove_eps (Concat (r1, r3)))
        (remove_eps (Concat (r2, r3)))
  | Star (true, r1), r2 ->
      ExtRe.alternative
        (remove_eps (Concat (r1, Concat (Star (false, r1), r2))))
        (remove_eps r2)
  | Concat _, _ -> failwith "unreachable"

let non_eps_iter r1 r2 = ExtRe.inter (remove_eps r1) (remove_eps r2)

(** [m2 r] returns the language of words that can possibly be matched in at
    least two traces in the expression [r]. *)
let rec m2 r = m2_rec r RS.empty

and m2_rec r explored =
  if RS.mem r explored then ExtRe.empty
  else
    match (head r, tail r) with
    | Epsilon, _ | Star (false, _), _ -> ExtRe.empty
    | (Char _ as a), r1 ->
        ExtRe.concat (to_ext_regex a) (m2_rec (Re.refesh_stars r1) explored)
    | Alternative (r1, r2), r3 ->
        let inter = non_eps_iter (Concat (r1, r3)) (Concat (r2, r3)) in
        let left = m2_rec (Concat (r1, r3)) explored in
        let right = m2_rec (Concat (r2, r3)) explored in
        ExtRe.alternative inter (ExtRe.alternative left right)
    | (Star (true, r1) as r1_star), r2 ->
        let expanded = Concat (r1, Concat (Star (false, r1), r2)) in
        let inter = non_eps_iter expanded r2 in
        let left = m2_rec expanded (RS.add r explored) in
        let right = ExtRe.concat (to_ext_regex r1_star) (m2_rec r2 explored) in
        ExtRe.alternative inter (ExtRe.alternative left right)
    | Concat _, _ -> failwith "unreachable"

(** [exp_attack_families r] returns the families of exponentially attack words
    for the regex [r]. *)
let rec exp_attack_families r =
  exp_attack_rec ExtRe.eps ExtRe.eps r |> AttackFamilySet.remove_empty

and exp_attack_rec pref suff r =
  match r with
  | Epsilon -> AttackFamilySet.empty
  | Char _ -> AttackFamilySet.empty
  | Alternative (r1, r2) -> exp_attack_rec_alternative pref suff r1 r2
  | Concat (r1, r2) -> exp_attack_rec_concat pref suff r1 r2
  | Star (_, r1) -> exp_attack_rec_star pref suff r r1

and exp_attack_rec_alternative pref suff r1 r2 =
  AttackFamilySet.union
    (exp_attack_rec pref suff r1)
    (exp_attack_rec pref suff r2)

and exp_attack_rec_concat pref suff r1 r2 =
  AttackFamilySet.union
    (exp_attack_rec pref (ExtRe.concat (to_ext_regex r2) suff) r1)
    (exp_attack_rec (ExtRe.concat pref (to_ext_regex r1)) suff r2)

and exp_attack_rec_star pref suff r r1 =
  let pref = ExtRe.concat pref (to_ext_regex r) in
  let suff = ExtRe.concat (to_ext_regex r) suff in
  let negated_suff = ExtRe.compl suff in
  let pump = m2 r in
  let attack_family =
    AttackFamilySet.singleton { prefix = pref; pump; suffix = negated_suff }
  in
  let attack_e' = exp_attack_rec pref suff r1 in
  AttackFamilySet.union attack_family attack_e'

let analyze r = exp_attack_families r

type redos_result = Safe | Dangerous | ParseError

let has_redos regex =
  match ParseRe.parse regex with
  | Error _ -> ParseError
  | Ok regex -> (
      match ParserRe.to_re ~semantics:Match regex with
      | Error _ -> ParseError
      | Ok regex ->
          let attack = analyze regex in
          if AttackFamilySet.is_empty attack then Safe else Dangerous)
