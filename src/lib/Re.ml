module U = Utility

type t =
  | Epsilon
  | Char of Charset.t
  | Concat of t * t
  | Choice of t * t
  | Star of bool * t

type transition =
  | None
  | Match of Charset.t * t
  | LeftOrRight of t * t
  | ExpandOrNot of t * t

let rec is_finite = function
  | Epsilon -> true
  | Char _ -> true
  | Concat (re1, re2) -> is_finite re1 && is_finite re2
  | Choice (re1, re2) -> is_finite re1 && is_finite re2
  | Star (_, Epsilon) -> true (* (eps)* is finite. *)
  | Star _ -> false

let eps = Epsilon
let ch c = Char c
let choice e1 e2 = Choice (e1, e2)

let star ?(expandible = true) e1 =
  match e1 with Epsilon -> Epsilon | _ -> Star (expandible, e1)

let concat e1 e2 =
  match (e1, e2) with
  | Epsilon, _ -> e2
  | _, Epsilon -> e1
  | _ -> Concat (e1, e2)

let any_char = Char Charset.all_symbols

let rec compare e1 e2 =
  match (e1, e2) with
  | Epsilon, Epsilon -> 0
  | Epsilon, _ -> -1
  | _, Epsilon -> 1
  | Char c1, Char c2 -> Charset.compare c1 c2
  | Char _, _ -> -1
  | _, Char _ -> 1
  | Concat (e1, e2), Concat (e3, e4) ->
    let comp_e1_e3 = compare e1 e3 in
    if comp_e1_e3 <> 0 then comp_e1_e3 else compare e2 e4
  | Concat _, _ -> -1
  | _, Concat _ -> 1
  | Choice (e1, e2), Choice (e3, e4) ->
    let comp_e1_e3 = compare e1 e3 in
    if comp_e1_e3 <> 0 then comp_e1_e3 else compare e2 e4
  | Choice _, _ -> -1
  | _, Choice _ -> 1
  | Star (b1, e1), Star (b2, e2) ->
    if Bool.compare b1 b2 <> 0 then Bool.compare b1 b2 else compare e1 e2

let rec to_string = function
  | Epsilon -> U.yellow ^ "ε" ^ U.reset
  | Char c -> Charset.to_string c
  | Concat (a, b) -> to_string a ^ to_string b
  | Choice (a, b) ->
    U.blue ^ "(" ^ U.reset ^ to_string a ^ U.blue ^ ")|(" ^ U.reset
    ^ to_string b ^ U.blue ^ ")" ^ U.reset
  | Star (true, a) ->
    U.green ^ "[" ^ U.reset ^ to_string a ^ U.green ^ "]*" ^ U.reset
  | Star (false, a) ->
    U.red ^ "~[" ^ U.reset ^ to_string a ^ U.red ^ "]*" ^ U.reset

let rec refesh_stars = function
  | Epsilon -> eps
  | Char c -> ch c
  | Concat (e1, e2) -> concat (refesh_stars e1) (refesh_stars e2)
  | Choice (e1, e2) -> choice (refesh_stars e1) (refesh_stars e2)
  | Star (_, e1) -> star (refesh_stars e1)

let rec next = function
  | Epsilon -> None
  | Char c -> Match (c, eps)
  | Choice (l, r) -> LeftOrRight (l, r)
  | Star (false, _) -> None
  | Star (true, e') -> ExpandOrNot (concat e' (star ~expandible:false e'), eps)
  | Concat (l, r) -> (
      match next l with
      | None -> None
      (* we matched => we can refresh the expandable stars *)
      | Match (c, e1') -> Match (c, concat e1' r |> refesh_stars)
      | LeftOrRight (l', r') -> LeftOrRight (concat l' r, concat r' r)
      | ExpandOrNot (e1', e1'') -> ExpandOrNot (concat e1' r, concat e1'' r))

let rec head e = match e with Concat (l, _) -> head l | _ -> e
let rec tail e = match e with Concat (l, r) -> concat (tail l) r | _ -> eps

let rec case_insensitive = function
  | Epsilon -> Epsilon
  | Concat (r1, r2) -> concat (case_insensitive r1) (case_insensitive r2)
  | Choice (r1, r2) -> choice (case_insensitive r1) (case_insensitive r2)
  | Star (b, r) -> star ~expandible:b (case_insensitive r)
  | Char cs -> Char (Charset.case_insensitive cs)

module ReSet = struct
  include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)

  let to_string rset =
    let rstring = elements rset |> List.map to_string |> String.concat ", " in
    "{" ^ rstring ^ "}"
end
