(** ToolBox *)

(** {1 Printing} *)

let format_to_string prt x =
  let b = Buffer.create 12 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f "%a%!" prt x in
  Buffer.contents b

(* Debug function *)
let debug s fmt =
  Format.kasprintf
    (fun str -> Format.printf "\027[1;3m[Debug %s]\027[0m @.@[%s@]@.@." s str)
    fmt

(* Set printing*)
let print_set printer f fmt x =
  Format.fprintf fmt "@[<v>@[<v 2>{@,%a@]@,}@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       printer)
    (f x)

let print_set_inline printer f fmt x =
  Format.fprintf fmt "@[{%a}@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       printer)
    (f x)

(* Map printing*)
let print_map printer1 printer2 f fmt x =
  let l = f x in
  if List.length l > 1 then
    Format.fprintf fmt "@[<v>@[<v 2>{@,%a@]@,}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (z, t) -> Format.fprintf fmt "%a ~> %a" printer1 z printer2 t))
      l
  else
    Format.fprintf fmt "@[{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt (z, t) -> Format.fprintf fmt "%a ~> %a" printer1 z printer2 t))
      l

let print_map_inline printer1 printer2 f fmt x =
  let l = f x in
  Format.fprintf fmt "@[{%a}@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt (z, t) -> Format.fprintf fmt "%a ~> %a" printer1 z printer2 t))
    l

(* List printing*)
let print_list_inline_no_border printer fmt x =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt z -> Format.fprintf fmt "%a" printer z))
    x

let print_list printer fmt x =
  Format.fprintf fmt "@[<v>@[<v 2>[@,%a@]@,]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       (fun fmt z -> Format.fprintf fmt "%a" printer z))
    x

let print_list_inline printer fmt x =
  Format.fprintf fmt "@[{%a}@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt z -> Format.fprintf fmt "%a" printer z))
    x

(* Pair printing *)
let print_pair printer1 printer2 fmt (x, y) =
  Format.fprintf fmt "(%a,%a)" printer1 x printer2 y

(* Parenthesis printing *)

let print_paren b printer fmt x =
  if b then Format.fprintf fmt "(%a)" printer x else printer fmt x

(** {1 Comparing} *)

let list_compare p l l' =
  let rec aux a b =
    match (a, b) with
    | t :: r, t' :: r' ->
        let x = p t t' in
        if x = 0 then aux r r' else x
    | [], t' :: r' -> -1
    | [], [] -> 0
    | t :: r, [] -> 1
  in
  aux l l'

let pair_compare p q (a, b) (c, d) =
  let x = p a c in
  if x = 0 then q b d else x

let triplet_compare p q r (a, b, c) (d, e, f) =
  let x = p a d in
  if x = 0 then
    let y = q b e in
    if y = 0 then r c f else y
  else x

(** {1 List} *)

let rec rm_doub cmp l =
  match l with p :: q -> p :: rm_doub cmp (filt cmp p q) | [] -> []

and filt cmp e l =
  match l with
  | p :: q -> if cmp e p = 0 then filt cmp e q else p :: filt cmp e q
  | [] -> []

let mem_compare cmp e l = List.exists (fun x -> cmp x e = 0) l

let fusion_mem cmp l l' =
  List.fold_left
    (fun acc x -> if mem_compare cmp x acc then acc else x :: acc)
    l' l

let list_eq cmp l l' =
  let list_leq l l' = List.for_all (fun x -> mem_compare cmp x l') l in
  list_leq l l' && list_leq l' l

let rec assoc_find cmp x l =
  match l with
  | (x', v) :: q ->
      let a = cmp x x' in
      if a > 0 then assoc_find cmp x q else if a = 0 then Some v else None
  | _ -> None

let rec assoc_find_exc cmp x l =
  match l with
  | (x', v) :: q ->
      let a = cmp x x' in
      if a > 0 then assoc_find_exc cmp x q
      else if a = 0 then v
      else raise Not_found
  | _ -> raise Not_found

let assoc_set cmp x f l =
  let rec aux l acc =
    match l with
    | (x', v) :: q ->
        let a = cmp x x' in
        if a > 0 then aux q ((x', v) :: acc)
        else if a = 0 then List.rev_append acc ((x, f (Some v)) :: q)
        else List.rev_append acc ((x, f None) :: l)
    | _ -> List.rev_append acc [ (x, f None) ]
  in
  aux l []

let set_assoc k v l =
  let rec aux l acc =
    match l with
    | (k', v') :: q when k = k' -> List.rev_append acc ((k', v) :: q)
    | (k', v') :: q -> aux q ((k', v') :: acc)
    | [] -> List.rev_append acc [ (k, v) ]
  in
  aux l []

let find_and_remove f l =
  let rec aux l acc =
    match l with
    | p :: q when f p -> (List.rev_append acc q, p)
    | p :: q -> aux q (p :: acc)
    | [] -> raise Not_found
  in
  aux l []

(** {1 Renaming} *)

module StringS = Set.Make (String)
module StringM = Map.Make (String)

module IntM = Map.Make (struct
  type t = int

  let compare = compare
end)

let stringm_replace k f s =
  try
    let a = StringM.find k s in
    StringM.remove k s |> StringM.add k (f (Some a))
  with Not_found -> StringM.add k (f None) s

let intm_replace k f s =
  try
    let a = IntM.find k s in
    IntM.remove k s |> IntM.add k (f (Some a))
  with Not_found -> IntM.add k (f None) s

type renaming = string StringM.t

(** {1 Misc}*)

(** [fold_int f n acc] fold on integers *)
let fold_int f i acc =
  let rec aux j acc = if j = i then acc else aux (j + 1) (f j acc) in
  aux 0 acc

let map_if_not_none f x = match x with None -> None | Some x -> Some (f x)
let fold f l x = List.fold_left (fun acc x -> f x acc) x l
