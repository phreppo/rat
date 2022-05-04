open Lexing

module State = struct
  type t = int

  let count = ref 0

  let fresh () =
    let rep = !count in
    incr count;
    rep

  let print = Format.pp_print_int
  let compare = compare
  let restart () = count := 0
end

module CharAlgebra = struct
  type t = char

  let compare = compare

  (* let print fmt c = Format.pp_print_string fmt (Char.escaped c) *)
  let print fmt c = Format.pp_print_string fmt (Char.escaped c)
end

module CharAutomaton = struct
  include AbstractRegularAutomaton.Make (State) (CharAlgebra)

  let string_of_position p =
    Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
end
