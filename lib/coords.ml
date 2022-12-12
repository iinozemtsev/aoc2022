open Core

module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  let ( = ) a b = compare a b = 0
  let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let ( - ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
end

module CoordSet = Core.Set.Make (Coord)

type coord = Coord.t
type coord_set = CoordSet.t
