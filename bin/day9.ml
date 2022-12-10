open Core

module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let ( - ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
end

type coord = Coord.t

module CoordSet = Core.Set.Make (Coord)

type snake = { head : coord; tail : coord }

let sgn i = match i with pos when pos > 0 -> 1 | 0 -> 0 | _ -> -1

let tail_offset head tail =
  let dx, dy = Coord.(head - tail) in
  if Int.abs dx <= 1 && Int.abs dy <= 1 then (0, 0) else (sgn dx, sgn dy)

let move { head; tail } offset =
  let new_head = Coord.(head + offset) in
  let toffset = tail_offset new_head tail in
  let new_tail = Coord.(tail + toffset) in
  { head = new_head; tail = new_tail }

type step = coord * int

let rec eval_step (snake, coords) (offset, count) =
  if count = 0 then (snake, coords)
  else
    let new_snake = move snake offset in
    eval_step
      (new_snake, new_snake.tail |> CoordSet.add coords)
      (offset, count - 1)

let eval_steps snake coords (steps : step list) =
  steps |> List.fold ~init:(snake, coords) ~f:eval_step

let parse_direction str =
  match str with
  | "L" -> (-1, 0)
  | "R" -> (1, 0)
  | "U" -> (0, -1)
  | "D" -> (0, 1)
  | _ -> assert false

let parse_step (str : string) : step =
  let direction, count = Scanf.sscanf str "%s %d" (fun s d -> (s, d)) in
  (parse_direction direction, count)

let input =
  In_channel.read_all "inputs/day9.txt"
  |> String.strip |> String.split_lines |> List.map ~f:parse_step

let _, cs = eval_steps { head = (0, 0); tail = (0, 0) } CoordSet.empty input;;

cs |> CoordSet.length |> Printf.printf "Part1: %d\n"

type long_snake = coord list

let rec move2 (s : long_snake) offset =
  match s with
  | [] -> []
  | [ h ] -> [ Coord.(h + offset) ]
  | h :: t :: rest ->
      let new_head = Coord.(h + offset) in
      let toffset = tail_offset new_head t in
      new_head :: move2 (t :: rest) toffset

let rec tail l =
  match l with [ e ] -> e | _ :: rest -> tail rest | [] -> assert false

let rec eval_step2 (snake, coords) (offset, count) =
  if count = 0 then (snake, coords)
  else
    let new_snake = move2 snake offset in
    eval_step2
      (new_snake, tail new_snake |> CoordSet.add coords)
      (offset, count - 1)

let eval_steps2 snake coords (steps : step list) =
  steps |> List.fold ~init:(snake, coords) ~f:eval_step2

let long_snake = Array.create ~len:10 (0, 0) |> Array.to_list
let _, cs2 = eval_steps2 long_snake CoordSet.empty input;;

cs2 |> CoordSet.length |> Printf.printf "Part 2: %d\n"
