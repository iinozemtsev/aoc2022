open Core

type field = { cells : int array; width : int; height : int }

module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
end

type coord = Coord.t

module CoordSet = Core.Set.Make (Coord)

type coord_set = CoordSet.t

let set_val ((x, y) : coord) v field : unit =
  field.cells.((y * field.width) + x) <- v

let get_val (x, y) field = field.cells.((y * field.width) + x)
let parse_ch (ch : char) : int = Char.to_int ch - Char.to_int '0'

let in_bounds field (x, y) =
  x >= 0 && y >= 0 && x < field.width && y < field.height

let visible_from_outside_towards (start : coord) (offset : coord) (field : field) : coord_set =
  let result = ref CoordSet.empty in
  let current = ref start in
  let max_height = ref (-1) in
  while in_bounds field !current do
    let curr_height = get_val !current field in
    if curr_height > !max_height then (
      max_height := curr_height;
      result := !current |> CoordSet.add !result);
    Coord.(current := !current + offset)
  done;
  !result

let visible_from_tree_towards (start : coord) (offset : coord) (field : field) :
    coord_set =
  let result = ref CoordSet.empty in
  let current = ref Coord.(start + offset) in
  let max_height = ref (get_val start field) in
  let break = ref false in
  while in_bounds field !current && not !break do
    let curr_height = get_val !current field in
    if curr_height >= !max_height then (
      max_height := curr_height;
      result := !current |> CoordSet.add !result;
      break := true)
    else result := !current |> CoordSet.add !result;

    Coord.(current := !current + offset)
  done;
  !result

let visible_from_tree (start : coord) (field : field) : coord_set =
  let result = ref CoordSet.empty in
  (* going up*)
  result :=
    visible_from_tree_towards start (0, -1) field |> CoordSet.union !result;
  (* going down*)
  result :=
    visible_from_tree_towards start (0, 1) field |> CoordSet.union !result;
  (* going left*)
  result :=
    visible_from_tree_towards start (-1, 0) field |> CoordSet.union !result;
  (* going right*)
  result :=
    visible_from_tree_towards start (1, 0) field |> CoordSet.union !result;
  !result

let visible_from_tree_score (start : coord) (field : field) : int =
  let result = ref 1 in
  (* going up*)
  result :=
    (visible_from_tree_towards start (0, -1) field |> CoordSet.length) * !result;
  (* going down*)
  result :=
    (visible_from_tree_towards start (0, 1) field |> CoordSet.length) * !result;
  (* going left*)
  result :=
    (visible_from_tree_towards start (-1, 0) field |> CoordSet.length) * !result;
  (* going right*)
  result :=
    (visible_from_tree_towards start (1, 0) field |> CoordSet.length) * !result;
  !result

let visible_from_outside (field : field) : coord_set =
  let result = ref CoordSet.empty in
  (* from top row down *)
  for x = 0 to field.width - 1 do
    result := visible_from_outside_towards (x, 0) (0, 1) field |> CoordSet.union !result
  done;
  (* from bottom row up *)
  for x = 0 to field.width - 1 do
    result :=
      visible_from_outside_towards (x, field.height - 1) (0, -1) field
      |> CoordSet.union !result
  done;
  (* from left column right *)
  for y = 0 to field.height - 1 do
    result := visible_from_outside_towards (0, y) (1, 0) field |> CoordSet.union !result
  done;
  (* from right column left *)
  for y = 0 to field.height - 1 do
    result :=
      visible_from_outside_towards (field.width - 1, y) (-1, 0) field |> CoordSet.union !result
  done;

  !result

let make_field (width, height) =
  { cells = Array.create ~len:(width * height) 0; width; height }

let parse_field (input : string) : field =
  let lines = input |> String.strip |> String.split_lines in
  let width = List.hd_exn lines |> String.length in
  let height = List.length lines in
  let result : field = make_field (width, height) in
  lines
  |> List.iteri ~f:(fun y line ->
         line
         |> String.iteri ~f:(fun x ch -> result |> set_val (x, y) (parse_ch ch)));
  result

let print_field (field : field) : string =
  Printf.collect_to_string (fun { printf } ->
      for y = 0 to field.height - 1 do
        for x = 0 to field.width - 1 do
          field |> get_val (x, y) |> printf "%d"
        done;
        printf "\n"
      done)

let to_visibility_field (width, height) (coords : coord_set) : field =
  let result = make_field (width, height) in
  coords |> CoordSet.iter ~f:(fun (x, y) -> set_val (x, y) 1 result);
  result

let best_visible_from_score (field : field) ~should_print_field =
  let best_pos = ref (0, 0) in
  let best = ref 0 in
  for x = 1 to field.width - 2 do
    for y = 1 to field.height - 2 do
      let score = visible_from_tree_score (x, y) field in
      if score > !best then (
        best := score;
        best_pos := (x, y))
    done
  done;
  if should_print_field then
(  visible_from_tree !best_pos field
  |> to_visibility_field (field.width, field.height)
  |> print_field |> Printf.printf "%s");
  !best

let field = In_channel.read_all "inputs/day8.txt" |> parse_field

let visib = field |> visible_from_outside



let () = visib |> CoordSet.length |> Printf.printf "Part1: %d\n"

let () =
  best_visible_from_score field ~should_print_field:false
  |> Printf.printf "Part2: %d\n"
