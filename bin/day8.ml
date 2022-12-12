open Core
open Aoc2022.Coords
open Aoc2022.Fields

let parse_ch (ch : char) : int = Char.to_int ch - Char.to_int '0'

let visible_from_outside_towards (start : coord) (offset : coord)
    (field : int_field) : coord_set =
  let result = ref CoordSet.empty in
  let current = ref start in
  let max_height = ref (-1) in
  while Field.contains field !current do
    let curr_height = Field.get !current field in
    if curr_height > !max_height then (
      max_height := curr_height;
      result := !current |> CoordSet.add !result);
    Coord.(current := !current + offset)
  done;
  !result

let visible_from_tree_towards (start : coord) (offset : coord)
    (field : int_field) : coord_set =
  let result = ref CoordSet.empty in
  let current = ref Coord.(start + offset) in
  let max_height = ref (Field.get start field) in
  let break = ref false in
  while Field.contains field !current && not !break do
    let curr_height = Field.get !current field in
    if curr_height >= !max_height then (
      max_height := curr_height;
      result := !current |> CoordSet.add !result;
      break := true)
    else result := !current |> CoordSet.add !result;

    Coord.(current := !current + offset)
  done;
  !result

let visible_from_tree (start : coord) (field : int_field) : coord_set =
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

let visible_from_tree_score (start : coord) (field : int_field) : int =
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

let visible_from_outside (field : int_field) : coord_set =
  let result = ref CoordSet.empty in
  (* from top row down *)
  for x = 0 to field.width - 1 do
    result :=
      visible_from_outside_towards (x, 0) (0, 1) field |> CoordSet.union !result
  done;
  (* from bottom row up *)
  for x = 0 to field.width - 1 do
    result :=
      visible_from_outside_towards (x, field.height - 1) (0, -1) field
      |> CoordSet.union !result
  done;
  (* from left column right *)
  for y = 0 to field.height - 1 do
    result :=
      visible_from_outside_towards (0, y) (1, 0) field |> CoordSet.union !result
  done;
  (* from right column left *)
  for y = 0 to field.height - 1 do
    result :=
      visible_from_outside_towards (field.width - 1, y) (-1, 0) field
      |> CoordSet.union !result
  done;

  !result

let print_field = Field.to_str ~f:(fun v -> String.get (string_of_int v) 0)

let to_visibility_field (width, height) (coords : coord_set) : int_field =
  let result = Field.create width height 0 in
  coords |> CoordSet.iter ~f:(fun (x, y) -> Field.set (x, y) 1 result);
  result

let best_visible_from_score (field : int_field) ~should_print_field =
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
    visible_from_tree !best_pos field
    |> to_visibility_field (field.width, field.height)
    |> print_field |> Printf.printf "%s";
  !best

let field =
  In_channel.read_all "inputs/day8.txt" |> Field.parse ~f:parse_ch ~default:0

let visib = field |> visible_from_outside
let () = visib |> CoordSet.length |> Printf.printf "Part1: %d\n"

let () =
  best_visible_from_score field ~should_print_field:false
  |> Printf.printf "Part2: %d\n"
