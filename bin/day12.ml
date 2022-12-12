open Printf
open Aoc2022.Fields
open Aoc2022.Coords

let start_ch = 1234
let end_ch = 4321

let parse_ch (ch : char) : int =
  match ch with
  | 'S' -> start_ch
  | 'E' -> end_ch
  | _ -> Core.Char.to_int ch - Core.Char.to_int 'a'

let height i : int = if i = start_ch then 0 else if i = end_ch then 25 else i

let print_ch (value : int) : char =
  match value with
  | s when s = start_ch -> 'S'
  | e when e = end_ch -> 'E'
  | _ -> Core.Char.of_int_exn @@ (value + Core.Char.to_int 'a')

type mymap = { field : int_field; start : coord; goal : coord }

let create_mymap f : mymap =
  {
    field = f;
    start = f |> Field.single ~f:(fun v -> v = start_ch);
    goal = f |> Field.single ~f:(fun v -> v = end_ch);
  }

let bfs (m : mymap) ~start ~stop =
  let to_do = Queue.create () in
  let visited = FieldSet.for_field m.field in
  to_do |> Queue.push (start, 0);
  let exception Found of int in
  try
    while not (Queue.is_empty to_do) do
      match Queue.pop to_do with
      | position, distance when stop position -> raise (Found distance)
      | position, distance ->
          let curr_val = Field.get position m.field in
          Field.neighbors4 m.field position
          |> List.filter (fun n ->
                 (not (FieldSet.contains n visited))
                 && not (height (Field.get n m.field) > curr_val + 1))
          |> List.iter (fun n ->
                 FieldSet.set n visited;
                 to_do |> Queue.push (n, distance + 1))
    done;
    assert false
  with Found i -> i

(* if !found then !found_elem else failwith "Cannot find goal" *)

let sample =
  Core.In_channel.read_all "inputs/day12_sample.txt"
  |> Field.parse ~f:parse_ch ~default:0
  |> create_mymap
;;

sample.field |> Field.to_str ~f:print_ch |> printf "\n%s";;

let sx, sy = sample.start in
printf "start: %d, %d\n" sx sy
;;

let gx, gy = sample.goal in
printf "goal: %d, %d\n" gx gy
;;

(0, 0)
|> Field.neighbors4 sample.field
|> List.iter (fun (x, y) -> printf "neighbor %d,%d\n" x y)

(* let draw_path (m : mymap) (path_list : coord list) = *)
(*   let result = Field.create m.field.width m.field.height 0 in *)
(*   let path = Array.of_list path_list in *)
(*   let prev = ref m.goal in *)
(*   for i = 0 to Array.length path - 1 do *)
(*     let cur = path.(i) in *)
(*     let dx, dy = Coord.(!prev - cur) in *)
(*     let dir_val = *)
(*       match (dx, dy) with *)
(*       | 0, -1 -> 1 *)
(*       | 1, 0 -> 2 *)
(*       | 0, 1 -> 3 *)
(*       | -1, 0 -> 4 *)
(*       | _ -> failwith "bad diff" *)
(*     in *)
(*     Field.set cur dir_val result; *)
(*     prev := cur *)
(*   done; *)
(*   result *)

let cost =
  bfs sample ~start:sample.start ~stop:(fun x -> Coord.(x = sample.goal))
;;

printf "Sample: %d\n" cost

let input =
  Core.In_channel.read_all "inputs/day12.txt"
  |> Field.parse ~f:parse_ch ~default:0
  |> create_mymap
;;

printf "read input, width: %d, height: %d\n" input.field.width
  input.field.height

let part1 = bfs input ~start:input.start ~stop:(fun x -> Coord.(x = input.goal));;

printf "Part1: %d\n" part1
