open Core

type move = { src : int; dst : int; count : int }
type stack = char Stack.t
type cargo = stack array

let parse_move str : move =
  Scanf.sscanf str "move %d from %d to %d" (fun count src dst ->
      { src; dst; count })

let parse_moves str =
  str |> String.strip |> String.split ~on:'\n'
  |> List.filter ~f:(String.is_prefix ~prefix:"move")
  |> List.map ~f:parse_move

let print_cargo (cargo : cargo) : unit =
  let arrays = cargo |> Array.map ~f:Stack.to_array in
  let max =
    arrays |> Array.map ~f:Array.length
    |> Array.max_elt ~compare:Int.ascending
    |> Option.value_exn
  in
  let stack_count = arrays |> Array.length in
  for y = 0 to max - 1 do
    for x = 0 to stack_count - 1 do
      let stack = arrays.(x) in
      let stack_len = Array.length stack in
      let offset = max - stack_len in
      if y >= offset then Printf.printf "[%c] " stack.(y - offset)
      else Printf.printf "    "
    done;
    Printf.printf "\n"
  done

let make_move cargo { src; dst; count } =
  let src_stack = cargo.(src - 1) in
  let dst_stack = cargo.(dst - 1) in
  for _ = 1 to count do
    src_stack |> Stack.pop_exn |> Stack.push dst_stack
  done

let make_super_move cargo { src; dst; count } =
  let tmp_stack = Stack.create () in
  let src_stack = cargo.(src - 1) in
  let dst_stack = cargo.(dst - 1) in
  for _ = 1 to count do
    src_stack |> Stack.pop_exn |> Stack.push tmp_stack
  done;
  for _ = 1 to count do
    tmp_stack |> Stack.pop_exn |> Stack.push dst_stack
  done

let print_move { src; dst; count } =
  Printf.printf "move %d from %d to %d\n" count src dst

let create_stack str = str |> String.to_list |> Stack.of_list
let create_cargo strs = strs |> List.map ~f:create_stack |> Array.of_list

let create_start () =
  create_cargo
    [
      "FHMTVLD";
      "PNTCJGQH";
      "HPMDSR";
      "FVBL";
      "QLGHN";
      "PMRGDBW";
      "QLHCRNMG";
      "WLC";
      "TMZJQLDR";
    ]

let my_move = parse_move "move 2 from 1 to 3"
let () = print_move my_move
let my_cargo = create_cargo [ "abc"; "def"; "" ]
let () = print_cargo my_cargo
let () = make_move my_cargo my_move
let () = Printf.printf "After move\n"
let () = print_cargo my_cargo
let moves = In_channel.read_all "inputs/day5.txt" |> parse_moves
let part1 = create_start ()
let () = moves |> List.iter ~f:print_move
let () = Printf.printf "\nInitial state:\n"
let () = print_cargo part1
let () = moves |> List.iter ~f:(make_move part1)
let () = Printf.printf "\nFinal state:\n"
let () = print_cargo part1
let part2 = create_start ()
let () = moves |> List.iter ~f:(make_super_move part2)
let () = Printf.printf "\nFinal state Part 2:\n"
let () = print_cargo part2
