open Core
open Option
open Printf
open Scanf

type instruction = Noop | Addx of int

let update_x instruction x =
  match instruction with Noop -> x | Addx dx -> x + dx

let cycle_count instruction = match instruction with Noop -> 1 | Addx _ -> 2

(* cycle: current cycle index
   x: register value
   curr_remaining_cycles: how many more cycles past the current, the
   current instructions should execute. Current instruction is a head
   of a program if program is not empty
*)
type computer = {
  cycle : int;
  x : int;
  curr_remaining_cycles : int;
  program : instruction list;
}

let start program : computer =
  {
    cycle = 1;
    x = 1;
    curr_remaining_cycles =
      (program |> List.hd >>| cycle_count |> Option.value ~default:1) - 1;
    program;
  }

let next_cycle computer : computer =
  let new_cycle = computer.cycle + 1 in
  let rem_cycles = computer.curr_remaining_cycles in
  if rem_cycles > 0 then
    { computer with cycle = new_cycle; curr_remaining_cycles = rem_cycles - 1 }
  else
    match computer.program with
    | [] -> { computer with cycle = new_cycle }
    | hd :: rest ->
        {
          cycle = new_cycle;
          x = update_x hd computer.x;
          curr_remaining_cycles =
            (match rest with [] -> 0 | n :: _ -> cycle_count n - 1);
          program = rest;
        }

let print_computer c =
  printf "%05d x = %d, %d\n" c.cycle c.x c.curr_remaining_cycles

let parse_instruction str =
  if str |> StringLabels.starts_with ~prefix:"noop" then Noop
  else sscanf str "addx %d" (fun x -> Addx x)

let program =
  let open List in
  In_channel.read_all "inputs/day10.txt"
  |> String.strip |> String.split_lines >>| parse_instruction

let rec run_for cycles computer : computer =
  if cycles = 0 then computer else run_for (cycles - 1) @@ next_cycle computer

let part1 program =
  let computer = ref (start program) in
  let result = ref 0 in
  (* 20th cycle *)
  computer := run_for 19 !computer;
  print_computer !computer;
  result := !result + (!computer.cycle * !computer.x);
  for _ = 1 to 5 do
    computer := run_for 40 !computer;
    print_computer !computer;
    result := !result + (!computer.cycle * !computer.x)
  done;
  !result

let part2 program =
  let computer = ref (start program) in
  for _ = 0 to 5 do
    for x = 0 to 39 do
      let sprite_middle = !computer.x in
      let is_visible = x >= sprite_middle - 1 && x <= sprite_middle + 1 in
      let ch = if is_visible then '#' else '.' in
      printf "%c" ch;
      computer := next_cycle !computer
    done;
    printf "\n"
  done;

part1 program |> printf "Part1: %d\n";;

let sample =
  let open List in
  In_channel.read_all "inputs/day10_sample.txt"
  |> String.strip |> String.split_lines >>| parse_instruction
;;

part2 sample;;
part2 program
