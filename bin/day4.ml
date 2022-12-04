open Core

let contains (from1, to1) (from2, to2) = from1 <= from2 && to1 >= to2
let full_overlap (r1, r2) = contains r1 r2 || contains r2 r1
let overlaps ((from1, to1), (from2, to2)) = to1 >= from2 && to2 >= from1

let parse_pair str =
  Scanf.sscanf str "%d-%d,%d-%d" (fun a b c d -> ((a, b), (c, d)))

let input = In_channel.read_all "inputs/day4.txt" |> String.strip
let pairs = String.split ~on:'\n' input |> List.map ~f:parse_pair
let intersections = List.filter pairs ~f:full_overlap
let () = List.length intersections |> Printf.printf "Part1: %d\n"
let oversaps = List.filter pairs ~f:overlaps
let () = Printf.printf "Part2: %d\n" (List.length oversaps)
