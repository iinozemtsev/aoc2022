open Core

let rec four_distinct (start : int) (chars : char list) : int =
  match chars with
  | a :: b :: c :: d :: rest ->
      if Char.Set.of_list [ a; b; c; d ] |> Char.Set.length = 4 then start + 4
      else four_distinct (start + 1) (b :: c :: d :: rest)
  | _ -> assert false

let input = In_channel.read_all "inputs/day6.txt" |> String.strip

let () =
  input |> String.to_list |> four_distinct 0 |> Printf.printf "Part1: %d\n"

let rec fourteen_distinct (start : int) (chars : char array) : int =
  let uniq =
    Array.slice chars start (start + 14) |> Char.Set.of_array |> Char.Set.length
  in
  if uniq = 14 then start + 14 else fourteen_distinct (start + 1) chars

let () =
  input |> String.to_array |> fourteen_distinct 0
  |> Printf.printf "Partd2: %d\n"
