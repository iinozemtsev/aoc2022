open Core

let parse_and_sum calories =
  String.strip calories |> String.split ~on:'\n' |> List.map ~f:int_of_string
  |> List.fold ~f:( + ) ~init:0

let caloriesList =
  In_channel.read_all "inputs/day1.txt"
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:parse_and_sum
  |> List.sort ~compare:Int.descending

let () = caloriesList |> List.hd_exn |> Printf.printf "Part1: %d\n"

let () =
  (match caloriesList with a :: b :: c :: _ -> a + b + c | _ -> -1)
  |> Printf.printf "Part2: %d\n"
