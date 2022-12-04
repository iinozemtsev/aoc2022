open Core

type compartment = Char.Set.t
type rucksack = compartment * compartment

let score ch =
  match ch with
  | 'a' .. 'z' -> Char.to_int ch - Char.to_int 'a' + 1
  | 'A' .. 'Z' -> Char.to_int ch - Char.to_int 'A' + 27
  | _ -> assert false

let parse_compartment (str : string) =
  String.fold ~f:Char.Set.add ~init:Char.Set.empty str

let parse_rucksack (str : string) : rucksack =
  let half_len = String.length str / 2 in
  ( String.sub str ~pos:0 ~len:half_len |> parse_compartment,
    String.sub str ~pos:half_len ~len:half_len |> parse_compartment )

let common (c1, c2) = Char.Set.inter c1 c2 |> Char.Set.elements |> List.hd_exn

let rucksacks =
  In_channel.read_all "inputs/day3.txt"
  |> String.strip |> String.split ~on:'\n' |> List.map ~f:parse_rucksack

let rucksack_score r = score (common r)
let scores = List.map rucksacks ~f:rucksack_score
let total = List.fold scores ~init:0 ~f:( + )
let () = Printf.printf "Part1: %d\n" total

type group = rucksack list

let rec to_groups rucksacks =
  match rucksacks with
  | g1 :: g2 :: g3 :: rest -> [ g1; g2; g3 ] :: to_groups rest
  | [] -> []
  | _ -> assert false

let common group =
  match (group : group) with
  | (c11, c12) :: (c21, c22) :: (c31, c32) :: _ ->
      Char.Set.union c21 c22
      |> Char.Set.inter (Char.Set.union c11 c12)
      |> Char.Set.inter (Char.Set.union c31 c32)
      |> Char.Set.elements |> List.hd_exn
  | _ -> assert false

let () =
  to_groups rucksacks |> List.map ~f:common |> List.map ~f:score
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "Part2: %d\n"
