open Core

type hand = Rock | Paper | Scissors
type game = hand * hand

let parse_hand str =
  match str with
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | _ -> assert false

let hand_score hand = match hand with Rock -> 1 | Paper -> 2 | Scissors -> 3

let game_score (opponent, player) : int =
  match player with
  | Rock -> ( match opponent with Rock -> 3 | Paper -> 0 | Scissors -> 6)
  | Paper -> ( match opponent with Rock -> 6 | Paper -> 3 | Scissors -> 0)
  | Scissors -> ( match opponent with Rock -> 0 | Paper -> 6 | Scissors -> 3)

let total_score game =
  let _, p = game in
  hand_score p + game_score game

let parse_game (str : string) : game =
  Scanf.sscanf str "%s %s" (fun a b -> (parse_hand a, parse_hand b))

let input =
  In_channel.read_all "inputs/day2.txt" |> String.strip |> String.split ~on:'\n'

let games = input |> List.map ~f:parse_game
let scores = List.map games ~f:total_score
let total = List.fold scores ~init:0 ~f:( + )
let () = Printf.printf "part1: %d\n" total

type outcome = Lose | Draw | Win
type game2 = { opp : hand; p : outcome }

let find_draw (hand : hand) = hand

(* find hand which looses to opp hand *)
let find_lose (opp : hand) =
  match opp with Rock -> Scissors | Paper -> Rock | Scissors -> Paper

let find_win (opp : hand) =
  match opp with Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let find_player_hand { opp; p } =
  match p with
  | Lose -> find_lose opp
  | Draw -> find_draw opp
  | Win -> find_win opp

let resolve_game g = (g.opp, find_player_hand g)
let score2 g = total_score (resolve_game g)

let parse_outcome str =
  match str with "X" -> Lose | "Y" -> Draw | "Z" -> Win | _ -> assert false

let parse_game2 (str : string) : game2 =
  Scanf.sscanf str "%s %s" (fun a b ->
      { opp = parse_hand a; p = parse_outcome b })

let () =
  input |> List.map ~f:parse_game2 |> List.map ~f:score2
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "part2: %d\n"
