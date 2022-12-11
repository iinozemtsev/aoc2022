open Core
open Printf
open Scanf

type operand = Old | Val of int
type operation = Add of (operand * operand) | Mul of (operand * operand)
type test = { modulo : int; when_true : int; when_false : int }
type monkey = { id : int; items : int Queue.t; op : operation; test : test; mutable inspect_count : int }

let id x = x

let parse_operand str =
  if String.(str = "old") then Old else Val (int_of_string str)

let parse_operation (str : string) : operation =
  let (parts : string list) =
    str
    |> String.chop_prefix_exn ~prefix:"Operation: new = "
    |> String.split ~on:' '
  in
  match parts with
  | [ left_str; op; right_str ] -> (
      let left = parse_operand left_str in
      let right = parse_operand right_str in
      match op with
      | "*" -> Mul (left, right)
      | "+" -> Add (left, right)
      | _ -> assert false)
  | _ -> assert false

let parse_test (q : string Queue.t) =
  let open Queue in
  let line = dequeue_exn q in
  let modulo = sscanf line "Test: divisible by %d" id in
  let when_true = sscanf (dequeue_exn q) "If true: throw to monkey %d" id in
  let when_false = sscanf (dequeue_exn q) "If false: throw to monkey %d" id in
  { modulo; when_true; when_false }

let parse_start_items str =
  let open List in
  str
  |> String.chop_prefix_exn ~prefix:"Starting items: "
  |> String.split ~on:',' >>| String.strip >>| int_of_string

let parse_monkey_id str = sscanf str "Monkey %d:" (fun m -> m)

let parse_monkey (q : string Queue.t) =
  let open Queue in
  let id = dequeue_exn q |> parse_monkey_id in
  let items = dequeue_exn q |> parse_start_items |> Queue.of_list in
  let op = dequeue_exn q |> parse_operation in
  let test = q |> parse_test in
  { id; items; op; test; inspect_count = 0 }

let rec parse_monkeys_rec q =
  let monkey = q |> parse_monkey in
  let next_line = q |> Queue.peek in
  match next_line with
  | None -> [ monkey ]
  | Some "" ->
      q |> Queue.dequeue_exn |> ignore;
      monkey :: parse_monkeys_rec q
  | Some other ->
      printf "Unexpected line: %s" other;
      assert false

let parse_monkeys str =
  let open List in
  let q =
    str |> String.strip |> String.split ~on:'\n' >>| String.strip
    |> Queue.of_list
  in
  parse_monkeys_rec q |> Array.of_list

let operand_to_str op = match op with Old -> "old" | Val x -> string_of_int x

let operation_to_str op =
  match op with
  | Add (x, y) ->
      collect_to_string (fun { printf } ->
          printf "%s + %s" (operand_to_str x) (operand_to_str y))
  | Mul (x, y) ->
      collect_to_string (fun { printf } ->
          printf "%s * %s" (operand_to_str x) (operand_to_str y))

let print_test t =
  collect_to_string (fun { printf } ->
      printf "  Test: divisible by %d\n" t.modulo;
      printf "    If true: throw to monkey %d\n" t.when_true;
      printf "    If false: throw to monkey %d\n" t.when_false)

let print_monkey m =
  let open List in
  collect_to_string (fun { printf } ->
      printf "Monkey %d\n" m.id;
      printf "  Starting items: %s\n"
        (m.items |> Queue.to_list >>| string_of_int |> String.concat ~sep:", ");
      printf "  Operation: new = %s\n" (operation_to_str m.op);
      printf "%s\n" (print_test m.test))

let sample_monkeys = In_channel.read_all "inputs/day11_sample.txt" |> parse_monkeys;;


type monkeys = monkey array

let eval_operand operand old = match operand with Old -> old | Val x -> x

let apply_operation ?(debug = true) item operation =
  match operation with
  | Add (x, y) ->
      let left = eval_operand x item in
      let right = eval_operand y item in
      let result = left + right in
      if debug then printf "    Worry level increases by %d to %d.\n" right result;
      result
  | Mul (x, y) ->
      let left = eval_operand x item in
      let right = eval_operand y item in
      let result = left * right in
      if debug then
        printf "    Worry level is multiplied by %d to %d.\n" right result;
      result

let turn ?(debug = true) ?(modulo = 0) ?(is_part1 = true) (monkeys : monkeys) (monkey : monkey) : unit =
  if debug then printf "Monkey %d:\n" monkey.id;
  (* Monkey 0: *)
  while not (monkey.items |> Queue.is_empty) do
    monkey.inspect_count <- monkey.inspect_count + 1;
    let item = monkey.items |> Queue.dequeue_exn in
    if debug then
      printf "  Monkey inspects an item with a worry level of %d.\n" item;
    (*   Monkey inspects an item with a worry level of 79. *)
    let updated = apply_operation ~debug item monkey.op in
    (*     Worry level is multiplied by 19 to 1501. *)
    let divided = if is_part1 then  updated / 3 else updated in
    let final = if modulo = 0 then divided else divided mod modulo in
    if debug then
      printf
        "    Monkey gets bored with item. Worry level is divided by 3 to %d.\n"
        final;
    let modulo = final % monkey.test.modulo in
    if modulo = 0 then (
      if debug then
        printf "    Current worry level is divisible by %d\n" monkey.test.modulo;
      final |> Queue.enqueue monkeys.(monkey.test.when_true).items;
      if debug then
        printf "    Item with worry level %d is thrown to monkey %d.\n" final
          monkey.test.when_true)
    else (
      if debug then
        printf "    Current worry level is not divisible by %d.\n"
          monkey.test.modulo;
      final |> Queue.enqueue monkeys.(monkey.test.when_false).items;
      if debug then
        printf "    Item with worry level %d is thrown to monkey %d.\n" final
          monkey.test.when_false)
  done;
  ()

let calc_modulo (m: monkeys) = m |> Array.fold ~init:1 ~f:(fun r e -> r * e.test.modulo)

let mround ?(debug = true) ?(is_part1 = true) (m : monkeys) : unit =
  let modulo = calc_modulo m in
  Array.iter m ~f:(fun i -> turn ~debug:debug ~modulo:modulo ~is_part1:is_part1 m i)
;;

let business (m: monkeys) = let sorted = m |> Array.map ~f:(fun m -> m.inspect_count) |> Array.sorted_copy ~compare:Int.descending in sorted.(0) * sorted.(1);;

mround ~debug:true sample_monkeys

let monkeys  = In_channel.read_all "inputs/day11.txt" |> parse_monkeys;;
print_monkey monkeys.(0);;
 monkeys |> calc_modulo |> printf "Modulo: %d\n";;
for _ = 0 to 19 do  mround ~debug:false monkeys done;;
monkeys |> Array.iter ~f:(fun m -> printf "Monkey %d: %d\n" m.id m.inspect_count);;
monkeys |> business |> printf "Part1: %d\n";;
printf "\n\nPart 2\n";;
let monkeys2  = In_channel.read_all "inputs/day11.txt" |> parse_monkeys;;
for _ = 0 to 9999 do  mround ~debug:false ~is_part1:false monkeys2 done;;
monkeys2 |> Array.iter ~f:(fun m -> printf "Monkey %d: %d\n" m.id m.inspect_count);;
monkeys2 |> business |> printf "Part2: %d\n";;
