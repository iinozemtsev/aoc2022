open Coords

module Field = struct
  type 'a t = { cells : 'a array; width : int; height : int }

  let create (width : int) (height : int) (v : 'a) : 'a t =
    { cells = Core.Array.create ~len:(width * height) v; width; height }

  let get ((x, y) : coord) (field : 'a t) : 'a =
    field.cells.((y * field.width) + x)

  let set ((x, y) : coord) (value : 'a) (field : 'a t) : unit =
    field.cells.((y * field.width) + x) <- value

  let contains field (x, y) =
    x >= 0 && y >= 0 && x < field.width && y < field.height

  let parse (input : string) ~(f : char -> 'a) ~(default : 'a) : 'a t =
    let lines = input |> Core.String.strip |> Core.String.split_lines in
    let width = Core.List.hd_exn lines |> String.length in
    let height = List.length lines in
    let result : 'a t = create width height default in
    lines
    |> List.iteri (fun y line ->
           line
           |> Core.String.iteri ~f:(fun x ch -> result |> set (x, y) (f ch)));
    result

  let to_str (field : 'a t) ~(f : 'a -> char) : string =
    Core.Printf.collect_to_string (fun { printf } ->
        for y = 0 to field.height - 1 do
          for x = 0 to field.width - 1 do
            field |> get (x, y) |> f |> printf "%c"
          done;
          printf "\n"
        done)

  let single (field : 'a t) ~(f : 'a -> bool) : coord =
    let index = ref (-1) in
    field.cells |> Array.iteri (fun i v -> if f v then index := i);
    if !index = -1 then failwith "Not found"
    else
      let x = !index mod field.width in
      let y = !index / field.width in
      (x, y)

  let neighbors4 (field : 'a t) ((x, y) : coord)  =
    [ (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) ]
    |> List.filter (contains field)
end

module FieldSet = struct
  type t = bool array array

  let for_field (f : 'a Field.t) : t = Array.make_matrix f.width f.height false
  let clear ((x, y) : coord) (f : t) : unit = f.(x).(y) <- false
  let set ((x, y) : coord) (f : t) = f.(x).(y) <- true
  let contains ((x, y) : coord) (f : t) : bool = f.(x).(y)
end

type int_field = int Field.t
