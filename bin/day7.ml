open Core

(* screw immutability *)
type file = { name : string; size : int }

type dir = {
  name : string;
  files : file Stack.t;
  dirs : dir Stack.t;
  mutable total_size : int;
}

type pwd = string Stack.t
type computer = { pwd : pwd; root : dir }

let make_computer () : computer =
  {
    pwd = Stack.create ();
    root =
      {
        name = "/";
        files = Stack.create ();
        dirs = Stack.create ();
        total_size = 0;
      };
  }

let make_dir name : dir =
  { name; files = Stack.create (); dirs = Stack.create (); total_size = 0 }

let current_dir (computer : computer) : dir =
  Stack.to_array computer.pwd
  |> Array.rev
  |> Array.fold ~init:computer.root ~f:(fun dir child ->
         dir.dirs
         |> Stack.find ~f:(fun d -> String.equal d.name child)
         |> Option.value_exn)

let rec print_dir (prefix : string) (dir : dir) : unit =
  Printf.printf "%s%s (%d)\n" prefix dir.name dir.total_size;
  dir.files
  |> Stack.iter ~f:(fun (f : file) ->
         Printf.printf "%s  %s %d\n" prefix f.name f.size);
  let new_prefix = Printf.sprintf "%s  " prefix in
  dir.dirs |> Stack.iter ~f:(print_dir new_prefix)

let mkcd (computer : computer) (name : string) : unit =
  let cur = current_dir computer in
  name |> Stack.push computer.pwd;
  make_dir name |> Stack.push cur.dirs

let process_cd (computer : computer) (str : string) : unit =
  let path = Scanf.sscanf str "cd %s" (fun s -> s) in
  match path with
  | "/" -> ()
  | ".." -> computer.pwd |> Stack.pop_exn |> ignore
  | "" -> assert false
  | non_empty -> non_empty |> mkcd computer

let touch (computer : computer) (name : string) (size : int) : unit =
  let cur = current_dir computer in
  { name; size } |> Stack.push cur.files

let process_ls_entry (computer : computer) (entry : string) : unit =
  if Str.string_match (Str.regexp {|^dir|}) entry 0 then ()
  else
    let size, name =
      Scanf.sscanf entry "%d %s" (fun size name -> (size, name))
    in
    touch computer name size

let process_ls (computer : computer) (str : string) : unit =
  str |> String.split_lines |> List.tl_exn
  |> List.iter ~f:(process_ls_entry computer)

let process_cmd (computer : computer) (str : string) : unit =
  if Str.string_match (Str.regexp {|^cd|}) str 0 then process_cd computer str
  else if Str.string_match (Str.regexp {|^ls|}) str 0 then
    process_ls computer str
  else assert false

let rec traverse visitor (dir : dir) : unit =
  visitor dir;
  dir.dirs |> Stack.iter ~f:(traverse visitor)

let rec compute_transitive_size (dir : dir) : int =
  let result =
    dir.dirs |> Stack.to_list
    |> List.map ~f:(fun d -> compute_transitive_size d)
    |> List.append
         (dir.files |> Stack.to_list |> List.map ~f:(fun file -> file.size))
    |> List.fold ~init:0 ~f:( + )
  in
  dir.total_size <- result;
  result

let input =
  In_channel.read_all "inputs/day7.txt"
  |> String.strip
  |> Str.split @@ Str.regexp {|\$ |}
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> String.length s > 0)

let () =
  input
  |> List.iter ~f:(fun s -> Printf.printf "\nCommand and output\n%s\n\n" s)

let c = make_computer ();;

List.iter input ~f:(fun cmd -> process_cmd c cmd);;
compute_transitive_size c.root

let all_dirs : dir Stack.t = Stack.create ();;

c.root |> traverse (Stack.push all_dirs)

let all_dirs_list = all_dirs |> Stack.to_list;;

print_dir "" c.root

let () =
  all_dirs_list
  |> List.filter ~f:(fun d -> d.total_size <= 100000)
  |> List.map ~f:(fun d -> d.total_size)
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "Part1: %d\n"

let () =
  let all_dirs_sorted =
    all_dirs |> Stack.to_array
    |> Array.sorted_copy ~compare:(fun d1 d2 ->
           Int.compare d1.total_size d2.total_size)
  in
  let all_space = 70000000 in
  let required_space = 30000000 in
  let available_space = all_space - c.root.total_size in
  let to_free = required_space - available_space in
  all_dirs_sorted
  |> Array.find ~f:(fun d -> d.total_size > to_free)
  |> Option.map ~f:(fun d -> d.total_size)
  |> Option.value_exn
  |> Printf.printf "Part2: %d\n"
