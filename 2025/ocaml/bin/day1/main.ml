let () = print_endline "Day 1"

let infile = "bin/day1/input.txt"

type direction = 
   | Left
   | Right

type rotation = {
   dir: direction;
   steps: int;
}

let _string_of_direction = function
   | Left -> "Left"
   | Right -> "Right"

let _string_of_rotation d = 
   let dir = _string_of_direction d.dir in
   let steps = string_of_int d.steps in
   "Turn Dial " ^ steps ^ " To the " ^ dir

let lines_from_file (fname : string) : string list =
  let lines = ref [] in
  let chan = open_in fname in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    []  (* unreachable *)
  with End_of_file ->
    close_in chan;
    List.rev !lines

let _lines = lines_from_file infile

let parse_line s = 
   let d: direction = match s.[0] with
      | 'L' -> Left
      | 'R' -> Right
      | _ -> failwith "WHAT THE FUCK BEOOOWWWWW!!!!"
  in
  let len = String.length s in
  let steps = int_of_string (String.sub s 1 (len -1 )) in
  {dir = d; steps}

let lines = List.filter (fun s -> s <> "") (lines_from_file infile)

let _operations = List.map parse_line lines

let solve operations =
  let pos = ref 50 in
  let count = ref 0 in
  List.iter (fun r ->
      let raw =
        match r.dir with
        | Left -> !pos - r.steps
        | Right -> !pos + r.steps
      in
      let new_pos = raw mod 100 in
      let new_pos = if new_pos < 0 then new_pos + 100 else new_pos in
      pos := new_pos;
      if new_pos = 0 then
        count := !count + 1
    ) operations;
  !count

let () =
  let operations = List.map parse_line lines in
  let answer = solve operations in
  Printf.printf "Answer: %d\n" answer
