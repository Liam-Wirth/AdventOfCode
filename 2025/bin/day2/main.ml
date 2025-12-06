let () = print_endline "Day 2"

let infile = "bin/day2/input.txt"

let read_file fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let contents : string = read_file infile

(* Parse input into (start, stop) ranges *)
let ranges : (int * int) list =
  contents
  |> String.split_on_char ','             (* split "a-b,c-d,..." *)
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun s ->
         match String.split_on_char '-' (String.trim s) with
         | [a; b] -> (int_of_string a, int_of_string b)
         | _ -> failwith ("bad range: " ^ s)
     )

(* An ID is invalid if it is made only of some sequence of digits repeated twice *)
let is_invalid_id (id : string) : bool =
  let len = String.length id in
  if len mod 2 <> 0 then
    false
  else
    let half = len / 2 in
    let first_half  = String.sub id 0 half in
    let second_half = String.sub id half half in
    first_half = second_half

(* Collect all invalid IDs from all ranges *)
let invalid_ids : int list =
  ranges
  |> List.concat_map (fun (start_i, end_i) ->
         let rec loop acc n =
           if n > end_i then List.rev acc
           else
             let s = string_of_int n in
             if is_invalid_id s then
               loop (n :: acc) (n + 1)
             else
               loop acc (n + 1)
         in
         loop [] start_i
     )

let sum_invalid_ids : int64 =
  List.fold_left
    (fun acc id -> Int64.add acc (Int64.of_int id))
    Int64.zero
    invalid_ids

let () =
  Printf.printf "Sum of invalid ids: %Ld\n" sum_invalid_ids
