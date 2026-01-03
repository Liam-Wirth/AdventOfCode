let () = print_endline "Day 3"
let infile = "/home/liam/Projects/in-progress/AdventOfCode/2025/bin/day3/input.txt"
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

let lines = lines_from_file infile;;

let string_to_list s =  
  let rec exp i l =
    if i < 0 then l else exp (i -1 ) (s.[i] :: l) in
  exp (String.length s - 1) []

let j = "helrtkjsfdlksdfj"
let _tst = string_to_list j;;

(* now we make the list of lists *)
let banks =
  let rec build_bank_lists ls acc =
    match ls with
    | [] -> List.rev acc
    | h :: t -> build_bank_lists t (string_to_list h :: acc) in
  build_bank_lists lines []

  

(* Print out each bank with current number and size *)

(*
let () =
   let rec print_banks bls bank_num =
       match bls with
       | [] -> ()
       | h :: t ->
          Printf.printf "Bank Number %d/%d | Length: %d\n" bank_num (List.length banks) (List.length h);
          print_banks t (bank_num + 1) in
   print_banks banks 0
*)

(* Finds maximum joltage in a bank, which is determined by turning on exactly two batteries,
   left to right concatenated.

   So in 987654321111111, maximum joltage is 98.
   In 811111111118 the maximum joltage is 88
in 234234234234278 the maximum joltage is 78
in 818181819111211 the maximum joltage is 89

I think the best way to do this is to scan through the list, keeping track of the highest two-digit
number seen so far, and compare it against the current two-digit number formed by the current character
and n, automatically skip if current character is less than the first digit of the highest two-digit number.)

*)

let comp_joltage (b1: char list) : (char * char) =
   let best = match b1 with
    | a :: b :: _ -> (a,b)
    | _ -> failwith "Bank too small" in
   let  rec scan_bank bl (current_best: char * char) = function



