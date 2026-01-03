let lines_from_file (fname : string) : string list =
  let lines = ref [] in
  let chan = open_in fname in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    [] (* I think this is so that it will return an empty list as a fallback*)
  with End_of_file ->
    close_in chan;
    List.rev !lines (* Reverse the list because... uhh.. I said so *)
