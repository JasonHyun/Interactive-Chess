open Chess.Backend

let write x = print_string x

let x = write "demo on moving knight from b1 to c3"

let res = demo { column = 1; row = 0 } { column = 2; row = 2 }

let x = write (if res then "true\n" else "false\n")

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "demo on moving knight from b1 to c3\n";
  let res = demo { column = 1; row = 0 } { column = 2; row = 2 } in
  match res with
  | true -> print_endline "true\n"
  | false -> (
      print_endline "false\n";
      print_endline "demo on moving knight from b1 to c3\n";
      let res = demo { column = 1; row = 0 } { column = 2; row = 2 } in
      match res with
      | true -> print_endline "true\n"
      | false -> print_endline "false\n")

(* Execute the game engine. *)
let () = main ()