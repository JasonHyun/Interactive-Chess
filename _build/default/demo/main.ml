open Chess.Backend

type demoset = (board_coord * board_coord * string) list

let demo x y line =
  print_endline line;
  let res = demo x y in
  match res with
  | true -> print_endline "true\n"
  | false -> print_endline "false\n"

let rec democheck = function
  | (start, finish, line) :: t ->
      let _ = demo start finish line in
      democheck t
  | [] -> print_endline "complete.\n"

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  let piecetests =
    [
      ( { column = 1; row = 0 },
        { column = 2; row = 2 },
        "demo on moving knight from b1 to c3\n" );
    ]
  in
  democheck piecetests

(* Execute the game engine. *)
let () = main ()