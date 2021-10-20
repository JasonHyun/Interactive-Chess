include Chess.Backend

type demoset = (board_coord * board_coord * string) list

let demos x y line =
  print_endline line;
  let res = demo x y in
  match res with
  | true -> print_endline "true\n"
  | false -> print_endline "false\n"

let rec democheck = function
  | (start, finish, line) :: t ->
      let _ = demos start finish line in
      democheck t
  | [] -> print_endline "complete.\n"

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nInitializing MS1 backend demo.\n";
  print_endline "First showing representation of initial board layout";

  print_endline
    "now showing piece checks for several valid/invalid moves";
  (* let () = log_board init_board in *)
  let piecetests =
    [
      ( { column = 1; row = 0 },
        { column = 2; row = 2 },
        "demo on moving knight from b1 to c3 - should work\n" );
      ( { column = 3; row = 1 },
        { column = 3; row = 3 },
        "demo on moving pawn from d2 to d4 - should work\n" );
      ( { column = 1; row = 0 },
        { column = 2; row = 2 },
        "demo on moving rook from a1 to a5 - should fail\n" );
      ( { column = 2; row = 7 },
        { column = 2; row = 2 },
        "demo on moving bishop from c8 to c3 - should fail\n" );
    ]
  in
  democheck piecetests

(* Execute the game engine. *)
let () = main ()