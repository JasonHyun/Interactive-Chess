open OUnit2
include Chess.Backend

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let init_board_test
    (name : string)
    (start_coord : board_coord)
    (end_coord : board_coord)
    (result : bool) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal result
    (demo start_coord end_coord empty_board)
    ~printer:string_of_bool

(* You will find it helpful to write functions like [make_index_test]
   for each of the other functions you are testing. They will keep your
   lists of tests below very readable, and will also help you to avoid
   repeating code. You will also find it helpful to create [~printer]
   functions for the data types in use. *)

let init_board_tests =
  [
    init_board_test "move of knight from b1 to c3 is valid"
      { column = 1; row = 0 } { column = 2; row = 2 } true;
    init_board_test "move of knight from b1 to c4 is invalid"
      { column = 1; row = 0 } { column = 2; row = 3 } false;
    init_board_test "move of knight from b1 to d2 is invalid"
      { column = 1; row = 0 } { column = 3; row = 1 } false;
    init_board_test "move of pawn from a2 to a3 is valid"
      { column = 0; row = 1 } { column = 0; row = 2 } true;
    init_board_test "move of pawn from a2 to a4 is valid"
      { column = 0; row = 1 } { column = 0; row = 3 } true;
    init_board_test "move of pawn from a2 to a4 is invalid"
      { column = 0; row = 1 } { column = 0; row = 5 } false;
    init_board_test "move of rook from a1 to a2 is invalid"
      { column = 0; row = 0 } { column = 0; row = 1 } false;
    init_board_test "move of bishop from c8 to d7 is invalid"
      { column = 2; row = 7 } { column = 3; row = 6 } false;
    init_board_test "move of king from e8 to d7 is invalid"
      { column = 4; row = 7 } { column = 3; row = 6 } false;
    init_board_test "move of queen from d8 to d7 is invalid"
      { column = 3; row = 7 } { column = 3; row = 6 } false;
  ]

(**Makes a row of empty places for map initialization*)
let empty_row =
  [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]

let test_board =
  [
    empty_row;
    [
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Piece { player = White; piece_type = 'P' };
    ];
    empty_row;
    [
      Empty;
      Empty;
      Piece { player = White; piece_type = 'Q' };
      Empty;
      Empty;
      Empty;
      Piece { player = White; piece_type = 'P' };
      Piece { player = Black; piece_type = 'R' };
    ];
    [
      Empty;
      Piece { player = Black; piece_type = 'R' };
      Empty;
      Empty;
      Piece { player = White; piece_type = 'P' };
      Empty;
      Empty;
      Empty;
    ];
    [
      Empty;
      Empty;
      Empty;
      Empty;
      Piece { player = White; piece_type = 'K' };
      Piece { player = Black; piece_type = 'P' };
      Empty;
      Empty;
    ];
    [
      Empty;
      Piece { player = Black; piece_type = 'P' };
      Piece { player = Black; piece_type = 'B' };
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
    ];
    [
      Piece { player = White; piece_type = 'N' };
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
    ];
  ]

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let adv_test
    (name : string)
    (start_coord : board_coord)
    (end_coord : board_coord)
    (result : bool) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal result
    (demo start_coord end_coord test_board)
    ~printer:string_of_bool

let adv_tests =
  [
    adv_test "adv - move of knight from a8 to c7 is valid"
      { column = 0; row = 7 } { column = 2; row = 6 } true;
    adv_test "adv - move of pawn from e5 to f6 is valid"
      { column = 4; row = 4 } { column = 5; row = 5 } true;
    adv_test "adv - move of pawn from f6 to e5 is valid"
      { column = 5; row = 5 } { column = 4; row = 4 } true;
    adv_test "adv - move of pawn from e5 to f5 is invalid"
      { column = 4; row = 4 } { column = 5; row = 4 } false;
    adv_test "adv - move of pawn from e5 to e6 is invalid"
      { column = 4; row = 4 } { column = 4; row = 5 } false;
    adv_test "adv - move of pawn from f6 to f4 is invalid"
      { column = 5; row = 5 } { column = 5; row = 3 } false;
    adv_test "adv - move of pawn from g4 to g6 is invalid"
      { column = 6; row = 3 } { column = 6; row = 5 } false;
    adv_test "adv - move of pawn from b7 to b5 is invalid"
      { column = 1; row = 6 } { column = 1; row = 4 } false;
    adv_test "adv - move of pawn from h2 to h4 is invalid"
      { column = 7; row = 1 } { column = 7; row = 3 } false;
    adv_test "adv - move of rook from b5 to b0 is valid"
      { column = 1; row = 4 } { column = 1; row = 0 } true;
    adv_test "adv - move of rook from b5 to e5 is valid"
      { column = 1; row = 4 } { column = 4; row = 4 } true;
    adv_test "adv - move of rook from b5 to c6 is invalid"
      { column = 1; row = 4 } { column = 2; row = 5 } false;
    adv_test "adv - move of bishop from c7 to a5 is valid"
      { column = 2; row = 6 } { column = 0; row = 4 } true;
    adv_test "adv - move of bishop from c7 to e5 is valid"
      { column = 2; row = 6 } { column = 4; row = 4 } true;
    adv_test "adv - move of bishop from c7 to c6 is invalid"
      { column = 2; row = 6 } { column = 2; row = 5 } false;
    adv_test "adv - move of queen from c4 to c7 is valid"
      { column = 2; row = 3 } { column = 2; row = 6 } true;
    adv_test "adv - move of queen from c4 to c2 is valid"
      { column = 2; row = 3 } { column = 2; row = 1 } true;
    adv_test "adv - move of queen from c4 to b5 is valid"
      { column = 2; row = 3 } { column = 1; row = 4 } true;
    adv_test "adv - move of queen from c4 to a2 is valid"
      { column = 2; row = 3 } { column = 0; row = 1 } true;
  ]

let tests =
  "test suite for piece checks"
  >::: List.flatten [ init_board_tests; adv_tests ]

let _ = run_test_tt_main tests
