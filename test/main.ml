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
    (demo start_coord end_coord [ [ Empty ] ])
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

let test_board =
  [
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
    [
      Empty;
      Empty;
      Piece { player = Black; piece_type = 'B' };
      Empty;
      Empty;
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
      Piece { player = White; piece_type = 'Q' };
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
    (demo start_coord end_coord [ [ Empty ] ])
    ~printer:string_of_bool

let adv_tests =
  [
    adv_test "adv - move of knight from a5 to c4 is valid"
      { column = 0; row = 4 } { column = 2; row = 3 } true;
    adv_test "adv - move of pawn from e2 to f3 is valid"
      { column = 4; row = 1 } { column = 5; row = 2 } true;
    adv_test "adv - move of pawn from f3 to e2 is valid"
      { column = 5; row = 2 } { column = 4; row = 1 } true;
    adv_test "adv - move of pawn from e2 to f2 is invalid"
      { column = 4; row = 1 } { column = 5; row = 1 } false;
    adv_test "adv - move of rook from b2 to b5 is valid"
      { column = 1; row = 1 } { column = 1; row = 4 } true;
    adv_test "adv - move of rook from b2 to e2 is valid"
      { column = 1; row = 1 } { column = 4; row = 1 } true;
    adv_test "adv - move of rook from b2 to c3 is invalid"
      { column = 1; row = 1 } { column = 2; row = 2 } false;
    adv_test "adv - move of bishop from c4 to a2 is valid"
      { column = 2; row = 3 } { column = 0; row = 1 } true;
    adv_test "adv - move of bishop from c4 to e2 is valid"
      { column = 2; row = 3 } { column = 4; row = 1 } true;
    adv_test "adv - move of bishop from c4 to e2 is invalid"
      { column = 2; row = 3 } { column = 2; row = 2 } false;
    adv_test "adv - move of queen from c1 to c4 is valid"
      { column = 2; row = 0 } { column = 2; row = 3 } true;
    adv_test "adv - move of queen from c1 to c3 is valid"
      { column = 2; row = 0 } { column = 2; row = 2 } true;
    adv_test "adv - move of queen from c1 to b2 is valid"
      { column = 2; row = 0 } { column = 1; row = 1 } true;
    adv_test "adv -   move of queen from c1 to b2 is valid"
      { column = 2; row = 0 } { column = 3; row = 1 } true;
  ]

let tests =
  "test suite for piece checks"
  >::: List.flatten [ init_board_tests; adv_tests ]

let _ = run_test_tt_main tests
