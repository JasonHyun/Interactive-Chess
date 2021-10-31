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
    (demo start_coord end_coord)
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
  ]

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)

let tests = "test suite for A1" >::: List.flatten [ init_board_tests ]

let _ = run_test_tt_main tests
