(**Representation of what inputs the front-end team will send to
   back-end. This module represesents the interface that the front-end
   team will use to communicate to the back-end, as well as the types
   that the backend will convert this to. *)

type move
(** The abstract type of values representing the moves made on the board *)

type player =
  | White
  | Black

type column = char
(** The type of column identifiers, a capital letter A-G *)

type row = int
(** The type of row identifiers, a positive integer 1-8 inclusive*)

type coordinate = {
  column : column;
  row : row;
}
(** The type of coordinate identifiers*)

type piece_type = char
(** The type representing piece name Rook: 'R' Bishop: 'B' Pawn: 'P'
    Knight: 'N' King: 'K' Queen: 'Q' *)

type valid = bool
(** True iff the move is legal*)

type check = bool
(** True iff the move places the opposing player in check*)

type checkmate = bool

(** True iff the move places the opposing player in checkmate*)

(* added for the purpose of demoing, not for final product*)

type board_coord = {
  column : int;
  row : int;
}

type piece = {
  player : player;
  piece_type : piece_type;
}

type space =
  | Piece of piece
  | Empty

type board = space list list

type game

val empty_board : board

val demo : board_coord -> board_coord -> board -> checkmate

val demo_board : string list

val log_board : game -> string list

(*Returns a list of all of the moves taken in chronological ordering*)

val make_move : game -> coordinate -> coordinate -> move
(*[start_coordinate end_coordinate] takes the given input, creates
  object of type [move] with given start coordinate, end coordinate*)

val player_makes_move : game -> move -> game
(** Returns the altered game state if the move is legal, otherwise
    original game state *)

val get_log : game -> string list

val get_start : move -> coordinate
(*Gets the starting coordinate of the given move*)

val get_end : move -> coordinate
(*Gets the end coordinate of the given move*)

val get_piece_type : move -> piece_type
(*Gets the piece used in the given move*)

val get_owner : move -> player
(*Gets the owner of the piece used in the given move*)

val get_valid : move -> valid
(*True iff the move is valid*)

val get_check : move -> check
(*True iff the move puts the opponent in check*)

val get_checkmate : move -> checkmate
(*True iff the move puts the opponent in checkmate*)

val get_piece : game -> coordinate -> piece_type
(*Returns the type of piece at the given coordinate*)

val get_piece_owner : game -> coordinate -> player
(*Returns the owner of the piece at the given coordinate*)

val get_legal_moves : game -> coordinate -> coordinate list
(*Returns all the positions that the piece at the coordinate can legally
  move to.*)
