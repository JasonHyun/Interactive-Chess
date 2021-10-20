(**Representation of what inputs the front-end team will send to back-end. 
  This module represesents the interface that the front-end team will use to 
  communicate to the back-end, as well as the types that the backend will convert this to.
*)

type move
(** The abstract type of values representing the moves made on the board *)

type player = White | Black

type column = char
(** The type of column identifiers, a capital letter A-G *)
type row = int
(** The type of row identifiers, a positive integer 1-8 inclusive*)
type coordinate = {
  column: column;
  row: row;
}
(** The type of coordinate identifiers*)
type piece_type = char
(** The type representing piece name
 Rook: 'R'
 Bishop: 'B'
 Pawn: 'P'
 Knight: 'N'
 King: 'K'
 Queen: 'Q'
*) 

type time = int
(** The type representing time, an int in milliseconds.*)
type valid = bool 
(** True iff the move is legal*)
type check = bool
(** True iff the move places the opposing player in check*)
type checkmate = bool
(** True iff the move places the opposing player in checkmate*)

(*Below are the methods I suspect will be most useful to the front end team*)

type board_coord = {
  column:int;
  row:int;
}
val demo : board_coord -> board_coord -> checkmate

val make_move: coordinate -> coordinate -> move
(*[start_coordinate end_coordinate] takes the given input, creates object of type [move] 
  with given start coordinate, end coordinate*)
val get_log: move list 
(*Returns a list of all of the moves taken in chronological ordering*)
val get_start: move -> coordinate
(*Gets the starting coordinate of the given move*)
val get_end: move -> coordinate
(*Gets the end coordinate of the given move*)
val get_piece_type: move -> piece_type
(*Gets the piece used in the given move*)
val get_owner: move -> player
(*Gets the owner of the piece used in the given move*)
val get_valid: move -> valid
(*True iff the move is valid*)
val get_check: move -> check
(*True iff the move puts the opponent in check*)
val get_checkmate: move -> checkmate
(*True iff the move puts the opponent in checkmate*)
val get_time_since_start: move -> time
(*Gets the time elapsed from game start to the given move, in seconds*)
val get_time_since_last_move: time 
(*Gets the time elapsed since the last move in seconds*)
val get_piece: coordinate -> piece_type
(*Returns the type of piece at the given coordinate*)
val get_piece_owner: coordinate -> player
(*Returns the owner of the piece at the given coordinate*)
val get_time_left: player -> time
(*Returns the amount of time that the given player has remaining*)
val get_legal_moves: coordinate -> coordinate list
(*Returns all the positions that the piece at the coordinate can legally move to.*)



