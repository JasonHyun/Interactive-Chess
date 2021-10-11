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
type coordinate = (column * row)
(** The type of coordinate identifiers*)
type piece = char
(** The type representing piece name*)
type time = int
(** The type representing time, an int in milliseconds.*)
type valid = bool 
(** True iff the move is legal*)
type check = bool
(** True iff the move places the opposing player in check*)
type checkmate = bool
(** True iff the move places the opposing player in checkmate*)

(*Below are the methods I suspect will be most useful to the front end team*)
val make_move: coordinate -> coordinate -> piece  -> move
(*[start_coordinate end_coordinate piece] takes the given input, creates object of type [move] 
  with given start coordinate, end coordinate, piece *)
val get_log: move list 
(*Returns a list of all of the moves taken in chronological ordering*)
val get_start: move -> coordinate
(*Gets the starting coordinate of the given move*)
val get_end: move -> coordinate
(*Gets the end coordinate of the given move*)
val get_piece: move -> piece
(*Gets the piece used in the given move*)
val get_player: move -> player
(*Gets the player who made the given move*)
val get_time_since_start: move -> time
(*Gets the time elapsed from game start to the given move, in seconds*)
val get_time_since_last_move: move -> time 
(*Gets the time elapsed from previous to the given move, in seconds*)
val get_piece: coordinate -> piece
(*Returns the type of piece at the given coordinate*)
val get_time_left: player -> time
(*Returns the amount of time that the given player has remaining*)
val get_legal_moves: coordinate -> coordinate list
(*Returns all the positions that the given piece can legally move to.*)



