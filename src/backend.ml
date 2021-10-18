type player = White | Black
(*Player identifier, color based on regular chess considerations*)
type column = char
(** The type of column identifiers, a capital letter A-G *)
type row = int
(** The type of row identifiers, a positive integer 1-8 inclusive*)
type coordinate = {
  column: column;
  row: row;
}
(** The type of coordinate identifiers*)
type index = int
(** type of an index of the 8x8 board, range of 0-7 *)
type board_coord = {
  column:index;
  row:index;
}
(** type of record to index board, derived from coordinate *)
type piece_type = char
(** The type representing piece name
 Rook: 'R'
 Bishop: 'B'
 Pawn: 'P'
 Knight: 'N'
 King: 'K'
 Queen: 'Q'
*) 
type piece = {
  player: player;
  piece_type: piece_type;
}
(* Defines a piece on the board, by showing the player who controls it
   and the piece's type*)
type space = Piece of piece | Empty
(*Defines a space on the board, either a piece or an empty space*)
type time = int
(** The type representing time, an int in milliseconds.*)
let player_time_in_mins= 15
(* The amount of time each player gets to take their turns*)
let player_time = player_time_in_mins * 60 * 1000
(*Conversion of player time to millisecond format *)
type valid = bool 
(** True iff the move is legal*)
type check = bool
(** True iff the move places the opposing player in check*)
type checkmate = bool
(** True iff the move places the opposing player in checkmate*)

type move = {
  player: player;
  start_coord: coordinate;
  end_coord: coordinate;
  piece: piece;
  time_since_start: time;
  valid: valid;
  check: check;
  checkmate: checkmate;
}

type board = space list list
(** The board will always be a matrix of pieces 8x8 *)

type game = {
  board: board;
  white_time: time;
  black_time: time;
}
(** Defines the total state of a game, including board state and time
    left for every player*)

let make_pieces (player : player) (piece_types : piece_type list) =
  let rec make_rec
      (player : player)
      (piece_types : piece_type list)
      (pieces : space list) =
    match piece_types with
    | [] -> pieces
    | h :: t ->
        make_rec player t (pieces @ [ Piece { player; piece_type = h } ])
  in
  make_rec player piece_types []
(** Makes pieces of given types and player. Kinda ugly so refactor should be done.*)

let empty_row =
  [Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty]
(**Makes a row of empty places for map initialization*)
let make_pawns (player: player)=
  make_pieces player ['P';'P';'P';'P';'P';'P';'P';'P']
(**Makes a row of pawns under the control of the given player*)
let make_rest (player: player)=
  make_pieces player ['R';'N';'B';'Q';'K';'B';'N';'R']
(**Creates a row that contains the starting non-pawn pieces for the given player*)
let init_board = 
  [make_rest Black; make_pawns Black; empty_row; empty_row; 
  empty_row; empty_row; make_pawns White; make_rest White; ]
(**Initializes the state of a board at the start of the game of chess*)


let get_piece_from_space (space: space) =
  match space with 
  | Empty -> ' '
  | Piece piece -> 
    match piece.player with 
      | White -> piece.piece_type 
      | Black -> Char.lowercase_ascii piece.piece_type
(**Gives a printable char representation of a piece from the given space. 
Uppercase is a white piece, lowercase for a black piece*)
let log_row (row: space list)=
  let chars = List.map get_piece_from_space row in 
  print_string (String.concat " " (List.map Char.escaped chars))
(**Logs the given row of the board to the printer.*)
let log_board (board: board) = 
  List.iter log_row board
(**Logs the board to the printer.*)
let init_game = 
  {
    board = init_board;
    white_time = player_time;
    black_time = player_time;
  }
(*Initializes the board to the correct opening state, sets players' time
  as well *)


let col_to_index(column: column) =
  match column with 
  | 'A' -> 0
  | 'B' -> 1 
  | 'C' -> 2
  | 'D' -> 3 
  | 'E' -> 4
  | 'F' -> 5 
  | 'G' -> 6
  | 'H' -> 7
  | _ -> failwith ("col_to_index invalid column "^ Char.escaped column) 
(** Maps the column character to an index to be used when indexing rows*)

let coord_to_index (coordinate : coordinate) =
  { column = col_to_index coordinate.column; row = coordinate.row - 1 }
(** convert coordinate type to board_coord type *)

let check_coord_in_bounds (board_coord: board_coord)= 
    let columnCheck = (board_coord.column >= 0 && board_coord.column <= 7) in
    let rowCheck = (board_coord.row >= 0 && board_coord.row <= 7) in 
    columnCheck && rowCheck
(**Checks that the given coordinate obeys the rules for coordinates *)

let get_space_at_coord (board_coord: board_coord) (board: board) = 
  if check_coord_in_bounds board_coord then 
   let row = List.nth board board_coord.row in 
    List.nth row board_coord.column
  else 
    failwith ("Unusable coordinate supplied to get_space_at_coord")
(** Returns the space on the board with the given coordinates*)

let move_dist (start_coord : board_coord) (end_coord : board_coord) =
  ( Int.abs
      (end_coord.column - start_coord.column),
    Int.abs (end_coord.row - start_coord.row) )
(** Finds distance travelled from start to end locations recorded in
    magnitude *)

let space_check (coord : board_coord) (board : board) =
  let tile = get_space_at_coord coord board in
  get_piece_from_space tile
(** returns piece color at a given coordinate, if empty returns space *)


let rec path_clear
    (current : board_coord)
    (finish : board_coord)
    (board : board)
    (incr_c : int)
    (incr_r : int) =
  if current = finish then
    let first = get_space_at_coord finish board in
    let second = get_space_at_coord current board in
    match (first, second) with
    | Piece p1, Piece p2 -> p1.player <> p2.player
    | _ -> true
  else
    space_check current board = ' '
    && path_clear
         { column = current.column + incr_c; row = current.row + incr_r }
         finish board incr_c incr_r
(** checks for a linear path from start to end that no other pieces are
    in the way on that path *)

let incr_deriv (start_coord: board_coord) (end_coord: board_coord) =
  let dx = end_coord.column-start_coord.column in let dy = end_coord.row-start_coord.row in 
  (if dx = 0 then 0 else dx/Int.abs(dx), if dy = 0 then 0 else dy/Int.abs(dy))


let check_pawn (start_coord: board_coord) (end_coord: board_coord) (board : board) = let (dy,dx) = move_dist start_coord end_coord in (dx = 0 && (dy = 1 ))
(*TODO checks using pawn rules to see if move is valid*)
let check_knight
    (start_coord : board_coord)
    (end_coord : board_coord)
    (board : board) =
  let dy, dx = move_dist start_coord end_coord in
  dy + dx = 3
  && Int.abs (dy - dx) = 1
  && path_clear end_coord end_coord board dx dy
(*TODO checks using knight rules to see if move is valid*)
let check_bishop (start_coord : board_coord) (end_coord : board_coord) (board : board) =
  let dx, dy = move_dist start_coord end_coord in
  dy = dx && dy > 0 && path_clear start_coord end_coord board 1 2
(*TODO checks using bishop rules to see if move is valid*)
let check_king
    (start_coord : board_coord)
    (end_coord : board_coord)
    (board : board) =
  let dy, dx = move_dist start_coord end_coord in
  (dy > 0 || dx > 0) && space_check end_coord board = ' '
(*TODO checks using king rules to see if move is valid*)
let check_rook (start_coord: board_coord) (end_coord: board_coord) (board : board) = 
  let dy, dx = move_dist start_coord end_coord in
  (dx = 0 && dy > 0) || (dy = 0 && dx > 0)
(*TODO checks using rook rules to see if move is valid*)
let check_queen (start_coord: board_coord) (end_coord: board_coord) (board : board) = 
  check_bishop start_coord end_coord || check_rook start_coord end_coord
(*TODO checks using queen rules to see if move is valid*)

  
let check_coords_in_bounds (coordinate_list: board_coord list)= 
  let bool_list =List.map check_coord_in_bounds coordinate_list in
  let is_true x = x = true in 
    List.for_all(is_true) bool_list
(**Checks that all coordinates in the given list follow the invariants.  *)

let check_piece_rules (start_coord: board_coord) (end_coord: board_coord) (board: board) (piece_type: piece_type)= 
  match piece_type with
  | 'P' -> check_pawn start_coord end_coord
  | 'N' -> check_knight start_coord end_coord
  | 'B' -> check_bishop start_coord end_coord
  | 'K' -> check_king start_coord end_coord
  | 'R' -> check_rook start_coord end_coord
  | 'Q' -> check_queen start_coord end_coord
  | _ -> failwith ("Board invariant violated: non-valid piece-type "^ Char.escaped piece_type)
(**Using the rules for the given piece, returns true iff the given move is legal. Ignores if move puts you in check*)
let check_move (start_coord: board_coord) (end_coord: board_coord) (board: board)= 
  if check_coords_in_bounds [start_coord; end_coord] = false then failwith ("Invalid coordinate") else 
    let piece = get_space_at_coord start_coord board in
      match piece with
      | Empty -> false 
      | Piece piece-> check_piece_rules start_coord end_coord board piece.piece_type
        
(**Checks that the move from the start coordinate to the end coordinate follows the rules of movement 
for the given piece, ignores putting oneself into check considerations *)


let get_check(move: move) = move.check
(*True iff the move puts the opponent in check*)
let get_checkmate(move: move) = move.checkmate
(*True iff the move puts the opponent in checkmate*)
let get_valid(move: move) = move.valid
(*True iff the move is valid*)
let get_owner(move: move) = move.player
(*Gets the owner of the piece used in the given move*)
let get_start (move: move) =
  move.start_coord
(*Gets the starting coordinate of the given move*)
let get_end (move: move) =
  move.end_coord
(*Gets the end coordinate of the given move*)
let get_piece_type (move: move) =
  move.piece.piece_type
(*Gets the piece used in the given move*)

(** Unimplementable functionality for near future specified by MLI. Many of these fail because of no persistant state*)
let get_legal_moves (coordinate: coordinate)= failwith ("Unimplemented")
(*Returns all the positions that the piece at the coordinate can legally move to.*)
let get_time_left (player: player)= failwith("Unimplemented")
(*Returns the amount of time that the given player has remaining*)
let get_piece_owner (coordinate: coordinate) = failwith("Unimplemented")
(*Returns the owner of the piece at the given coordinate*)

let make_move (start_coordinate: coordinate) (end_coordinate: coordinate) = 
  failwith("Unimplemented")
(*[start_coordinate end_coordinate] takes the given input, creates object of type [move] 
  with given start coordinate, end coordinate*)

let get_piece (coordinate: coordinate) = failwith("Unimplemented")
(*Returns the type of piece at the given coordinate*)
let get_time_since_last_move = failwith("Unimplemented")
(*Gets the time elapsed since the last move in seconds*)
let get_time_since_start = failwith("Unimplemented")
(*Gets the time elapsed from game start to the given move, in seconds*)
let get_log = failwith("Unimplemented")
(*Returns a list of all of the moves taken in chronological ordering*)