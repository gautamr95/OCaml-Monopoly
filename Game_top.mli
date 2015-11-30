open Player_functions
open Board_functions
open AI_functions

type board
type player
type property

(*main repl*)
val main : board -> player -> player -> player -> player -> int -> unit

(*move player on board, modify players position and return a new player and
  return property landed on*)
val move : player -> board -> player * property

(*take in landed on property, list of players in the game, player making move,
  game board, and return the players updated state if the bought or not
  and the new game board*)
val prop_check : property -> player list -> player ->  board -> player * board

(*board is game state, give it player name, returns player*)
val get_player : board -> string -> player

(*get list of players*)
val get_player_list : board -> player list

(*get property from name of it*)
val get_property : board -> string -> property

(*get a players position*)
val get_position : board -> player -> property

(*get the player whos turn it is*)
val get_turn : board -> player

(*get a random chest card*)
val get_chest : board -> community_chest

(*get a random chance card*)
val get_chance : board -> chance

(*set a new player position*)
val move_player : board -> player -> int -> board

(*create board*)
val create_board : int -> string list -> string list -> string -> board

(*takes in board and player and change in money*)
val change_money : board -> player ->  int -> board

(*take in board, property name to move *)
val move_property : board -> player -> property -> board

(*contains player*)
val is_player : board -> string -> bool

val is_property : board -> string -> bool
