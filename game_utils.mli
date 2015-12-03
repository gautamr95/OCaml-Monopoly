open AI_functions

type community_chest
type chance
type board
type player
type property
type property_container
type tile


(*take in landed on property, list of players in the game, player making move,
  game board, and return the players updated state if the bought or not
  and the new game board*)
val prop_check : property -> player list -> player ->  board -> player * board

(*board is game state, give it player number, returns player, precondition is that
int is always less than or equal to number of players*)
val get_player : board -> int -> player

(*get list of players*)
val get_player_list : board -> player list

(*get property from name of it*)
val get_property : board -> string -> property option

(*get a players position*)
val get_pl_position : board -> int -> tile

(*get a ploperty position*)
val get_prop_position : board -> property -> tile


(*get a random chest card*)
val get_chest : board -> community_chest

(*get a random chance card*)
val get_chance : board -> chance

(*set a new player position*)
val move_player : board -> int -> int -> unit

(* Creates a board to be used in the beginning
  Inputs:
  - number of human players
  - list of names of the human players (ids will be generated in this order)
  -
*)
val create_board : int -> string array -> string list -> string -> board

(*takes in board and player and change in money*)
val change_money : board -> int ->  int -> unit

(* the player is an AI *)
val is_ai : player -> bool
(*take in board, property to move and id of player to get it*)
val move_property : board -> int -> property -> unit

val is_property : board -> string -> bool

val get_prop_price : property -> int

val get_prop_name : property -> string

(*take in player id*)
val in_jail : board -> int -> bool

val is_chance : board -> int -> bool

val is_community_chest : board -> position -> bool

(*take in player id*)
val get_player_property : board -> int -> property_container

(*get players money from id*)
val get_money : board -> int -> int

(*takes in position*)
val get_tile : board -> int -> tile

val is_bankrupt : board -> int -> bool

val others_bankrupt : board -> int -> bool

