open AI_functions

type community_chest
type chance
type board
type player
type property
type property_container


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

(* Creates a board to be used in the beginning
  Inputs:
  - number of human players
  - list of names of the human players (ids will be generated in this order)
  -
*)
val create_board : int -> string array -> string list -> string -> board

(*takes in board and player and change in money*)
val change_money : board -> player ->  int -> board

(*take in board, property name to move *)
val move_property : board -> player -> property -> unit

val is_property : board -> int -> bool

val get_prop_price : property -> int

val get_prop_name : property -> string

val in_jail : player -> bool