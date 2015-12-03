open AI_functions

type community_chest
type chance
type board
type player
type property
type property_container
type tile



(*board is game state, give it player number, returns player, precondition is that
int is always less than or equal to number of players*)
val get_player : board -> int -> player

(*get list of players*)
val get_player_list : board -> player list

(*get property from name of it, none if it doesnt exist*)
val get_property_from_name : board -> string -> property option

(*get property from position, none if it isnt a property*)
val get_property : board -> int -> property option

(*get the integer position a player is at take in board and player id*)
val get_pl_position : board -> int -> int

(*get the integer position a property is at take in board and property*)
val get_prop_position : board -> property -> int

(*get a random chest card*)
val get_chest : board -> community_chest

(*get a random chance card*)
val get_chance : board -> chance

(*set a new player position, thake in board, player id and how many spaces to move*)
val move_player : board -> int -> int -> unit

(*takes in board and player and change in money*)
val change_money : board -> int ->  int -> unit

(* the player is an AI *)
val is_ai : board -> int -> bool

(*take in board,  and id of player to get it, and id of player to lose it*)
val move_property : board -> int -> int option -> property -> unit

val get_prop_price : property -> int

val get_prop_name : property -> string

(*take in player id return if the player is in jail, will change to something else tho*)
val in_jail : board -> int -> bool

val is_chance : board -> int -> bool

val is_chest : board -> int -> bool

val is_go_jail : board -> int -> bool

(*take in player id*)
val get_player_property : board -> int -> property_container

(*get players money from id*)
val get_money : board -> int -> int

(*takes in position*)
val get_tile : board -> int -> tile

val is_bankrupt : board -> int -> bool

val others_bankrupt : board -> int -> bool

val get_pl_prop_of_color: board -> int -> property -> property list ref

val get_rent : property -> int

val get_holder : property -> int option

val get_houses : property -> int

val get_tile : board -> int -> tile
(* Creates a board to be used in the beginning
  Inputs:
  - number of human players
  - list of names of the human players (ids will be generated in this order)
  -
*)
val create_board : int -> string array -> string list -> string -> board

val move_to_jail : board -> int -> unit