
type community_chest = string * int
type chance = string * int
type board
type player
type property
type property_container
type tile = Prop of property | Chance | Chest |Jail of int | Go | Go_jail
type color = Brown|Grey|Pink|Orange|Red|Yellow|Green|Blue
(*get list of players*)
val get_player_list : board -> player list

(*board is game state, give it player number, returns player, precondition is that
int is always less than or equal to number of players*)
val get_player : board -> int -> player

(*get property from name of it, none if it doesnt exist*)
val get_property_from_name : board -> string -> property option

(*takes in position and returns its tile*)
val get_tile : board -> int -> tile

(*get property from position, none if it isnt a property*)
val get_property : board -> int -> property option

(*get the integer position a player is at take in board and player id*)
val get_pl_position : board -> int -> int
(*
(*get the integer position a property is at take in board and property*)
val get_prop_position : property -> int
*)
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

(*get a property's price*)
val get_prop_price : property -> int

(*get a property's name*)
val get_prop_name : property -> string

(*take in player id return if the player is in jail, will change to something else tho*)
val in_jail : board -> int -> bool

(*returns if a position is chance tile*)
val is_chance : board -> int -> bool

(*returns if a position is a chest tile*)
val is_chest : board -> int -> bool

(*returns if a tile is jail*)
val is_go_jail : board -> int -> bool

(*take in player id*)
val get_player_property : board -> int -> property_container

(*determine total values of a player and their print_players_properties*)
val get_player_property_val : board -> int -> int

(*gets the color of a property and returns a the player with player ids
  list of properties with thatcolor*)
val get_pl_prop_of_color: board -> int -> property -> property list ref

(*get players money from id*)
val get_money : board -> int -> int

(*takes in player id and and return a boolean for if a player is bankrupt*)
val is_bankrupt : board -> int -> bool

(*returns if other players are all bankrupt*)
val others_bankrupt : board -> int -> bool

(*returns rent for a property*)
val get_rent : property -> int

(*returns holder of a property, in the form of a player id, if its not held
  return none*)
val get_holder : property -> int option

(*get the number of houses on a property*)
val get_houses : property -> int

(*based on position return the tile of that position*)
val get_tile : board -> int -> tile

(*move player to jail with a given player id, modify their position and set
  in_jail to true*)
val move_to_jail : board -> int -> unit

(*leave jail, reset player flag*)
val leave_jail : board -> int -> unit

(*print all of a players properties*)
val print_players_properties : board -> int -> unit

(*returns true if a player can buy a house for a specified property*)
val can_buy_house : board -> int -> property -> bool

(*increments the number of houses for that property, for player designated by
  player id and reduces their money by $50*)
val add_house : board -> int -> property -> unit

(*pos, color, cost, rent name*)
val create_property : int -> color -> int -> int -> string -> property


(* Creates a board to be used in the beginning
  Inputs:
  - number of human players
  - list of names of the human players (ids will be generated in this order)
  -

val create_board : int -> string array -> string list -> string -> board


*)
val create_player_list : bool list -> player list

val create_board : bool list -> community_chest list -> chance list ->
                   property list -> tile list -> board
