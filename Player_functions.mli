open AI_functions
open Board_functions

type player


(*generates string to roll dice, called by player in top level*)
val roll_dice : unit -> string

(*take in property that player is currently on, take in list of owned properties,
  take in player info structure.
  Buys property if not taken and player has enough money, subtracts price from
  player's current savings*)
val buy_property : property -> property list -> player -> string

(*requests a trade with the selected player*)
val request_trade : player -> player -> string

(*accepts an incoming trade request from a player*)
val accept_trade : player -> player -> string

(*declines an incoming trade request from a player*)
val decline_trade : player -> player -> string

(*upgrades a property owned by a player if they have enough money
  and if the property can be upgraded*)
val upgrade_property : property -> player -> string

(*ends a players turn and returns the updated player state*)
val end_turn : unit -> player

(*quits the game*)
val quit : unit -> string

(*gets player money*)
val savings : player -> string

(*gets turns*)
val turns : player -> string

(*gets scores*)
val scores : player -> string

(*gets owned properties*)
val get_properties : player -> string

(*gets current location*)
val get_location : player -> string
