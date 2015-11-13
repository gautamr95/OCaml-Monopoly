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

(*Requests a trade with the selected player*)
val request_trade : player -> player -> string

(*Accepts an incoming trade request from a player*)
val accept_trade : player -> player -> string

(*Declines an incoming trade request from a player*)
val decline_trade : player -> player -> string

(*Upgrades a property owned by a player if they have enough money
  and if the property can be upgraded*)
val upgrade_property : property -> player -> string

(*ends a players turn and returns the updated player state*)
val end_turn : unit -> player
