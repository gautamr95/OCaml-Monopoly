open AI_functions
open Board_functions

type player


(*take in property that player is currently on, take in list of owned properties,
  take in player info structure.
  Buys property if not taken and player has enough money, subtracts price from
  player's current savings*)

(*requests a trade with the selected player*)

(*accepts an incoming trade request from a player*)

(*declines an incoming trade request from a player*)

(*upgrades a property owned by a player if they have enough money
  and if the property can be upgraded*)

(*ends a players turn and returns the updated player state*)


(*quits the game*)

(*gets player money*)
val get_money : player -> int

(*gets owned properties*)
val get_pl_property : player -> property list

(*gets current location*)
val get_location : player -> property

val set_money : player -> int -> player

val has_property : player -> property -> bool

val set_property : player -> property -> player

