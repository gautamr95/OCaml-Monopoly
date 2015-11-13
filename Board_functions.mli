open Player_functions

type board
type property
type community_chest
type chance
type tile

(* buy_property [s l] removes the property with name s from the available
properties list ref, so it can not be accessed by other players *)
val buy_property : string -> (property list) ref -> unit

(* sell_property [s l] add the property with name s from the available
properties list ref, so it can be accessed by other players *)
val sell_property : string -> (property list) ref -> unit

(* get_chance_card [a] returns the chance card at the top of the list
and modifies the list so the drawn card moves to the bottom of the deck. *)
val get_chance_card : (chance list) ref -> chance

(* get_community_chest [a] returns the community chest card at the top of 
the list and modifies the list so the drawn card moves to the bottom of the 
deck. *)
val get_community_chest : (community_chest list) ref -> community_chest

(* get_tile [a i] returns the tile object at index i *)
val get_tile : (tile list) -> int -> tile
