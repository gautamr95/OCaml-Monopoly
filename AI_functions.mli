open Player_functions
open Board_functions


(*generates string to roll dice, called by top level if player is ai*)
val roll_die : unit -> string

(*take in property that ai is on, take in list of properties from game board,
  take list of players in the game, take in you player info structure
  determine if property should be bought*)
val property_eval : property -> property list -> player list -> player -> string

(*determine if you want to make a trade based on other players property and
  your own players material*)
val make_trade : player list -> player -> string

(*if you recieve *)
val receive_trade : int -> property -> string