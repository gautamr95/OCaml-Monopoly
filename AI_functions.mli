open Game_utils

(*Runs the AI's turn and makes decisions, returns a unit when AI finishes turn*)
val ai_decision: board -> int -> unit

(*AI determines which properties to upgrade with houses if it is allowed to*)
val upgrade_a_prop : board -> int -> bool

(*take in property that ai is on, take in list of properties from game board,
  take list of players in the game, take in you player info structure
  determine if property should be bought
val property_eval : property -> property list -> player list -> player -> string

(*determine if you want to make a trade based on other players property and
  your own players material*)
val make_trade : player list -> player -> string

(*if you recieve a trade, take in the property that is offered and the price
  and determine if you will buy*)
val receive_trade : int -> property -> string*)
val accept_trade : board -> property list -> property list -> int -> int -> int -> int -> string