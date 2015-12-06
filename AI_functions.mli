open Game_utils

(*Runs the AI's turn and makes decisions, returns a unit when AI finishes turn*)
val ai_decision: board -> int -> unit

(*AI determines which properties to upgrade with houses if it is allowed to*)
val upgrade_a_prop : board -> int -> int -> bool

(*Determines whether it should accept a trade*)
val accept_trade : board -> property list -> property list -> int -> int -> int -> int -> bool

(*Determines what property it wants and requests a trade*)
val trade_a_prop : board -> int -> unit
