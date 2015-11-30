open Player_functions
open Board_functions

let roll_die () =
  "ROLL"

let property_eval (b : board) (pl : player) (prp : property): string =
  if (get_money pl) > (get_prop_price prp) then
    "BUY " ^ get_prop_name