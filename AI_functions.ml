open Player_functions
open Board_functions

type decicion = 
  | Trade of property * property
  | Upgrade of property
  | Buy of property
  | End
 
let property_eval (b : board) (pl : player) (prp : property): string =
  if (get_money pl) > (get_prop_price prp) then
    "BUY"
  else
    "NO"

let ai_decision (b : board) (pl : player) (prop : property) : decision =
  if (get_holder prop = None && (get_money pl) > (get_prop_price prop )) then
    Buy prop
  else if (upgrade_prop_dec b pl) then
    let uprop = choose_upgrade pl in
    Upgrade uprop
  else if (trade_prop_dec b pl) then
    let (myprop,tprop) = choose_trade pl in
    Trade (myprop, tprop)
  else 
    End

let upgrade_prop_dec b pl =
  let my_props = get_my_properties b pl in
  let helper lst = 
    match lst with
    | [] -> false
    | h::t -> 
        let pcolor = get_color h in
        let possible =  (List.filter (fun x -> get_color x = pcolor)) in
        if List.length possible > 0 then 
          Trade (my_prop, List.hd possible)
