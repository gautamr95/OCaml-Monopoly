open Game_utils
open Trade_offer

let accept_trade b req off rm om pl tp =
  let get_cost_from_list = List.fold_left (fun a x -> a + get_prop_price x) 0 in
  let off_total = (get_cost_from_list off) + om in
  let req_total = (get_cost_from_list req) + rm in
  let gain_money = off_total > req_total in
  let my_prop = List.fold_left (fun a x -> !(get_pl_prop_of_color b tp x)::a) []  in
  let my_props_off = my_prop off in
  let need_prop =  List.fold_left (fun a x -> a || x <> []) false my_props_off in
  let my_money = get_money b tp in
  let can_afford = my_money > rm in
  let my_props_req = my_prop req in
  let own_triple = List.fold_left (fun a x -> a || (List.length x = 3)) false my_props_req in
  let _ = Gui.print_to_cmd (Printf.sprintf 
  "Gain Money : %b
  Own triple : %b
  Need prop : %b\n" gain_money own_triple need_prop ) in
  let trade =
    if not can_afford then false
    else if own_triple then false
    else if need_prop then true
    else if gain_money then true
    else false in
  trade

let upgrade_a_prop b pl up =
  if (up > 2) then false else(
  let brown_prop = !(get_pl_prop_from_color b pl Brown) in
  let grey_prop = !(get_pl_prop_from_color b pl Grey) in
  let pink_prop = !(get_pl_prop_from_color b pl Pink) in
  let orange_prop = !(get_pl_prop_from_color b pl Orange) in
  let red_prop = !(get_pl_prop_from_color b pl Red) in
  let yellow_prop = !(get_pl_prop_from_color b pl Yellow) in
  let green_prop = !(get_pl_prop_from_color b pl Green) in
  let blue_prop = !(get_pl_prop_from_color b pl Blue) in
  let can_buy_houses = List.fold_left (fun a x -> a || (can_buy_house b pl x)) false in
  let rec house_to_buy plst =
    match plst with
    | [] -> ()
    | h::t ->
        if (can_buy_house b pl h) then (
          Gui.print_to_cmd (Printf.sprintf "Player %i bought a house for %s\n" 
          pl (get_prop_name h));
          add_house b pl h)
        else house_to_buy t in
  if (can_buy_houses brown_prop) then
   (house_to_buy brown_prop; true)
  else if (can_buy_houses grey_prop) then
    (house_to_buy brown_prop; true)
  else if (can_buy_houses pink_prop) then
    (house_to_buy pink_prop; true)
  else if (can_buy_houses orange_prop) then
    (house_to_buy orange_prop; true)
  else if (can_buy_houses red_prop) then
    (house_to_buy red_prop; true)
  else if (can_buy_houses yellow_prop) then
    (house_to_buy yellow_prop; true)
  else if (can_buy_houses green_prop) then
    (house_to_buy green_prop; true)
  else if (can_buy_houses blue_prop ) then
    (house_to_buy blue_prop; true)
  else false)

let trade_a_prop b pl =
  let brown_prop = !(get_pl_prop_from_color b pl Brown) in
  let grey_prop = !(get_pl_prop_from_color b pl Grey) in
  let pink_prop = !(get_pl_prop_from_color b pl Pink) in
  let orange_prop = !(get_pl_prop_from_color b pl Orange) in
  let red_prop = !(get_pl_prop_from_color b pl Red) in
  let yellow_prop = !(get_pl_prop_from_color b pl Yellow) in
  let green_prop = !(get_pl_prop_from_color b pl Green) in
  let blue_prop = !(get_pl_prop_from_color b pl Blue) in
  let want_to_trade lst = List.length lst = 2 in
  let rec prop_to_req plst  =
    match plst with
    | [] -> ()
    | h::t ->(
      match get_holder h with
      | None -> prop_to_req t
      | Some player ->
          let cost = get_prop_price h in
          let num_houses = get_houses h in
          let offer = int_of_float (0.75 *. (float_of_int cost)) in
          let can_afford = (get_money b pl) > offer in
          let trade_fn = if (is_ai b player) then accept_trade b else trade_offer in
          let will_trade = if (can_afford && num_houses = 0) then
            (trade_fn [h] [] 0 offer pl player)
          else false in
          if will_trade then(
            let _ = move_property b player (Some pl) h in
            let _ = change_money b pl (-offer) in
            let _ = change_money b player (offer) in
           Gui.print_to_cmd"Trade accepted\n")
          else Gui.print_to_cmd "Trade denied \n") in
  let can_trade = ref true in
  if( !can_trade && want_to_trade brown_prop) then (
    can_trade := false;
    prop_to_req brown_prop)
  else if (!can_trade && want_to_trade grey_prop) then (
    can_trade := false;
    prop_to_req grey_prop)
  else if (!can_trade && want_to_trade pink_prop) then (
    can_trade :=false;
    prop_to_req pink_prop)
  else if (!can_trade && want_to_trade orange_prop) then (
    can_trade := false;
    prop_to_req orange_prop)
  else if (!can_trade && want_to_trade red_prop) then (
    can_trade :=false;
    prop_to_req red_prop )
  else if (!can_trade && want_to_trade yellow_prop) then (
    can_trade := false;
    prop_to_req yellow_prop )
  else if (!can_trade && want_to_trade green_prop) then (
    can_trade := false;
    prop_to_req green_prop )
  else if (!can_trade && want_to_trade blue_prop) then (
    can_trade := false;
    prop_to_req blue_prop)
  else
    can_trade := false


let ai_decision (b : board) ( pl : int ) : unit =
  let rolled = ref false in
  let upgraded = ref 0 in
  let traded = ref false in
  let curr_pos = ref (get_pl_position b pl) in
  let rec inner_repl _ =
    if not !rolled then
      (let (d1,d2) = roll_dice () in
      let _ = Gui.print_to_cmd (Printf.sprintf "\nPlayer %i has rolled a %i and %i, with a total move of %i.\n"
                            pl d1 d2 (d1 + d2)) in
      let _ = if (not (in_jail b pl)) then
      move_player b pl (d1+d2)
      else () in
      let new_pos = get_pl_position b pl in
      let _ = if new_pos < (!curr_pos) then
        Gui.print_to_cmd (Printf.sprintf "Player %i collected $200 for passing GO!\n" pl)
              else () in
      curr_pos := new_pos;
      rolled := true;
      let tile = (get_tile b (!curr_pos)) in
      (match tile with
      | Prop property ->
          let name = get_prop_name property in
          let _ = Gui.print_to_cmd (Printf.sprintf "Player %i landed on %s.\n" pl name) in
          (match (get_holder property) with
          | None ->
              let my_money = get_money b pl in
              let price = get_prop_price property in
              if my_money > price then
                let _ = Gui.print_to_cmd (Printf.sprintf "Player %i bought %s for $%i\n" pl name price) in
               (move_property b pl None property)
              else
                ()
          | Some hl ->
              let rent = get_rent property in
              let num_houses = get_houses property in
              let updated_rent = match num_houses with
              | 1 -> rent * 5
              | 2 -> rent * 15
              | 3 -> rent * 45
              | 4 -> rent * 60
              | _ -> rent in
              let _ = Gui.print_to_cmd (Printf.sprintf "Player %i paid Player %i $%i in rent for %s\n"
                pl hl updated_rent name) in
              (change_money b hl updated_rent);
              (change_money b pl (-updated_rent)))

      | Chance ->
          let (s,mm,tm) = get_chance b in
          let _ = Gui.print_to_cmd (Printf.sprintf "\n---------------------------Player %i landed on Chance!\n%s\n--------------------------\n" pl s) in
          let _ = Gui.print_to_cmd (Printf.sprintf "Player %i gets %i \nAll other Players get %i\n" pl mm tm) in
          change_money b pl mm;
          change_others_money b pl tm
      | Chest ->
          let (s,mm,tm) = get_chest b in
          let _ = Gui.print_to_cmd (Printf.sprintf "\n---------------------------Player %i landed on Community Chest!\n%s\n--------------------------\n" pl s) in
          let _ = Gui.print_to_cmd (Printf.sprintf "Player %i gets %i \nAll other Players get %i\n" pl mm tm) in
          change_money b pl mm;
          change_others_money b pl tm
      | Jail _ -> 
          let is_in_jail = in_jail b pl in
          let _ = if (is_in_jail) then 
            let _ = move_player b pl (d1 + d2) in
            let _ = leave_jail b pl in
            let _ = Gui.print_to_cmd (Printf.sprintf "Player %i is in jail!\n" pl) in
            if(d1=d2) then 
              Gui.print_to_cmd (Printf.sprintf "Player %i rolled doubles!\n" pl)
            else (
              Gui.print_to_cmd (Printf.sprintf "Player %i didn't roll doubles. He will lose $30.\n" pl);
              change_money b pl (-30))
          else 
            Gui.print_to_cmd (Printf.sprintf "Player %i is just visiting jail.\n" pl) in ()
      | Go -> ()
      | Go_jail  -> Gui.print_to_cmd (Printf.sprintf "Player %i is going to jail!\n" pl);move_to_jail b pl); inner_repl ())
    else (
      if (upgrade_a_prop b pl (!upgraded) ) then (upgraded := !upgraded + 1;
              inner_repl ())
      else if (not (!traded)) then
        let _ = trade_a_prop b pl in
        traded:=true;
        inner_repl ()
      else ()) in
  inner_repl ()

        (*Search through own props for if own two, if do then
          * offer trade with person who owns it
          * offer 75% of cost
          * maybe if reject trade next round offer more/ include diff prop
          * *)


