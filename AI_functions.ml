open Game_utils
 
(*let upgrade_prop_dec b pl =
  let my_props = get_player_property b pl in
  let color = *)


let ai_decision (b : board) ( pl : int ) : unit =
  let rolled = ref false in
  let curr_pos = ref (get_pl_position b pl) in
  let rec inner_repl _ = 
    if not !rolled then
      (let (d1,d2) = roll_dice () in
      let _ = Printf.printf "\nPlayer %i has rolled a %i and %i, with a total move of %i.\n" 
                            pl d1 d2 (d1 + d2) in
      move_player b pl (d1+d2);
      curr_pos := get_pl_position b pl;
      rolled := true;
      let tile = (get_tile b (!curr_pos)) in
      match tile with
      | Prop property -> 
          let name = get_prop_name property in
          let _ = Printf.printf "Player %i landed on %s.\n" pl name in
          (match (get_holder property) with
          | None -> 
              let my_money = get_money b pl in 
              let price = get_prop_price property in
              if my_money > price then
                let _ = Printf.printf "Player %i bought %s for %i\n" pl name price in
               (move_property b pl None property) 
              else 
                ()
          | Some hl ->  
              let rent = get_rent property in
              let _ = Printf.printf "Player %i paid Player %i %i in rent for %s\n"
                pl hl rent name in
              (change_money b hl rent);
              (change_money b pl (-rent)))

      | Chance -> 
          let (s,i) = get_chance b in
          let _ = Printf.printf "Player %i landed on Chance!\n
          %s\n" pl s in
          change_money b pl i
      | Chest -> 
          let (s,i) = get_chest b in 
          let _ = Printf.printf "Player %i landed on Community Chest!\n
          %s\n" pl s in
          change_money b pl i
      | Jail _ -> ()
      | Go -> ()
      | Go_jail  -> move_to_jail b pl)
    else () in
  inner_repl ()

        (*Search through own props for if own two, if do then 
          * offer trade with person who owns it
          * offer 75% of cost 
          * maybe if reject trade next round offer more/ include diff prop
          * *)
        

let accept_trade b req off rm om pl tp =
  let gain_money = om - rm > 0 in
  let gain_prop_cost= (get_prop_price off) - (get_prop_price req) > 0 in 
  let my_prop_off = !(get_pl_prop_of_color b tp off) in
  let need_prop =  my_prop_off <> [] in
  let my_money = get_money b tp in
  let can_afford = my_money > rm in
  let my_prop_req = !(get_pl_prop_of_color b tp req) in
  let own_triple = List.length my_prop_req = 3 in
  let trade = 
    if not can_afford then false
    else if own_triple then false
    else if need_prop then true
    else if gain_money && gain_prop_cost then true
    else false in
  if trade then "y" else "n"

