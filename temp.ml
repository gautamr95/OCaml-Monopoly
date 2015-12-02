let rec main_game _ =
  let curr_player = get_turn game_state in
  let curr_pos = get_position game_state curr_player in
  
  let rec mini_repl _ =
    Printf.printf "You have the following options:
      Money: Displays how much money you currently have
      Property: Displays what properties you own
      House: Displays which of your properties have houses and how many
      Trade PROPERTY: Initiates a trade for specified PROPERTY
      Upgrade PROPERTY: Upgrades a property with additional houses, if possible
      End: Ends your turn";
      let command = read_line () in
      match command with
      | "Money" -> (*Show money*); mini_repl ()
      | "Property" -> (*Show properties*); mini_repl ()
      | "House" -> (*Show houses*); mini_repl ()
      | "Trade" -> (*Initiate trade*); mini_repl ()
      | "Upgrade" -> (*Upgrade property*) ; mini_repl ()
      | "End" ->
    (* Display options for trading and upgrading
     * exit on end turn*) in
  if (is_player game_state) then
    (*AI function goes here*)
    let (d1,d2) = roll_dice () in
    Printf.printf "AI rolled %i, %i." d1 d2;
    move_player game_state curr_player (d1 + d2);
    let ai_repl _ =
      match ai_decision_function with 
      | Trade i -> make_trade plist (get_player board i)
      | Upgrade prop -> (*upgrade prop*);
      | Buy prop -> (*buy prop*)
  else if (in_jail curr_player) then
    Printf.printf "You are in jail";
    let (d1,d2) = roll_dice () in
    Printf.printf "You rolled %i and %i." d1 d2;
    (if d1=d2 then
     Printf.printf "You rolled doubles!" ;
      move_player game_state curr_player (d1 + d2);
    else
      Printf.printf "Aw you didn't roll doubles. You are still in jail.";
      mini_repl ();)
  else
    let (d1,d2) = roll_dice () in
    Printf.printf "You rolled %i and %i." d1 d2;
    if(d1 + d2 + curr_pos.pos > board_size) then
      change_money game_state curr_player 30;
    else
      ();
    move_player game_state curr_player (d1 + d2);
    let new_pos = get_position game_state curr_player in
    if (is_property new_pos) then
      (* Insert
       * functions
       * for buying/trading
       *)
      mini_repl ();
    else if (is_chance new_pos) then
      let chance = get_chance in
      (*use chance card*)
      mini_repl ();
    else if (is_chest new_pos) then
      let chest = get_chest in
      (*use community chest card*)
      mini_repl ();
    else if (is_tax new_pos) then
      change_money game_state curr_player (-20)
      mini_repl ();
    else
      mini_repl ();



