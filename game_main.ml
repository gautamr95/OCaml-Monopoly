open Str
open Game_utils
open AI_functions
open Board_gen
open Async.Std
open Trading

(* Game constants *)
let total_players = 4
let jail_fee = 30
let tot_rounds = 10000
let house_cost = 50

(* Embed everything in a function to be run through the GUI. *)
let run_game_main () =

(* Mutex based function *)

let wait_lock = ref (Mutex.create ()) in
let cmd_input_str = ref "" in

let get_input () : string=
  Mutex.unlock (!wait_lock);
  Gui.readline wait_lock cmd_input_str;
  Mutex.lock (!wait_lock);
  Mutex.unlock (!wait_lock);
  !cmd_input_str in

(* To create random seed. *)
let _ = Random.self_init () in

let property_list = Board_gen.create_prop_list () in
let tile_list = Board_gen.create_tile_list property_list in
let chance_list = Board_gen.create_community_chest_list() in
let community_chest_list =  Board_gen.create_chance_list() in

(*      end         *)

(* Give introductory message, need to press enter to continue *)
let _ = (Gui.print_to_cmd "\n\n\nWelcome to OCaml Monopoly! This game has been developed by
  \nSacheth Hegde\nGautam Ramaswamy\nGaurab Bhattacharya\nTian Yao\n\nPlease press enter to start: ") in

let _ = get_input () in

(* Function that asks for correct values (confirmation), and returns
a boolean based on the user input.
  Input - Unit
  Output - bool of whether or not the input was valid *)
let is_correct (  ) : bool =
  Gui.print_to_cmd "Is this value correct? (y/n) -> ";
  (* Checks for any input errors *)
  let correct = try Some (get_input ()) with
    | Failure s -> None in

  let correction = match correct with
  | None -> false
  | Some a ->
    if a = "y" || a = "Y" then true else false in
  correction in

(* Gets total players through user input
  Input: Unit
  Output: Number of players determined by user input. *)
let rec get_players () : int =

  let rec get_players_prompt () =
    Gui.print_to_cmd "\nPlease enter the number of human players (1-4) -> ";
    try (int_of_string (get_input ())) with
    | Failure s -> get_players_prompt () in

  let num_players = get_players_prompt () in

  if 0 <= num_players && num_players <= 4 && (is_correct ()) then num_players else get_players () in

let num_players = get_players () in

let rec make_ai_list acc human_num init =
  if init = 5 then acc
  else if human_num > 0 then
    (make_ai_list (acc@[false]) (human_num-1) (init+1))
  else (make_ai_list (acc@[true]) (human_num) (init+1)) in

let is_ai_list = make_ai_list [] num_players 1 in

let game_board = create_board is_ai_list community_chest_list chance_list
                          property_list tile_list in
Gui.updateboard game_board;

(* total turns (for each round)*)
let rounds = ref 0 in

(* Used to calculate the accumulated turns, within a round *)
let turns = ref 0 in

(* Helper function that will prompt the player if they want to buy a certain property.
   Inputs:
   p_id - int of the player ID
   p_position - int of the player's position on the board.
   Output: bool of whether a transaction occurred or not*)
let property_prompt p_id p_position  =
  let prop_opt = get_property game_board p_position in
  match prop_opt with
  | None -> false
  | Some prop ->
    (let prop_name = get_prop_name prop in
    let prop_price = get_prop_price prop in
    (Gui.print_to_cmd (Printf.sprintf "\nWould you like to purchase %s, for a cost of %d? (y/n) -> " prop_name prop_price));
    let answer = get_input () in
    match String.lowercase (answer) with
    | "y" ->
      (let tot_money = get_money game_board p_id in
      if prop_price > tot_money  then
        ((Gui.print_to_cmd "\nError. You do not have enough money for this transaction.");
        false)
      else
        let _ = (Gui.print_to_cmd (Printf.sprintf "\nYou have bought the property, %s!\n" prop_name)) in
        let _ = (move_property game_board p_id None prop) in
        let _ = (change_money game_board p_id (-1 * prop_price)) in
        true)
    | "n" -> false
    | _ -> Gui.print_to_cmd "\nInvalid command."; false
    ) in

(* Helper REPL function for purchasing a house on a property. *)
let rec buy_house p_id =
  (Gui.print_to_cmd "\nChoose from the following options:
    Upgrade - Options to buy a house
    Properties - View your properties
    Quit - Go back to the main game options");
  let command = get_input () in
  match String.lowercase (command) with
  | "upgrade" ->
    (Gui.print_to_cmd "\nPlease enter the name of the property you would like to buy a house for -> ");
    let house_prop = get_input () in
    let prop_obj_option = get_property_from_name game_board house_prop in
    begin match prop_obj_option with
    | None ->
      Gui.print_to_cmd "\nInvalid move. The property doesn't exist.";
      buy_house p_id
    | Some prop_obj ->
      begin match can_buy_house game_board p_id prop_obj with
      | true ->
        if (get_money game_board p_id >= house_cost) then
          (Gui.print_to_cmd "\nYou have bought a house for your property!";
          add_house game_board p_id prop_obj;
          buy_house p_id)
        else
          (Gui.print_to_cmd "\nYou do not have enough money to buy this house.";
          buy_house p_id)
      | false -> ( Gui.print_to_cmd "\nInvalid move. You can not buy the house because you either do not own the property or you don't have a monopoly.");
        buy_house p_id
      end
    end
  | "properties" -> Gui.print_to_cmd (print_players_properties game_board p_id); buy_house p_id
  | "quit" -> ()
  | _ -> (Gui.print_to_cmd "\nInvalid command."; buy_house p_id) in

(* Loop through game states, and update game state. This loop is taken for
each player that plays the game. *)
let rec game_loop () =
  Gui.updateboard game_board;
  turns := !turns + 1;
  let _ = if !turns > 4 then (turns := 1; rounds := !rounds + 1) else () in
  if !rounds >= tot_rounds then ()
  else let curr_player_id = !turns - 1 in
  if others_bankrupt game_board curr_player_id then ()
  else if is_bankrupt game_board curr_player_id then
      (Gui.print_to_cmd (Printf.sprintf "\nPlayer %d, you are bankrupt, so your turn will be skipped.\n" curr_player_id))
  else if is_ai game_board curr_player_id then
      (ai_decision game_board curr_player_id; game_loop ())
  else
    (* REPL for the individual players and the actions they can perform. *)
    let _ = Gui.print_to_cmd "__________________________________________________________\n" in
    let _ = Gui.print_to_cmd "__________________________________________________________" in
    let _ = (Gui.print_to_cmd (Printf.sprintf "\nPlayer %d, it is your turn.\nPress enter to roll the dice -> " curr_player_id)) in
    let _ = get_input () in

    let (d1, d2) = roll_dice () in
    (Gui.print_to_cmd (Printf.sprintf "\nYou have rolled a %d and %d, with a total move of %d.\n" d1 d2 (d1+d2)));

    let prompt_buy_property = ref false in
    let bought_property     = ref false in

    let old_position = (get_pl_position game_board curr_player_id) in

    move_player game_board curr_player_id (d1+d2);

    let player_position = (get_pl_position game_board curr_player_id) in

    let _ = if (old_position > player_position)
      then Gui.print_to_cmd ("\nYou get $200 for passing go!") else () in

    Gui.updateboard game_board;

    let _ =
    if in_jail game_board curr_player_id then
      (Gui.print_to_cmd "\n\nYou were also in jail.";
      move_to_jail game_board curr_player_id;
      if (d1=d2) then
        (Gui.print_to_cmd "\nSince you rolled a double, though, you can move out of jail at no cost!\n")
      else
        ((Gui.print_to_cmd (Printf.sprintf "\nYou did not roll a double, though, so you will lose $%d and move out of jail.\n" jail_fee));
        change_money game_board curr_player_id (-1 * jail_fee)))
    else () in


    let prop_option = (get_property game_board player_position) in

    let _ = match prop_option with
    | None ->
      (if is_chance game_board player_position then
        let (message, money_change, other_money_change) = get_chance game_board in
        ((Gui.print_to_cmd (Printf.sprintf "\n---------------------------\nYou got a chance card!\n%s\n---------------------------\n" (message)));
        change_money game_board curr_player_id money_change;
        change_others_money game_board curr_player_id other_money_change)
        (*move_to_position game_board curr_player_id move_space*)
      else if is_chest game_board player_position then
        let (message, money_change, other_money_change) = get_chest game_board in
        ((Gui.print_to_cmd (Printf.sprintf "\n---------------------------\nYou got a community chest card!\n%s\n---------------------------\n" (message)));
        change_money game_board curr_player_id (money_change));
        change_others_money game_board curr_player_id other_money_change
        (*move_to_position game_board curr_player_id move_space*)
      else if is_go_jail game_board player_position then
        ((Gui.print_to_cmd "\n---------------------------\nYou are going to jail :(\n---------------------------\n");
        move_to_jail game_board curr_player_id) else ())
    | Some prop ->
      let holder = get_holder prop in
      (match holder with
      | None -> (* No one is holding the current property. *)
        (* They will have an extra prompt to buy the property *)
        if (not !bought_property) then (prompt_buy_property := true) else ()
      | Some p_id -> (* id of player holding the property. *)
        (* They will have a prompt that they lost money. *)
        let num_houses = get_houses prop in
        if p_id = curr_player_id then ()
        else
          let rent_amt = get_rent prop in

          let rent_multiplier = match num_houses with
          | 1 -> 5 | 2 -> 15 | 3 -> 45 | 4 -> 60 | _ -> 1 in

          let pay_amt = rent_amt * rent_multiplier in
          ((Gui.print_to_cmd (Printf.sprintf "\n---------------------------\nYou have landed on player %d's property, and will pay a rent of %d.\n---------------------------\n" p_id pay_amt));
          change_money game_board curr_player_id (-1 * pay_amt);
          change_money game_board p_id (pay_amt))) in

    Gui.updateboard game_board;

    let rec mini_repl () =
      Gui.updateboard game_board;

      (* First print the relevant options. *)
      let _ = Gui.print_to_cmd "\nYou have the following options:\n
        Money - Displays how much money you currently have
        Property - Displays what properties you own
        Position - Displays your numeric position on the board
        Trade - Initiates a trade, if possible
        House - Options for buying houses for a property
        Done - End turn" in

      let _ = if !prompt_buy_property then
        (Gui.print_to_cmd
        "\n\tBuy - Options for buying the current property")
      else () in

      Gui.print_to_cmd "\n\nCommand -> ";
      let command = get_input () in

      let _ = match String.lowercase command with
      | "money" ->
        (Gui.print_to_cmd (Printf.sprintf "\n---------------------------\nYou have $%d.\n---------------------------" (get_money game_board curr_player_id));
        mini_repl ())
      | "property" ->
        (Gui.print_to_cmd (print_players_properties game_board curr_player_id);
        mini_repl ())
      | "position" -> ((Gui.print_to_cmd (Printf.sprintf "\n---------------------------\nYou are currently on position %d.\n---------------------------" player_position)); mini_repl ())
      | "trade" -> (trade_prompt game_board curr_player_id; mini_repl ())
      | "house" -> (buy_house curr_player_id; mini_repl ()); Gui.updateboard game_board
      | "done" -> ()
      | "buy" -> (* Buying a new property. *)
        if not !prompt_buy_property then ((Gui.print_to_cmd "\n---------------------------\nInvalid command.\n---------------------------\n"); mini_repl ())
        else
          let transaction = property_prompt curr_player_id player_position in
          let _ = if transaction then (prompt_buy_property := false; bought_property := true) else () in
          mini_repl ()
      | _ -> ((Gui.print_to_cmd "\n---------------------------\nInvalid command.\n---------------------------"); mini_repl ()) in
      Gui.updateboard game_board in

    let _ = (mini_repl ()) in
  game_loop () in

let _ = game_loop () in

(* Determine end state of game. *)

(* We know that the game has ended, so there is a winner or winner(s).
Either 3 of the 4 players became bankrupt or the total rounds were finished. *)

(* Maximum value is determined by money held + value in property *)
let winner_id = ref (0) in
let winner_value = ref min_int in

(* Loop through players and determine how much value they have *)
let rec determine_winner id =
  if id >= 4 then () else
  let held_money = get_money game_board id in
  let total_property_value = get_player_property_val game_board id in
  let tot_value = held_money + total_property_value in
  let _ = if (tot_value > !winner_value) then
    (winner_id := id; winner_value := tot_value)
  else () in determine_winner (id+1) in

let _ = determine_winner 0 in

Gui.print_to_cmd (Printf.sprintf "\nCongratulations player %d! You have won the game with a total evaluation value of %d!\n" !winner_id !winner_value)

let _ = Gui.print_to_cmd "\n\nThe game is finished! Thanks for playing!\n\n" in ()

(* Done with game script. *)