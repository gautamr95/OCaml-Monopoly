open Str
open Game_utils
open AI_functions

(* Game constants *)
let total_players = 4
let jail_fee = 30
let tot_rounds = 50
let house_cost = 50

(* To create random seed. *)
let _ = Random.self_init ()

(* TODO Modify LATER *)
let create_prop_list () =
    (create_property 1 Brown 300 20 "baltic") ::
    (create_property 3 Brown 300 20 "blah") ::
    (create_property 4 Brown 300 20 "boom") ::
    (create_property 5 Green 300 20 "park") ::
    (create_property 7 Green 300 20 "atlantic") ::
    (create_property 8 Green 300 20 "pacific") :: []

let create_tile_list prop_lst =
  Go :: Prop(List.nth prop_lst 0) :: Chance :: Prop(List.nth prop_lst 1) ::
  Prop(List.nth prop_lst 2) :: Prop(List.nth prop_lst 3) :: Chest
  :: Prop(List.nth prop_lst 4) :: Prop(List.nth prop_lst 5) :: Jail(9) ::
  Go_jail :: Chest :: Chest :: Chance :: []
let create_chance_list () =
  [("boo", -50) ; ("shoo",30)]

let create_community_chest_list () =
  [("foo", 100) ; ("dog",-300)]

let property_list = create_prop_list ()
let tile_list = create_tile_list property_list
let chance_list = create_community_chest_list()
let community_chest_list =  create_chance_list()
(*      end         *)

(* Give introductory message, need to press enter to continue *)
let _ = (Printf.printf "\n\n\nWelcome to OCaml Monopoly! This game has been developed by
  \nSacheth Hegde\nGautam Ramaswamy\nGaurab Bhattacharya\nTian Yao\n\nPlease press enter to start: ")

let _ = Pervasives.read_line ()

(* Function that asks for correct values (confirmation), and returns
a boolean based on the user input.
  Input - Unit
  Output - bool of whether or not the input was valid *)
let is_correct (  ) : bool =
  Printf.printf "Is this value correct? (y/n) -> ";
  (* Checks for any input errors *)
  let correct = try Some (Pervasives.read_line ()) with
    | Failure s -> None in

  let correction = match correct with
  | None -> false
  | Some a ->
    if a = "y" || a = "Y" then true else false in
  correction

(* Gets total players through user input
  Input: Unit
  Output: Number of players determined by user input. *)
let rec get_players () : int =

  let rec get_players_prompt () =
    Printf.printf "\nPlease enter the number of human players (1-4) -> ";
    try (int_of_string (Pervasives.read_line ())) with
    | Failure s -> get_players_prompt () in

  let num_players = get_players_prompt () in

  if 1 <= num_players && num_players <= 4 && (is_correct ()) then num_players else get_players ()

let num_players = get_players ()

let rec make_ai_list acc human_num init =
  if init = 5 then acc
  else if human_num > 0 then
    (make_ai_list (acc@[false]) (human_num-1) (init+1))
  else (make_ai_list (acc@[true]) (human_num) (init+1))

let is_ai_list = make_ai_list [] num_players 1

let game_board = create_board is_ai_list community_chest_list chance_list
                          property_list tile_list

(* total turns (for each round)*)
let rounds = ref 0

(* Used to calculate the accumulated turns, within a round *)
let turns = ref 0

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
    (Printf.printf "\nWould you like to purchase %s, for a cost of %d? (y/n) -> " prop_name prop_price);
    let answer = Pervasives.read_line () in
    match String.lowercase (answer) with
    | "y" ->
      (let tot_money = get_money game_board p_id in
      if prop_price > tot_money  then
        ((Printf.printf "\nError. You do not have enough money for this transaction.");
        false)
      else
        let _ = (Printf.printf "\nYou have bought the property, %s!\n" prop_name) in
        let _ = (move_property game_board p_id None prop) in
        let _ = (change_money game_board p_id (-1 * prop_price)) in
        true)
    | "n" -> false
    | _ -> Printf.printf "\nInvalid command."; false
    )

(* Helper REPL function for purchasing a house on a property. *)
let rec buy_house p_id =
  (Printf.printf "\nChoose from the following options:
    Upgrade - Options to buy a house
    Properties - View your properties
    Quit - Go back to the main game options");
  let command = Pervasives.read_line () in
  match String.lowercase (command) with
  | "upgrade" ->
    (Printf.printf "\nPlease enter the name of the property you would like to buy a house for -> ");
    let house_prop = Pervasives.read_line () in
    let prop_obj = get_property_from_name game_board house_prop in
    match can_buy_house game_board p_id prop_obj with
    | true ->
      if (get_money game_board p_id >= house_cost) then
        (Printf.printf "\nYou have bought a house for your property!";
        add_house game_board p_id prop_obj;
        buy_house p_id)
      else
        (Printf.printf "\nYou do not have enough money to buy this house.";
        buy_house p_id)
    | false -> ( Printf.printf "\nInvalid move. You can not buy the house because you either do not own the property, the property doesn't exist, or you don't have a monopoly.");
      buy_house p_id
  | "properties" -> (print_players_properties game_board p_id; buy_house p_id)
  | "quit" -> ()
  | _ -> (Printf.printf "\nInvalid command."; buy_house p_id)

(* Loop through game states, and update game state. This loop is taken for
each player that plays the game. *)
let rec game_loop () =
  turns := !turns + 1;
  let _ = if !turns > 4 then (turns := 1; rounds := !rounds + 1) else () in
  if !rounds >= tot_rounds then ()
  else let curr_player_id = !turns - 1 in
  if others_bankrupt game_board curr_player_id then ()
  else if is_bankrupt game_board curr_player_id then
      (Printf.printf "\nPlayer %d, you are bankrupt, so your turn will be skipped.\n" curr_player_id)
  else if is_ai game_board curr_player_id then
      (ai_decision game_board curr_player_id; game_loop ())
  else
    (* REPL for the individual players and the actions they can perform. *)
    let _ = Printf.printf "__________________________________________________________\n" in
    let _ = Printf.printf "__________________________________________________________" in
    let _ = Printf.printf "\nPlayer %d, it is your turn.\nPress enter to roll the dice -> " curr_player_id in
    let _ = Pervasives.read_line () in

    let (d1, d2) = roll_dice () in
    Printf.printf "\nYou have rolled a %d and %d, with a total move of %d.\n" d1 d2 (d1+d2);

    let prompt_buy_property = ref false in
    let bought_property     = ref false in

    move_player game_board curr_player_id (d1+d2);

    let _ =
    if in_jail game_board curr_player_id then
      (Printf.printf "\n\nYou are also currently in jail.";
      move_to_jail game_board curr_player_id;
      if (d1=d2) then
        (Printf.printf "\nSince you rolled a double, though, you can move out of jail at no cost!\n")
      else
        (Printf.printf "\nYou did not roll a double, though, so you will lose $%d and move out of jail.\n" jail_fee;
        change_money game_board curr_player_id (-jail_fee)))
    else () in

    let player_position = (get_pl_position game_board curr_player_id) in

    let prop_option = (get_property game_board player_position) in

    let _ = match prop_option with
    | None ->
      (if is_chance game_board player_position then
        let (message, money_change, other_money_change, move_space) = get_chance game_board in
        (Printf.printf "\n---------------------------\nYou got a chance card!\n%s\n---------------------------\n" (message);
        change_money game_board curr_player_id money_change;
        change_others_money game_board curr_player_id other_money_change
        (*move_to_position game_board curr_player_id move_space*)
      else if is_chest game_board player_position then
        let (message, money_change, other_money_change, move_space) = get_chest game_board in
        (Printf.printf "\n---------------------------\nYou got a community chest card!\n%s\n---------------------------\n" (message);
        change_money game_board curr_player_id (money_change));
        change_others_money game_board curr_player_id other_money_change;
        (*move_to_position game_board curr_player_id move_space*)
      else if is_go_jail game_board player_position then
        (Printf.printf "\n---------------------------\nYou are going to jail :(\n---------------------------\n";
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

          let int_pow a b = int_of_float ((float_of_int a) ** (float_of_int b)) in

          let pay_amt = rent_amt * (int_pow 2 num_houses) in
          (Printf.printf "\n---------------------------\nYou have landed on player %d's property, and will pay a rent of %d.\n---------------------------\n" p_id pay_amt;
          change_money game_board curr_player_id (-1 * pay_amt);
          change_money game_board p_id (pay_amt))) in

    let rec mini_repl () =

      (* First print the relevant options. *)
      let _ = Printf.printf "\nYou have the following options:\n
        Money - Displays how much money you currently have
        Property - Displays what properties you own
        Position - Displays your numeric position on the board
        Trade - Initiates a trade, if possible
        Upgrade - Upgrades a property with additional houses, if possible
        House - Options for buying houses for a property
        Done - End turn" in

      let _ = if !prompt_buy_property then
        (Printf.printf
        "\n\tBuy - Options for buying the current property")
      else () in

      Printf.printf "\n\nCommand -> ";
      let command = Pervasives.read_line () in

      match String.lowercase command with
      | "money" ->
        (Printf.printf "\n---------------------------\nYou have $%d.\n---------------------------" (get_money game_board curr_player_id);
        mini_repl ())
      | "property" ->
        (print_players_properties game_board curr_player_id;
        mini_repl ())
      | "position" -> (Printf.printf "\n---------------------------\nYou are currently on position %d.\n---------------------------" player_position; mini_repl ())
      (*| "trade" -> (execute_trade (); mini_repl ()) TODO *)
      | "house" -> (buy_house curr_player_id; mini_repl ())
      | "done" -> ()
      | "buy" -> (* Buying a new property. *)
        if not !prompt_buy_property then (Printf.printf "\n---------------------------\nInvalid command.\n---------------------------\n"; mini_repl ())
        else
          let transaction = property_prompt curr_player_id player_position in
          let _ = if transaction then (prompt_buy_property := false; bought_property := true) else () in
          mini_repl ()
      | _ -> ((Printf.printf "\n---------------------------\nInvalid command.\n---------------------------"); mini_repl ()) in

    let _ = (mini_repl ()) in
  game_loop ()

let _ = game_loop ()

(* Determine end state of game. *)

(* We know that the game has ended, so there is a winner or winner(s).
Either 3 of the 4 players became bankrupt or the total rounds were finished. *)

(* Maximum value is determined by money held + value in property *)
let winner_id = ref (0)
let winner_value = ref min_int

(* Loop through players and determine how much value they have *)
let rec determine_winner id =
  if id >= 4 then () else
  let held_money = get_money game_board id in
  let total_property_value = get_player_property_val game_board id in
  let tot_value = held_money + total_property_value in
  let _ = if (tot_value > !winner_value) then
    (winner_id := id; winner_value := tot_value)
  else () in
  determine_winner (id+1)

let _ = Printf.printf "\n\nThe game is finished! Thanks for playing!\n\n"

(* Done with game script. *)