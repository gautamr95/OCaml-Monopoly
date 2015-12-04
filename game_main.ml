open Str
open Game_utils

(* Game constants *)
let total_players = 4
let starting_money = 200
let go_salary = 30
let jail_fee = 30
let tot_rounds = 50

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
  Go_jail :: []
let create_chance_list () =
  [("boo", -50) ; ("shoo",30)]

let create_community_chest_list () =
  [("foo", 100) ; ("dog",-300)]

let property_list = create_prop_list ()
let tile_list = create_tile_list property_list
let chance_list = create_community_chest_list()
let community_chest_list =  create_chance_list()
(*      end             *)



(* Give introductory message, need to press enter to continue *)
let _ = (Printf.printf "\n\n\nWelcome to OCaml Monopoly! This game has been developed by
  \nSacheth Hegde\nGautam Ramaswamy\nGaurab Bhattacharya\nTian Yao\n\nPlease press enter to start: ")

let _ = Pervasives.read_line ()

(* Function that asks for correct values (confirmation), and returns
a boolean based on the user input.
  Input - Unit
  Output - bool of whether or not the input was valid *)
let is_correct () : bool =
  Printf.printf "Is this value correct? (y/n) -> ";
  (* Checks for any input errors *)
  let correct = try Some (Pervasives.read_line ()) with
    | Failure s -> None in

  let correction = match correct with
  | None -> false
  | Some a ->
    if a = "y" then true else false in
  correction

(* Gets total players through user input
  Input: Unit
  Output: Number of players determined by user input. *)
let rec get_players () : int =

  let rec get_players_prompt () =
    Printf.printf "\nPlease enter the number of human players -> ";
    try (int_of_string (Pervasives.read_line ())) with
    | Failure s -> get_players_prompt () in

  let num_players = get_players_prompt () in

  let correct = is_correct () in if correct then num_players else get_players ()

let num_players = get_players ()

let rec make_ai_list acc human_num init =
  if init = 5 then acc
  else if human_num > 0 then
    (make_ai_list (acc@[false]) (human_num-1) (init+1))
  else (make_ai_list (acc@[true]) (human_num) (init+1))

let is_ai_list = make_ai_list [] num_players 1

let game_board = create_board is_ai_list community_chest_list chance_list
                          property_list tile_list

(* Returns a number between 1 and 12 inclusive, simulating two dice rolled. *)
let roll_dice () : (int * int) = (Random.int 6, Random.int 6)

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
        let _ = (Printf.printf "\nYou have bought the property!") in
        let _ = (move_property game_board p_id None prop) in
        let _ = (change_money game_board p_id (-1 * prop_price)) in
        true)
    | "n" -> false
    | _ -> Printf.printf "\nInvalid command."; false
    )

(* Loop through game states, and update game state. This loop is taken for
each player that plays the game. *)
let rec game_loop () =
  turns := !turns + 1;
  let _ = if !turns > 4 then (turns := 0; rounds := !rounds + 1) else () in
  if !rounds >= tot_rounds then ()
  else let curr_player_id = !turns - 1 in
  if others_bankrupt game_board curr_player_id then ()
  else if is_bankrupt game_board curr_player_id then
      (Printf.printf "\nPlayer %d, you are bankrupt, so your turn will be skipped.\n" curr_player_id)
  else if is_ai game_board curr_player_id then
      (* TODO *)
      (*ai_decision game_board player ()*) ()
  else
    (* REPL for the individual players and the actions they can perform. *)
    let _ = Printf.printf "Player %d, it is your turn.\nPress any key to roll the dice -> " curr_player_id in
    let _ = Pervasives.read_line () in

    let (d1, d2) = roll_dice () in
    Printf.printf "\nYou have rolled a %d and %d, with a total move of %d." d1 d2 (d1+d2);

    let prompt_buy_property = ref false in
    let bought_property     = ref false in

    let rec mini_repl () =

      let _ = prompt_buy_property := false in

      (* Not really implemented yet. *)
      let _ =
      if in_jail game_board curr_player_id then
        (Printf.printf "\n\nYou are also currently in jail.";
        if (d1=d2) then
          (Printf.printf "\nSince you rolled a double, you can move out of jail at no cost!\n")
        else
          (Printf.printf "\nYou did not roll a double, so you will lose $%d and move out of jail.\n" jail_fee;
          change_money game_board curr_player_id (-jail_fee)))

        (*failwith "TODO"*)
      else () in

      move_player game_board curr_player_id (d1+d2);

      let player_position = (get_pl_position game_board curr_player_id) in

      let prop_option = (get_property game_board player_position) in

      let _ = match prop_option with
      | None ->
        (if is_chance game_board player_position then
          let card = get_chance game_board in
          (Printf.printf "\n%s\n" (fst card);
          change_money game_board curr_player_id (snd card))
        else if is_chest game_board player_position then
          let card = get_chest game_board in
          (Printf.printf "\n%s\n" (fst card);
          change_money game_board curr_player_id (snd card))
        else if is_go_jail game_board player_position then
          (Printf.printf "\nYou are going to jail :(\n";
          (*TODO*)()))
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
            (Printf.printf "\nYou have landed on player %d's property, and will pay a rent of %d.\n" p_id pay_amt;
            change_money game_board curr_player_id (-1 * pay_amt);
            change_money game_board p_id (pay_amt))) in

      (* First print the relevant options. *)
      let _ = Printf.printf "\nYou have the following options:\n
        Money - Displays how much money you currently have\n
        Property - Displays what properties you own\n
        Position - Displays your numeric position on the board\n
        Trade - Initiates a trade, if possible\n
        Upgrade - Upgrades a property with additional houses, if possible\n
        House - Options for buying houses for a property\n
        Done - End turn" in

      let _ = if !prompt_buy_property then
        (Printf.printf "Buy - Options for buying the current property")
      else () in

      Printf.printf "\n\nCommand -> ";
      let command = Pervasives.read_line () in

      match command with
      | "Money" ->
        (Printf.printf "\nYou have $%d." (get_money game_board curr_player_id);
        mini_repl ())
      | "Property" ->
        (print_players_properties game_board curr_player_id;
        mini_repl ())
      | "Position" -> (Printf.printf "\nYou are currently on position %d." player_position; mini_repl ())
      (*| "Trade" -> (execute_trade (); mini_repl ()) TODO *)
      (*| "House" -> (buy_house (); mini_repl ()) TODO *)
      | "Done" -> ()
      | "Buy" -> (* Buying a new property. *)
        if !prompt_buy_property then (Printf.printf "\nInvalid command.")
        else
          let transaction = property_prompt curr_player_id player_position in
          let _ = if transaction then (bought_property := true) else () in
          mini_repl ()
      | _ -> ((Printf.printf "\nInvalid command."); mini_repl ()) in

    (mini_repl ())

let _ = game_loop ()

let _ = Printf.printf "\n\nGame finished, yay!"

(* Done *)








