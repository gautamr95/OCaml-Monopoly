open Str
(* Game constants *)
(*let total_players = 4 in
let starting_money = 200 in
let go_salary = 30 in
let jail_fee = 30 in
let tot_rounds = 50 in
*)

(* Give introductory message, need to press enter to continue *)
Printf.printf "\n\n\nWelcome to OCaml Monopoly! This game has been developed by
  \nSacheth Hegde\nGautam Ramaswamy\nGaurab Bhattacharya\nTian Yao\n\nPlease press enter to start: "

let _ = Pervasives.read_line ()

(* Applies regex and tokenizing to split an input into the corresponding words
  Input: input_str - the String that needs to be tokenized
  Output: A string list of the tokenized input *)
let parse_input (input_str: string) : string list =
  (Str.bounded_split (Str.regexp "[ \t]+") input_str 2)

let is_correct () =
  Printf.printf "Is this value correct? (y/n) -> ";
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

(* Ref that will point to a list of the players *)
(* let player_list = ref [] *)

(* Function to get names of each of the players. *)
let get_player_names () : string list =

  let rec player_list_creator player_id acc =
    if player_id <= 4 then
      ((Printf.printf "\nPlayer %d, enter your name -> " player_id);
      let name = (Pervasives.read_line ()) in
      player_list_creator (player_id + 1) (acc@[name]))
    else acc in

  player_list_creator 1 []

let player_names_list = get_player_names ()

let game_board = create_board num_players player_names_list

(* Returns a number between 1 and 12 inclusive, simulating two dice rolled. *)
let roll_dice () : (int * int) = (Random.int 6, Random.int 6)

(* total turns (for each round)*)
let rounds = ref 0

(* Used to calculate the accumulated turns, within a round *)
let turns = ref 0

(* Loop through game states, and update game state. This loop is taken for
each player that plays the game. *)
let rec game_loop () =
  turns := !turns + 1;
  let _ = if !turns > 4 then turns := 0; rounds := !rounds + 1 else () in
  let _ = if rounds >= tot_rounds then () else
    let curr_player_id = !turns - 1 in

    if others_bankrupt game_board curr_player_id then () else begin

      if is_bankrupt game_board curr_player_id then
      Printf.printf "\nPlayer %d, you are bankrupt, so your turn will be skipped.\n" curr_player_id;

      else begin

        if is_ai curr_player then
          ai_decision game_board player ()
        else begin
          (* REPL for the individual players and the actions they can perform. *)

          Printf.printf "Press any key to roll the dice -> "
          let _ = Pervasives.read_line () in

          let (d1, d2) = roll_dice () in
          Printf.printf "\nYou have rolled a %d and %d, with a total move of %d." d1 d2 (d1+d2);

          let mini_repl () =

            (* Not really implemented yet. *)
            let _ = if in_jail curr_player then
              Printf.printf "\n\nYou are also currently in jail.";
              if (d1=d2)
                Printf.printf "\nSince you rolled a double, you can move out of jail at no cost!\n";
              else
                Printf.printf "\nYou did not roll a double, so you will lose $%d and move out of jail.\n" jail_fee;
                change_money game_board curr_player_id (-jail_fee);

            else () in

            move_player game_board curr_player_id (d1+d2);
            (* First move to the location, and then check the stuff below *)

            if is_chance game_board (get_pl_position curr_player_id) then
              let card = get_chance game_board in
              Printf.printf "\n%d\n" (fst card);
              change_money game_board curr_player_id (snd card)
            else if is_chest game_board (get_pl_position curr_player_id) then
              let card = get_chest game_board in
              Printf.printf "\n%d\n" (fst card);
              change_money game_board curr_player_id (snd card)
            else (* is_go_to_jail *)

            (* First print the relevant options. *)
            Printf.printf "You have the following options:\n
              Money - Displays how much money you currently have\n
              Property - Displays what properties you own\n
              Summary - Displays a summary of all the players\n
              Position - Displays your numeric position on the board\n
              Trade: Initiates a trade, if possible\n
              Upgrade: Upgrades a property with additional houses, if possible\n
              Done: End turn";
        end
      end
    end
  end

game_loop ()

Printf.printf "Game finished, yay!"

(* Done *)








