(* Give introductory message, need to press enter to continue *)
Printf.printf "Welcome to OCaml Monopoly! This game has been developed
  by\nSacheth Hegde\nGautam Ramaswamy\nGaurab\nTian Yao\n\nPlease press enter to start."

let _ = (* Press enter *)

(* See if connecting to a separate game or making a new one. (If using Ocsigen) *)

(* Get total players. *)
let get_players () =
  Printf.printf "Please enter the number of players playing: ";
  let num_players =(* Get input *) in

  Printf.printf "Is this value correct? (y/n)";
  let correct = (* Get input *) in
  if correct then num_players else get_players ()

let num_players = get_players()

let player_list = ref []

(* Function to manage configurations for every player *)
let player_config_creator (ai: bool) (name: string) (ocsigen_config: ocsigen_conf option) (token: token) =
  (* Use configurations shown above and append to list each of the players with the corresponding configurations. *)

(* Loop through players and get configurations (i.e. AI or not AI, ocsigen configurations if not AI) *)
(* Very pseudo-cody at this point. *)
for i in 1:num_players (* unsure syntax *)
  Printf.printf "Is player " + i + " an AI? (y/n) "
  let ai = (* get_input *) in
  Printf.printf "Name? ";
  let name = (* get input *) in

  Printf.printf "Token? ";
  let token = (* get input *) in

  if ai then player_config_creator true name None token
  else

  Printf.printf "Oscigen configuration? ";
  let ocsigen_config = (* Get input *) in
  player_config_creator false name (Some ocsigen_config) token

(* Initialize the game state and start up the GUI + Ocsigen connection if necessary. *)
let game_state = initialize_game_state player_list

(* Returns a number between 1 and 12 inclusive, simulating two dice rolled. *)
let roll_dice () : int =
  let dice1 = (* Get random num *) in
  let dice2 = (* Get random num *) in
  dice1 + dice2


(* Loop through game states, and update game state*)
let game_loop () =
  Printf.printf "Press any key to roll the dice -> ";
  let _ = (* Get enter*) in

  let num = roll_dice () in

  Printf.printf "You rolled a " + num;

  (* Note getting curr_player will change the order of the lists, so it should only be called when using a new player. (Due to option True) *)
  let curr_player = get_current_player game_state True in

  update_board curr_player num;

  (* See current position of the player and print it out. *)

  (* Make decisions based on what kind of tile it is (e.g., if it's jail vs property, vs someone else's property. *)

  (* If applicable, using a bool, ask for some inputs such as auctioning, seeing current score, etc. *)
  (* There will be*)

game_loop ()

Printf.printf "Game finished, yay"

(* Done *)








