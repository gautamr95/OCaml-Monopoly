(* Game constants *)
let total_players = 4
let starting_money = 200
let go_salary = 30


(* Give introductory message, need to press enter to continue *)
Printf.printf "Welcome to OCaml Monopoly! This game has been developed
  by\nSacheth Hegde\nGautam Ramaswamy\nGaurab Bhattacharya\nTian Yao\n\nPlease press enter to start -> "

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
  match correct with
  | None -> false
  | Some a ->
    if a = "y" then true else false
  if correct = "y" then num_players
  else get_players ()

(* Gets total players through user input
  Input: Unit
  Output: Number of players determined by user input. *)
let get_players () : int =
  Printf.printf "Please enter the number of human players -> ";
  let num_players_option =
    try Some (int_of_string (Pervasives.read_line ())) with
    | Failure s -> None in

  let num_players =
    match num_players_option with
    | None -> get_players ()
    | Some a -> a in

  let correct = is_correct () in if correct then num_players else get_players ()

(* Get and store the ocsigen configurations *)
let get_ocsigen_config () =
  Printf.printf "Please enter the ocsigen server configuration -> ";
  let ocsigen_config_option =
    try Some (Pervasives.read_line ()) with
    | Failure s -> None in

  let ocsigen_config =
    match ocsigen_config_option with
    | None -> get_ocsigen_config ()
    | Some a -> a in

  let correct = is_correct () in
  if correct then ocsigen_config else get_ocsigen_config ()

let num_players = get_players ()

(* Ref that will point to a list of the players *)
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

  Player p
  (* Assuming access to a Player Object *)
  let player_name = get_player_name p in

  if in_jail p then
    Printf.printf "%x, you are in jail. The following commands are available: \n" player_name;
    Printf.printf "(1) Get money held\n(2) Get game summary\n(3) Get properties held
    \n(4) Get position\n(5) Roll dice (to attempt jail escape)\n
    (6) Pay $50 (for jail escape)";

    let
  else Printf.printf "Press any key to roll the dice -> ";
  let _ = Pervasives.read_line () in

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

Printf.printf "Game finished, yay!"

(* Done *)








