open Str
(* Game constants *)
(*let total_players = 4 in
let starting_money = 200 in
let go_salary = 30 in*)

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

(* Loop through game states, and update game state. This loop is taken for
each player that plays the game. *)
let game_loop () =
  let curr_player = get_turn game_state in
  let curr_pos = get_position game_state curr_player in

  if in_jail curr_player then df
    df
  else





game_loop ()

Printf.printf "Game finished, yay!"

(* Done *)








