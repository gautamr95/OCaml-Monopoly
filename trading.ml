open Str
open Game_utils

(* req = requested prop
 * off = offered prop
 * rm = requested money
 * om = offered money
 * pl = the requester
 * tp = the one pl wants to trade with
 * Starts a trade request with the specified parameters*)
let rec trade_offer req off rm om pl tp : bool =
  Gui.print_to_cmd (Printf.sprintf "Player %i's trade request:\n" tp);
  Gui.print_to_cmd (Printf.sprintf "Player %i wants:\n" pl);
  List.iter (fun x -> Gui.print_to_cmd ("\n" ^ x)) req;
  Gui.print_to_cmd (Printf.sprintf "$%i" rm);
  Gui.print_to_cmd "In exchange for:";
  List.iter (fun x -> Gui.print_to_cmd ("\n" ^ x)) off;
  Gui.print_to_cmd (Printf.sprintf "$%i" om);
  Gui.print_to_cmd "will you accept? (y/n):";
  let input = String.lowercase(read_line ()) in
  if input = "y" then true else if input = "n" then false else (print_endline "Invalid";
    trade_offer req off rm om pl tp)

(*call this function given the above event, if the player says they want to trade,
this function steps them through the prompts to make a trade request, if the any input is valid
it kicks you out of trading and repeat the main repl, once its taken all your inputs,
 it calls trade_offer which then prompts the player who was requested to respond to
the trade, if they say yes, then the trade carries out with the function make_trade
which manipupates the players money and property list based on this, it is a function who returns
unit, this function will be done once we get the board figured out more*)
let trade_prompt pl b : unit=
  let rec trade_player_prompt () = 
    print_string "Who do you want to trade with?\n";
    try (int_of_string (read_line ())) with 
    | Failure s -> trade_player_prompt () in
  let trade_player = trade_player_prompt () in
  print_string "What properties do you want? None for just money";
  let requests = String.lowercase(read_line ()) in
  let req_list = split (regexp " ") requests in
  let correct_holder x = 
    match (get_property_from_name b x) with
    | None -> -1
    | Some prop -> (
      match get_holder prop with
      | None -> -1
      | Some i -> i) in
  let valid = List.fold_left (fun a x -> a && (correct_holder x) = trade_player) true in
  if (not (valid req_list)) then Printf.printf "Invalid entry" 
  else
    let req_props = List.map (get_property_from_name b) req_list in
    let rec request_prompt () =
      Printf.printf "How much money do you want?\n";
      try (int_of_string (read_line ())) with
      | Failure s -> request_prompt () in
    let money = request_prompt () in
    if money > (get_money b trade_player) then Printf.printf "They cannot afford this\n" else
      print_string "What properties will you offer?";
      let offer = String.lowercase(read_line ()) in
      let offer_list = split (regexp " ") requests in
      if not (valid offer_list) then Printf.printf "Invalid entry" 
      else
        let offer_props = List.map (get_property_from_name b) offer_list in
        let rec offer_prompt () = 
          Printf.printf "How much Money will you offer?\n";
          try (int_of_string (read_line ())) with
          | Failure s -> offer_prompt () in
        let offer = offer_prompt () in
        if offer > (get_money b pl) then Printf.printf "You cannot afford this\n" else
          let trade_accept = trade_offer req_list offer_list money offer pl trade_player in
          if trade_accept then make_trade req_props offer_props money offer pl trade_player else
            print_string "Trade denied"

