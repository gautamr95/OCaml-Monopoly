open Str
if readline_string = "TRADE" then

(*call this function given the above event, if the player says they want to trade,
this function steps them through the prompts to make a trade request, if the any input is valid
it kicks you out of trading and repeat the main repl, once its taken all your inputs,
 it calls trade_offer which then prompts the player who was requested to respond to
the trade, if they say yes, then the trade carries out with the function make_trade
which manipupates the players money and property list based on this, it is a function who returns
unit, this function will be done once we get the board figured out more*)
let trade_prompt pl b : unit=
  print_string "Who do you want to trade with?\n";
  let trade_player_s = String.lowercase(read_line ()) in
  let trade_player = get_player b trade_player_s in
  if trade_player = "null" then print_string "Invalid entry" else
    print_string "What properties do you want? None for just money";
    let requests = String.lowercase(read_line ()) in
    let req_list = split (regexp " ") requests in
    let valid = List.fold_left (fun x y -> x && (player_has_property trade_player)) true req_list in
    if not valid then "Invalid entry" else
      let req_props = List.map (get_property b) req_list in
      print_string "How much Money do you want?";
      let money_s = read_line () in
      let money_opt = try Some(int_of_string money_s) with
                      | failure _ -> None in
      match money_opt with
      |None -> "Invalid entry"
      |Some money ->
        if money > get_money trade_player then "Invalid entry" else
          print_string "What properties will you offer?";
          let offer = String.lowercase(read_line ()) in
          let offer_list = split (regexp " ") requests in
          let valid = List.fold_left (fun x y -> x && (player_has_property pl)) true offer_list in
          if not valid then "Invalid entry" else
          let offer_props = List.map (get_property b) offer_list in
          print_string "How much Money will you offer?";
          let money_o_s = read_line () in
          let money_o_opt = try Some(int_of_string money_o_s) with
                            | failure _ -> None in
          match money_o_opt with
          | None -> "Invalid Entry"
          | Some money_o ->
            if money_o > get_money pl then "invalid entry" else
              let trade_accept = trade_offer req_list offer_list money_s money_o_s pl trade_player in
              if trade_accept then make_trade req_props offer_props money money_o pl trade_player else
                print_string "trade denied"


let rec trade_offer req off rm om pl tp : bool =
  print_endline (get_name tp) ^ "'s trade request:";
  print_endline (get_name pl) ^ " wants:";
  List.iter print_endline req;
  print_endline ("$" ^ rm);
  print_endline "In exchange for:";
  List.iter print_endline off;
  print_endline ("$" ^ om);
  print_endline "will you accept? (y/n)";
  let input = String.lowercase(read_line ()) in
  if input = y then true else if input = n then false else print_endline "Invalid";
    trade_offer req off rm om pl tp