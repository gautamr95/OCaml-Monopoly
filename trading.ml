open Str
open Game_utils
open Trade_offer
open Async.Std

let wait_lock = ref (Mutex.create ())
let cmd_input_str = ref ""

(* Mutex based function to get inputs *)
let get_input () : string=
  Mutex.unlock (!wait_lock);
  Gui.readline wait_lock cmd_input_str;
  Mutex.lock (!wait_lock);
  Mutex.unlock (!wait_lock);
  !cmd_input_str

(*call this function given the above event, if the player says they want to trade,
this function steps them through the prompts to make a trade request, if the any input is valid
it kicks you out of trading and repeat the main repl, once its taken all your inputs,
 it calls trade_offer which then prompts the player who was requested to respond to
the trade, if they say yes, then the trade carries out with the function make_trade
which manipupates the players money and property list based on this, it is a function who returns
unit, this function will be done once we get the board figured out more*)
let trade_prompt b pl : unit=

  let rec trade_player_prompt () =
    Gui.print_to_cmd "Back - Go back to main options \nWho do you want to trade with? Enter player number\n";
    let inp = get_input() in if String.lowercase(inp) = "back" then -1 else
      try (int_of_string (inp)) with
      | Failure s -> Gui.print_to_cmd "Invalid entry\n"; trade_player_prompt () in

  let trade_player = trade_player_prompt () in
  if trade_player = -1 then () else
  let is_done = get_done b trade_player in
  if is_done then Gui.print_to_cmd "That player is no longer in the game\n"
  else(
  Gui.print_to_cmd "What properties do you want? None for just money \n";
  let requests = String.lowercase(get_input ()) in
  if requests = "back" then ()
  else
    let req_list = if requests = "none" then []
                   else split (regexp ", ") requests in

    let correct_holder x =
      match (get_property_from_name b x) with
      | None -> -1
      | Some prop -> (
        if (get_houses prop = 0)  then
          (match get_holder prop with
          | None -> -1
          | Some i -> if (List.fold_left (fun x y -> x && (get_houses y = 0)) true
                      !(get_pl_prop_of_color b i prop)) then i else -1)
        else -1) in

    let valid pl lst = List.fold_left (fun a x -> a && ((correct_holder x) = pl)) true lst in

    if (not (valid trade_player req_list)) then Gui.print_to_cmd "Invalid entries\n"
    else
      let req_p = List.map (get_property_from_name b) req_list in
      let req_props = List.fold_left
        (fun x y -> match y with
                    |Some(a) -> a::x
                    |None -> x) [] req_p in

      let rec request_prompt () =
        Gui.print_to_cmd "How much money do you want?\n";
        let inp2 = String.lowercase(get_input()) in
        if inp2 = "back" then -1
        else
          try (int_of_string (inp2)) with
          | Failure s -> request_prompt () in

      let money = request_prompt () in
      if money = -1 then () else
      if money < 0 then Gui.print_to_cmd "Money must be greater than 0\n" else
      if money > (get_money b trade_player) then Gui.print_to_cmd "They cannot afford this\n" else
        Gui.print_to_cmd "What properties will you offer? None for just Money\n Please place \", \" between properties";
        let offer = String.lowercase(get_input ()) in
        if offer = "back" then ()
        else
          let offer_list = if offer = "none" then []
                           else split (regexp ", ") offer in
          if not (valid pl offer_list) then Gui.print_to_cmd "Invalid entry\n"
          else
            let offer_p = List.map (get_property_from_name b) offer_list in
            let offer_props = List.fold_left (fun x y -> match y with
                                                         |Some(a) -> a::x
                                                         |None -> x) [] offer_p in
            let rec offer_prompt () =
              Gui.print_to_cmd "How much Money will you offer?\n";
              let inp3 = String.lowercase(get_input()) in
              if inp3 = "back" then -1
              else
                (try (int_of_string (inp3)) with
                | Failure s -> offer_prompt ()) in

            let offer = offer_prompt () in
            if offer = -1 then () else
            if offer < 0 then Gui.print_to_cmd "Money must be greater than 0\n" else
            if offer > (get_money b pl) then Gui.print_to_cmd "You cannot afford this\n" else
              let trade_accept =
              (if is_ai b trade_player then
                AI_functions.accept_trade b
              else
                trade_offer) req_props offer_props money offer pl trade_player in

              if trade_accept then
                let _ = List.iter (move_property b pl (Some trade_player)) req_props in
                let _ = List.iter (move_property b trade_player (Some pl)) offer_props in
                let _ = change_money b pl (-offer) in
                let _ = change_money b trade_player (offer) in
                let _ = change_money b pl (money) in
                let _ = change_money b trade_player (-money) in
                Gui.print_to_cmd "Trade accepted!\n"
              else
                Gui.print_to_cmd "Trade denied!\n")

