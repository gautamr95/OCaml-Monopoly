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
  List.iter (fun x -> Gui.print_to_cmd ("\n" ^ get_prop_name x)) req;
  Gui.print_to_cmd (Printf.sprintf "$%i" rm);
  Gui.print_to_cmd "In exchange for:";
  List.iter (fun x -> Gui.print_to_cmd ("\n" ^ get_prop_name x)) off;
  Gui.print_to_cmd (Printf.sprintf "$%i" om);
  Gui.print_to_cmd "will you accept? (y/n):";
  let input = String.lowercase(read_line ()) in
  if input = "y" then true else if input = "n" then false else (print_endline "Invalid";
    trade_offer req off rm om pl tp)