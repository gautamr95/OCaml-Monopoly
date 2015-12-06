open Game_utils
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
(* req = requested prop
 * off = offered prop
 * rm = requested money
 * om = offered money
 * pl = the requester
 * tp = the one pl wants to trade with
 * Starts a trade request with the specified parameters*)
let rec trade_offer req off rm om pl tp : bool =
  Gui.print_to_cmd ("\n----------------------- \n");
  Gui.print_to_cmd (Printf.sprintf "trade request for Player %i: \n" tp);
  Gui.print_to_cmd (Printf.sprintf "Player %i wants:\n" pl);
  List.iter (fun x -> Gui.print_to_cmd ((get_prop_name x) ^"\n")) req;
  Gui.print_to_cmd (Printf.sprintf "$%i\n" rm);
  Gui.print_to_cmd "In exchange for:\n";
  List.iter (fun x -> Gui.print_to_cmd ((get_prop_name x) ^ "\n")) off;
  Gui.print_to_cmd (Printf.sprintf "$%i\n" om);
  Gui.print_to_cmd "will you accept? (y/n):\n";
  let input = String.lowercase(get_input ()) in
  if input = "y" then true else if input = "n" then false else ( Gui.print_to_cmd "Invalid\n";
    trade_offer req off rm om pl tp)