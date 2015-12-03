open Player_functions
open Board_functions

(* draw_gui [b pr pl] updates the GUI with the current board b, property
list pr, and player list pl *)
val draw_gui : board -> property list -> player list -> ()

(* print_to_cmd [str] prints the [str] to the command display inside the GUI
  preconditions: None
  postconditions: None*)
val print_to_cmd : str -> ()
