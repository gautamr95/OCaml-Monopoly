open Game_utils

(* draw_gui [b pr pl] updates the GUI with the current board b, property
list pr, and player list pl *)
val updateboard : board -> unit

(* print_to_cmd [str] prints the [str] to the command display inside the GUI
  preconditions: None
  postconditions: None*)
val print_to_cmd : ('a, unit, string) format -> unit

(* readline [ref lock] [ref str] will set the [ref str] to the user's command
  input after the user enters a command. the function will also lock the Mutex
  [ref lock] that was created in the calling thread, so the calling thread has
  the option to do a busy wait and block until the user has entered a command
  preconditions: None
  postconditions: None*)
val readline : Mutex.t ref -> string ref -> unit

val main : unit -> unit