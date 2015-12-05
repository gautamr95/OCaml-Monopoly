open Async.Std

let wait_lock = ref (Mutex.create ())
let cmd_input_str = ref "fuck"

let get_input () =
  Mutex.unlock (!wait_lock);
  Gtktest.readline wait_lock cmd_input_str;
  Gtktest.print_to_cmd "waiting for lock\n";
  Mutex.lock (!wait_lock);
  Gtktest.print_to_cmd "lock acquired\n";
  Gtktest.print_to_cmd (!cmd_input_str);
  Mutex.unlock (!wait_lock);
  Gtktest.print_to_cmd "lock unlocked\n"