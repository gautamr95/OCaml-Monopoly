open Game_utils

let guit = Thread.create (Gui.main) ()
let gamet = Thread.create (Game_main.get_input) ()

let _ = (Thread.join guit, Thread.join gamet)