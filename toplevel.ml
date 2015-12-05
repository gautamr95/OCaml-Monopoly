open Game_utils

let guit = Thread.create (Gtktest.main) ()
let gamet = Thread.create (Gamethread.get_input) ()

let _ = (Thread.join guit, Thread.join gamet)