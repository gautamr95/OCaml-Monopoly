open Player_functions
open Board_functions
open AI_functions

type board
type player
type property

(*main repl*)
val main : board -> player -> player -> player -> player -> int -> unit

(*move player on board, modify players position and return a new player and
  return property landed on*)
val move : player -> board -> player * property

(*take in landed on property, list of players in the game, player making move,
  game board, and return the players updated state if the bought or not
  and the new game board*)
val prop_check : property -> player list -> player ->  board -> player * board
