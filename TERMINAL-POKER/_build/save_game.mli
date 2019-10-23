(**A module to provide functionality for saving a game*)
open Model

(**[get_winner g] is either Some of the winner of game [g], or should no
   player have won yet, none.*)
val save_game : Model.game -> string -> unit

(** [retrieve_game file_name] reads the game from a file named file_name
    and returns a valid game representation*)
val retrieve_game : string -> Model.game

(**Thrown when a file isn't found*)
exception FileNotFound of string
