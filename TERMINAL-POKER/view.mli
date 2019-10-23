(**A module for displaying things to the user*)
open Model
open ANSITerminal

(**the default style we use*)
val default_color : style

(**[print_exit ()] is the textual description of exiting the game*)
val print_exit : unit -> unit

(**[print_save ()] is the textual description of saving the game*)
val print_save : string -> unit

(**[print_negative ()] is the textual response of raise by a negative number*)
val print_negative : unit -> unit

(**[print_negative ()] is the textual response of raise by a non integer
number*)
val print_notInteger : unit -> unit

(**[print_retrieve ()] is the textual description of retrieving the game**)
val print_retrieve : unit -> unit

(**[print_malformed_cmd ()] is the textual response to a malformed
   gameplay command**)
val print_malformed_cmd : unit -> unit

(**[print_malformed_ins ()] is the textual response to a malformed
   instantiation command**)
val print_malformed_ins : unit -> unit

(**[print_invalid_ins ()] prints help text for playing correctly
   instantiating a valid game*)
val print_invalid_ins : unit -> unit

(**[print_play_help ()] prints help text for playing the game*)
val print_play_help : unit -> unit

(**[print_help ()] prints help text for playing the game and poker*)
val print_help : unit -> unit

(**[print_too_few_chips ()] tells the user that they have too few chips to
   continue playing, and that they have been automatically folded as a result.
   *)
val print_too_few_chips : unit -> unit

(**[print_cant_raise ()] tells the user that they have too few chips to
   raise by the desired amount.*)
val print_cant_raise : unit -> unit

(**[file_not_found ()] tells the user that the file they wanted to load
   couldn't be.*)
val file_not_found : unit -> unit

(**[print_invalid_instrn ()] tells the user that their instruction was
   invalid.*)
val print_invalid_instrn : unit -> unit

(**[print_cant_check ()] tells the user that they couldn't check.*)
val print_cant_check : unit -> unit

(**[print_next_round g w] prints the winner of the previous round *)
val print_next_round : Model.game -> Model.player -> string * card list ->
  unit

(**[print_winner p g] prints [p] information about winner [p] of game [g],
   when it is determined that [p] has won [g]*)
val print_winner : Model.player -> Model.game -> unit

(**[print_turn g] is a textual discription of [g], with nice formatting and
   functionality to not print a player's private cards without checking if
   that player is the one viewing the screen.
   Requires: [g] is a valid game*)
val print_game : ?open_cards:bool -> Model.game -> unit
