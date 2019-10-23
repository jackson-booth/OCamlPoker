(**A module for parsing user input*)

(**Raised if an input is empty*)
exception Empty

(**Raised if an input is malformed*)
exception Malformed

(**Raised if a player instantiating a game wishes to instantiate a saved game
*)
exception Retrieve of string
(**Raised if a player raises by a non integer*)
exception ExpectInt

(**Raised if a player exits before instantiating a game*)
exception PrematureExit

(**The type of a possible player response*)
type response = Raise of int | Match | Check | Fold | Help | Exit
              | Save of string

(**[parse s] is a command created from [s] if [s] is a valid command. A valid
   command is either "quit" or "go ?", where "?" is a part of the input string.
   Raises: [Empty] if [s] is "", or [Malformed] if [s] is not a recognized
      command.
   Example: parse "raise 10" is a Raise of 10*)
val parse : string -> response

(**[parse_instantiation s] is a list of player_info's corresponding to [s],
   used to instantiate a game.
   Raises: [Malformed] if the input isn't parsable
        [Empty] if the input is empty (or whitespace)
        [Retrieve x] if the user wishes to play a game saved in file [x]
   Example: [parse_instantiation s] where [s] is
   ["Picard human 1701, Data ai 1701,"] is
   [("Picard",1701,Human);("Data",1701,AI (0,0))]*)
val parse_instantiation : string -> (Model.player_info list) * bool
