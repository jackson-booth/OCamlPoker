open Model
open View
open ANSITerminal
open Parser
open Ai
open Roundwinner
open Save_game
open Unix


exception GameDone
exception GameSaved of string
exception UnknownError

(**[get_command g p] is the response of player [p] to gamestate [g]. This
   function gets current player input and maps that to a response, either
   through querying the user or the AI for the response.
   Requires: - When querying an AI, the only allowed responses are Raise i,
          Match, Check, and Fold—anything else will raise an error and end
          execution.
   - [g] and [p] are respectively a valid game and valid player
*)
let rec get_command ?open_cards:(open_cards=false) (g:Model.game)
    (p:Model.player) : Parser.response =
  View.print_game ~open_cards:open_cards g;
  save_cursor ();
  match Model.get_player_type p with
  | AI (_, _) -> (match Ai.get_move g p with
      | Parser.Raise i -> Parser.Raise i
      | Parser.Match -> Parser.Match
      | Parser.Check -> Parser.Check
      | Parser.Fold -> Parser.Fold
      | _ -> failwith "AI returned something it shouldn't have!")
  | Human -> print_string [green] "Type 'help' for help\n> ";
    match read_line () with
    | exception End_of_file -> failwith "Error 0"
    | str -> try match Parser.parse str with
      | Parser.Raise i -> Parser.Raise i
      | Parser.Match -> Parser.Match
      | Parser.Check -> Parser.Check
      | Parser.Fold -> Parser.Fold
      | Parser.Help -> Parser.Help
      | Parser.Exit -> Parser.Exit
      | Parser.Save i -> Parser.Save i
      with
      | Parser.Empty -> View.print_malformed_cmd ();
        get_command ~open_cards:true g p
      | Parser.Malformed -> View.print_malformed_cmd ();
        get_command ~open_cards:true g p

(**[play_raise g] is [g] after all players in [g] have either matched the
   minimum bet or folded.
   Requires: [g] is a valid game*)
let rec play_raise ?open_cards:(open_cards=false) (g:Model.game)
  : Model.game =
  let raise_played = Model.raise_resolved g in
  if raise_played then Model.reset_current_player g
  else try let p = Model.get_current_player g in
      let command = get_command ~open_cards:open_cards g p in
      match command with
      | Parser.Raise i -> let g = Model.raise_pot g i in
        play_raise g
      | Parser.Match -> let g = Model.match_pot g in
        play_raise g
      | Parser.Check -> let g = Model.match_pot g in
        print_string [] "To check you had to match the pot!";
        play_raise g
      | Parser.Fold -> let g = Model.fold g in
        play_raise g
      | Parser.Help -> View.print_help (); g |> play_raise
      | Parser.Exit -> View.print_exit (); raise GameDone
      | Parser.Save i ->  (Save_game.save_game g i); View.print_save i;
        raise (GameSaved i)
    with
    | IllegalPlay -> View.print_negative(); play_raise ~open_cards:false g
    | ExpectInt -> View.print_notInteger(); play_raise ~open_cards:false g
    | NoPlayersLeft -> Model.reset_current_player g
    | GameSaved i -> View.print_save i; raise GameDone
    | NotEnoughChips -> View.print_too_few_chips ();
      g |> Model.fold |> play_raise
    | CantRaise -> View.print_cant_raise ();
      play_raise ~open_cards:true g
    | CantCheck -> View.print_cant_check ();
      play_raise ~open_cards:true g

(**[play_stage g] steps game [g] through a stage.
   Requires: -For proper user experience, the sequence of functions calling this
          function must include [play]. That is, this function should be
          "descended" from a call to [play].
          -[g] is a valid game*)
and play_stage ?open_cards:(open_cards=false) (g:Model.game) : Model.game =
  try let p = Model.get_current_player g
    in let command = get_command ~open_cards:open_cards g p in
    match command with
    | Parser.Raise i -> let g = Model.raise_pot g i in
      play_raise g
    | Parser.Match -> let g = Model.match_pot g in
      play_stage g
    | Parser.Check -> let g = Model.check g in
      play_stage g
    | Parser.Fold -> let g = Model.fold g in
      play_stage g
    | Parser.Help -> View.print_help ();
      play_stage g
    | Exit -> View.print_exit (); raise GameDone
    | Parser.Save i -> Save_game.save_game g i;View.print_save i;
      raise (GameSaved i)
  with
  | IllegalPlay -> View.print_negative(); play_raise ~open_cards:false g
  | ExpectInt -> View.print_notInteger(); play_raise ~open_cards:false g
  | NoPlayersLeft -> Model.reset_current_player g
  | GameSaved i -> View.print_save i; raise GameDone
  | NotEnoughChips -> View.print_too_few_chips ();
    g |> Model.fold |> play_raise
  | CantRaise -> View.print_cant_raise ();
    play_raise ~open_cards:true g
  | CantCheck -> View.print_cant_check ();
    play_raise ~open_cards:true g

(**[play_round g] is [g] after a round has been played. Additionally, prints
   the winner of the round and the hand they won with, upon the round being won.
   Requires: -This function must be called from [play] for proper user
          experience
          -[g] is a valid game*)
and play_round (g:Model.game) : Model.game =
  match List.length (Model.get_table g) with
  | 0 ->
    let g = if Model.raise_resolved g then play_stage g
      else play_stage (play_raise g) in
    let g = Model.deal_3_to_table g in
    play_round g
  | 3 ->
    let g = if Model.raise_resolved g then play_stage g
      else play_stage (play_raise g) in
    let g = Model.deal_1_to_table g in
    play_round g
  | 4 ->
    let g = if Model.raise_resolved g then play_stage g
      else play_stage (play_raise g) in
    let g = Model.deal_1_to_table g in
    play_round g
  | 5 ->
    let g = if Model.raise_resolved g then play_stage g
      else play_stage (play_raise g) in
    let winner = Roundwinner.get_round_winner g in
    let winhand =
      Roundwinner.get_phand_and_clst
        (Model.get_player_cards winner @ Model.get_table g) in
    View.print_next_round g winner winhand;
    Model.end_round g winner
  | _ -> failwith "main - play_round - 0!"

(**[play g] plays game [g] and prints the winner of [g]
   Requires: [g] is a valid game*)
and play (g:Model.game) : unit =
  let r = play_round g
  in let p = Analytics.get_winner r
  in match p with
  | Some p -> View.print_winner p r
  | None -> play r

(**[main ()] prints out the opening user prompt and instantiates a game
   with user input. Depending on the input, [main ()] will call itself again
   to ensure that a valid game can be instantiated, should the user enter a
   valid game initalization.*)
let main () : unit =
  ANSITerminal.(print_string [default_color]
                  "\n\nWelcome to Texas Hold 'Em (CS 3110 edition)!\n\n");
  print_string [default_color]
    "For each player you want in the game, please enter a name, whether they
are an AI or human, and their starting chip stack, followed by a comma.\n\n
EXAMPLE: 'Picard human 500, Data ai 700,' is a two-person game with Picard, a
user-controlled character with 500 chips, and Data, an ai-controlled character
with 700 chips.\n\nOther options are\n'basic','basic-ai',... Explore them!\n";
  let rec helper () : unit =
    print_string  [cyan] "> ";
    try let x = read_line ()
      in match x with
      | exception End_of_file -> print_string [] "foo"; ()
      | str -> try let params = Parser.parse_instantiation str in
          let g = Model.instantiate params in
          try play g; () with GameDone -> ()
                            | GameSaved i -> ()

        with  
          Sys_error _ -> helper ()
    with
    | Parser.Retrieve i -> let g = Save_game.retrieve_game i in
      View.print_retrieve();
      play g
    | Parser.PrematureExit -> View.print_exit ();
    | Parser.Empty -> View.print_malformed_ins (); helper ()
    | Parser.Malformed -> View.print_malformed_ins (); helper ()
    | Model.InvalidInstantiation -> View.print_invalid_ins (); helper ()
  in helper ()

let () = main ()
