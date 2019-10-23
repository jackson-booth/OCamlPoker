open Model
open ANSITerminal

(**Our default color*)
let default_color = green

(*Documentation in the .mli*)
let print_exit () : unit =
  print_string [red;Bold] "EXITING GAME (come back!)\n"

(*Documentation in the .mli*)
let print_save (file:string) : unit =
  print_string [red;Bold] "GAME SAVED, EXITTING GAME";
  print_string [red;] ("You can retrieve the saved game by bringing up " ^
                       "the screen to make new game, and typing 'retrieve " ^
                       file ^ "')\n")

(*Documentation in the .mli*)
let print_retrieve () : unit =
  print_string [red;Bold] "GAME RETRIEVED, CONTINUE POKING\n"

(*Documentation in the .mli*)
let print_negative () : unit =
  print_string [red;Bold] ("\nAck! You can't raise by a negative number " ^
                           "Please try again\n")
(*Documentation in the .mli*)
let print_notInteger () : unit =
  print_string [red;Bold] ("\nAck! You can't raise by a non integer " ^
                           "Please try again\n")
(*Documentation in the .mli*)
let print_play_help () : unit =
  print_string [red;Bold] "VALID COMMANDS ARE:\n";
  print_string [default] "'raise i' where 'i' is a non-negative integer\n";
  print_string [default] "'bet i' where 'i' is a non-negative integer\n";
  print_string [default] "'match'\n";
  print_string [default] "'check'\n";
  print_string [default] "'fold'\n";
  print_string [default] "'exit' exits the game\n";
  print_string [default;Bold] "(Enter any key to continue)"

(*Documentation in the .mli*)
let print_malformed_cmd () : unit =
  print_string [red] ("\nThat command wasn't valid :(." ^
                      "Here's some help tips!\n");
  print_play_help ();
  read_line ();
  erase Above

(*Documentation in the .mli*)
let print_help () : unit =
  print_string [red;Bold] "HELP TO PLAY POKER:\n";
  print_string [default]
    ("https://www.partypoker.com/en/how-to-play/texas-holdem gives a good " ^
     "overview of the rules of the poker we implemented.
  https://www.partypoker.com/en/how-to-play/hand-rankings gives a good " ^
     " overview of the card rankings we use.\nNote that we don't fully " ^
     "implement these rules or card rankings--yet.\n\n");
  print_play_help ();
  read_line ();
  erase Above

(*Documentation in the .mli*)
let print_malformed_ins () : unit =
  let cursor_pos = pos_cursor () in
  print_string [red] "\nThat wasn't a valid way to instatiate a game! ";
  print_string [default;Bold] "(press enter to try again)\n";
  read_line ();
  erase Below



(*Documentation in the .mli*)
let print_invalid_ins () : unit =
  let cursor_pos = pos_cursor () in
  print_string [red] "\nThose inputs won't instantiate a valid game ";
  print_string [default;Bold] "(press enter to try again)\n";
  read_line ();
  erase Below

(*Documentation in the .mli*)
let print_too_few_chips () : unit =
  print_string [red] ("\nAck! You don't have enough chips to keep playing, "^
                      "so you folded instead!\n");
  Unix.sleepf 2.5

(*Documentation in the .mli*)
let print_cant_raise () =
  let cursor_pos = pos_cursor () in
  print_string [red] ("\nAck! You don't have enough " ^
                      "chips raise or bet by that much! ");
  print_string [default;Bold] "(press enter to try again)\n";
  read_line ();
  erase Below

(*Documentation in the .mli*)
let print_invalid_instrn () =
  let cursor_pos = pos_cursor () in
  print_string [red] ("\nYou can probably guess why that move wasn't valid!");
  print_string [default;Bold] "(press enter to try again)\n";
  read_line ();
  erase Below

(*Documentation in the .mli*)
let print_cant_check () =
  let cursor_pos = pos_cursor () in
  print_string [red] ("\nYou need to match the pot before you can check!");
  print_string [default;Bold] "(press enter to try again)\n";
  read_line ();
  erase Below

(*Documentation in the .mli*)
let file_not_found () =
  let cursor_pos = pos_cursor () in
  print_string [red;Bold] "\nAck!  File Not Found!\n";
  print_string [default;] ("Are you sure you entered the right file_name, " ^
                           "maybe you forgot .json!");
  print_string [default;Bold] "(press enter to try again)\n";
  read_line ();
  erase Below

(**[card_num_to_string c] is the card representation of card [c]
   Raises: fails if 14 < [i] < 2, where [i] is the value of [c]
   Example: [card_num_to_string 14] is ["Ace"]
         [card_num_to_string 2] is ["Two"]*)
let card_num_to_string (c:Model.card) : string =
  match Model.get_card_value c with
  | 14 -> "│  A  │"
  | 13 -> "│  K  │"
  | 12 -> "│  Q  │"
  | 11 -> "│  J  │"
  | 10 -> "│ 1 0 │"
  | 9 ->  "│  9  │"
  | 8 ->  "│  8  │"
  | 7 ->  "│  7  │"
  | 6 ->  "│  6  │"
  | 5 ->  "│  5  │"
  | 4 ->  "│  4  │"
  | 3 ->  "│  3  │"
  | 2 ->  "│  2  │"
  | _ -> failwith "invalid card"

(**[style_card c] is a list of the styles to use when displaying card [c]*)
let style_card (c: Model.card) : (style list) =
  match Model.get_card_suit c with
  | Hearts -> [red]
  | Clubs -> [default]
  | Diamonds -> [red]
  | Spades -> [default]

(**[card_bottom_suit_to_string c] is the style of the bottom left hand
   corner of a printed card*)
let card_bottom_suit_to_string (c:Model.card) : string =
  match Model.get_card_suit c,Model.get_card_value c with
  | Hearts,i ->   "│♥️    │"
  | Clubs,i ->    "│♣️    │"
  | Diamonds,i -> "|♦️    │"
  | Spades,i ->   "│♠️    │"

(**[card_top_suit_to_string c] is the style of the top right hand
   corner of a printed card*)
let card_top_suit_to_string (c:Model.card) : string =
  match Model.get_card_suit c,Model.get_card_value c with
  | Hearts,i ->   "│    ♥️│"
  | Clubs,i ->    "│    ♣️│"
  | Diamonds,i -> "│    ♦️│"
  | Spades,i ->   "│    ♠️|"

(**[top_card] is the top of a default card*)
let top_card : string =    "┌─────┐"

(**[card_space] is the middle of a default card*)
let card_space : string =  "│     │"

(**[bottom_card] is the bottom of a default card*)
let bottom_card : string = "└─────┘"

(**[space_between_cards] is space between cards*)
let space_between_cards : string = "   "

(**[print_top_cards c_list] prints the tops of the cards in [c_list],
in the order they appear in [c_list], nicely.*)
let rec print_top_cards (cards:Model.card list) : unit =
  match cards with
  | [] -> print_string [] "\n"
  | h::t -> print_string (style_card h) (space_between_cards ^ top_card);
    print_top_cards t

(**[print_cards_top_suites c_list] prints the top suit markings of the cards
in [c_list], in the order they appear in [c_list], nicely.*)
let rec print_cards_top_suites (cards:Model.card list) : unit =
  match cards with
  | [] -> print_string [] "\n"
  | h::t -> print_string (style_card h) (space_between_cards ^
                                         card_top_suit_to_string h);
                                         print_cards_top_suites t

(**[print_cards_spaces c_list] prints the spaces of the cards
in [c_list], in the order they appear in [c_list], nicely.*)
let rec print_cards_spaces (cards:Model.card list) : unit =
  match cards with
  | [] -> print_string [] "\n"
  | h::t -> print_string (style_card h) (space_between_cards ^ card_space);
    print_cards_spaces t

(**[print_cards_spaces c_list] prints the numbers (or faces) of the cards
in [c_list], in the order they appear in [c_list], nicely.*)
let rec print_cards_nums (cards:Model.card list) : unit =
  match cards with
  | [] -> print_string [] "\n"
  | h::t -> print_string (style_card h) (space_between_cards ^
                                         card_num_to_string h);
                                         print_cards_nums t

(**[print_cards_top_suites c_list] prints the bottom suit markings of the
cards in [c_list], in the order they appear in [c_list], nicely.*)
let rec print_cards_bottom_suites (cards:Model.card list) : unit =
  match cards with
  | [] -> print_string [] "\n"
  | h::t -> print_string (style_card h) (space_between_cards ^
                                         card_bottom_suit_to_string h);
    print_cards_bottom_suites t

(**[print_cards_top_suites c_list] prints the bottoms of the
cards in [c_list], in the order they appear in [c_list], nicely.*)
let rec print_bottom_cards (cards:Model.card list) : unit =
  match cards with
  | [] -> print_string [] "\n"
  | h::t -> print_string (style_card h) (space_between_cards ^
                                         bottom_card);
    print_bottom_cards t

(**[print_cards c] prints [string] representations of the cards in [c]*)
let print_cards (cards:Model.card list) : unit =
  print_top_cards cards;
  print_cards_top_suites cards;
  print_cards_nums cards;
  print_cards_bottom_suites cards;
  print_bottom_cards cards

(**[print_next_round w] prints a screen showing who won the previous round,
   what hand they won with, and the amount of chips they won *)
let print_next_round (g:Model.game) (winner:Model.player)
    (hand:string * card list) : unit =
  erase Above;
  print_string [cyan] ("\n\n\n" ^ (Model.get_name winner) ^ " won " ^
                       (string_of_int (Model.get_pot g) ^ " chips with a " ^
                        (fst hand) ^ "\n\n"));
  print_cards (snd hand);
  print_string [yellow]
    "This round is now over. Press any enter to continue.";
  read_line ();
  erase Above

(*Documentation in the .mli*)
let print_winner (p:Model.player) (g:Model.game) : unit =
  print_string [cyan] ("\n\n\n" ^ (Model.get_name p) ^
                       " won with private cards\n");
  print_cards (Model.get_player_cards p)

(**[player_to_string p] is the [string] representations player [p]*)
let player_to_string (p:Model.player) : string =
  (Model.get_name p) ^
  "  -  Chips: " ^ (string_of_int (Model.get_chips p)) ^
  "  -  Has folded: " ^ (string_of_bool (Model.has_folded p) ^
                         "  -  Amount bet: " ^
                         (string_of_int (Model.get_player_bet p)))

(**[print_players p] prints [string] representations of the players in [c]*)
let print_players (current_p: Model.player) (player_list:Model.player list)
    (fstn: int) : unit =
  let rec get_first_n (n:int) (ps:Model.player list) (acc:Model.player list) =
    match ps with
    | [] -> acc
    | h::t -> if n=0 then acc else get_first_n (n-1) t (acc@[h]) in
  let rec get_n_players_from_p (p:Model.player) (n:int)
      (ps:Model.player list) =
    match ps with
    | [] -> []
    | h::t ->
      if h = p then get_first_n n ps []
      else get_n_players_from_p p n t in
  let pl = get_n_players_from_p current_p fstn (player_list@player_list) in
  let rec print_players_helper (cp:Model.player) (players:Model.player list)
    : unit =
    match players with
    | [] ->
      if fstn < (List.length players)
      then print_string [cyan] "More Players..." else ()
    | h::t -> if h = cp then
        let _ = print_string [green;Bold] ("-> " ^
                                           (player_to_string h) ^ "\n") in
        print_players_helper cp t
      else let _ = print_string [green] ((player_to_string h) ^ "\n") in
        print_players_helper cp t
  in print_players_helper current_p pl

(**[print_line_separation ()] is a nice line separator.**)
let rec print_history (plays:string list) (num_prints:int) : unit =
  let rec get_first_n (n: int) (ps: string list) (acc: string list) =
    match ps with
    | [] -> acc
    | h::t -> if n=0 then acc else get_first_n (n-1) t (acc@[h])
  in let log = get_first_n (num_prints) plays [] in
  List.iter (fun x -> print_string [yellow] (x^"\n")) (List.rev log);
  print_string [] "\n"

(**[print_line_separation ()] is a nice line separator.*)
let print_line_separation () =
  print_string [default_color]
    ("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

(**[print_top_box stage p_name] nicely prints the stage of a game and the
   name of the game's current player.

   This method is meant to be called only from print_game.**)
let print_top_box (stage:string) (p_name:string) : unit =
  print_string [default_color] stage;
  print_string [default_color] ("        Current Player: " ^ p_name ^ "\n");
  print_line_separation()

(**[print_bottom_box t_cards p players g] is nicely formatted information
  about the table, player statuses, pot, minimum bet, and history of game [g],
  where [t_cards] are the cards on the table of [g], [p] is the current player
  of [g], and [players] are the players of [g].

   This method is meant to be called only from print_game.**)
let print_bottom_box (t_cards:card list) (p:player) (players:player list)
    (g:game) : unit =
  print_string [cyan] "Table Cards\n";
  print_line_separation ();
  print_cards t_cards;
  print_line_separation ();
  print_string [cyan] "Game Info\n";
  print_string [default_color] ("Minimum bet: " ^
                                (string_of_int (Model.get_min_bet g)) ^ "\n");
  print_string [default_color] ("Pot: " ^
                                (string_of_int (Model.get_pot g)) ^ "\n");
  print_string [default_color] "Player information:\n";
  print_players p players (min 6 (List.length (Model.get_players g)));
  print_line_separation();
  print_string [cyan] "History\n";
  print_history (Model.get_log g) (min 6 (List.length (Model.get_log g)))

(*Documented in the .mli*)
let print_game ?open_cards:(open_cards=false) (g:Model.game) : unit =
  let p = Model.get_current_player g in
  let p_cards = Model.get_player_cards p in
  let p_name = Model.get_name p in
  let t_cards = Model.get_table g in
  let stage = if (List.length t_cards) = 0 then "The Pre-Flop" else
    if (List.length t_cards) = 3 then "The Flop" else
    if (List.length t_cards) = 4 then "The Turn" else
    if (List.length t_cards) = 5 then "The River" else "" in
  let players = Model.get_players g in
  let player_type = p |> Model.get_player_type in
  print_string [] "";
  match player_type with
  | Human -> (*erase Above*)
    if Model.get_open_type g || open_cards then
      (print_top_box stage p_name;
       print_string [cyan] (p_name ^ "'s Cards\n"); print_cards p_cards;
       print_bottom_box t_cards p players g)
    else
      (print_top_box stage p_name;
       print_string [default_color] ("If you are " ^ p_name ^
                                     ", then press enter.\n\n\n\n\n\n");
       print_bottom_box t_cards p players g;
       read_line ();
       (* erase Above; *)
       print_top_box stage p_name;
       print_string [cyan] (p_name ^ "'s Cards\n"); print_cards p_cards;
       print_bottom_box t_cards p players g)
  | AI _ -> Unix.sleepf 1.0; erase Above;
    if Model.get_open_type g || open_cards then
      (print_top_box stage p_name;
       print_string [cyan] (p_name ^ "'s Cards\n"); print_cards p_cards;
       print_bottom_box t_cards p players g)
    else
      (print_top_box stage p_name;
       print_string [default_color] "AI is playing.\n\n\n\n\n\n";
       print_bottom_box t_cards p players g)
