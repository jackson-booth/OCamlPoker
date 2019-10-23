open Model
open Yojson.Basic.Util

exception FileNotFound of string

(** [suit_to_string s] returns the String type of a suit
    Raises: Nothing
    Example: [suit_to_string Hearts] returns "Hearts"
    Requires: s to be a valid suit*)
let suit_to_string (s: suit) =
  match s with
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Spades -> "Spades"

(** [string_to_suit s] returns the suit type of a string
    Raises: Nothing
    Example: [string_to_suit "Hearts"] returns the suit Hearts
    Requires: Nothing*)
let string_to_suit (s: string) : suit =
  match s with
  | "Hearts" -> Hearts
  | "Clubs" -> Clubs
  | "Diamonds" -> Diamonds
  | "Spades" -> Spades
  | _ -> failwith "Invalid Suit"

(** [get_player_type pt acc] is a Yojson representation of [pt]
    Raises: Nothing
    Requires: pt to be a valid player_type
    Example: [get_player_type AI(1,2) []] returns ["AI":[1,2]] *)
let get_player_type (pt :player_type) (acc:Yojson.Basic.json list)
  : Yojson.Basic.json list =
  match pt with
  | Human -> let human = `Assoc[("Human", `List[])] in (human::acc)
  | AI (i,j) -> let ai =
                  `Assoc[("AI", `List[`String (string_of_int i);
                                      `String (string_of_int j)])] in (ai::acc)


(** [get_card_list lst acc] returns the Yojson representation of
    a card list, i.e it converts a card list to json
    Raises: Nothing
    Requires: Nothing
    Example: [get_card_list [{v=3, s=Hearts}; {v=4; s=Spades}; {v=4; s=Clubs};
                               {v=15; s=Diamonds}]] []] 
                               returns [{"value":3, "suit":"Hearts"},
                               {"value":4, "suit":"Spades"},
                               {"value":4, "suit":"Clubs"},
                               {"value":15, "suit":"Diamonds"},]
                             *)
let rec get_card_list (lst: card list)
    (acc: Yojson.Basic.json list) : Yojson.Basic.json list =
  match lst with
  | [] -> List.rev acc
  | h::t ->
    let suit_string = suit_to_string h.suit in
    let new_card =
      `Assoc[
        "value",`Int h.value;
        "suit", `String suit_string
      ] in get_card_list t (new_card::acc)

(** [players_to_json players acc] returns the Yojson representation of
    a player list, i.e it converts a player list to json
    Raises: Nothing
    Requires: Nothing
    Example: 
    [players_to_json [name=Picard, chips=10,amt_bet=3, folded=false,
    player_type= Human, next = -1,
     cards= [{v=9, s=Spades}; {v=4; s=Diamonds};]][]] 
               returns {
               		"name": "Picard",
               		"chips": 10,
               		"amt_bet": 3,
               		"folded": false,
               		"player_type": [{
               			"Human": []
               		}],
               		"next": -1,
               		"cards": [{
               			"value": 9,
               			"suit": "Spades"
               		}, {
               			"value": 4,
               			"suit": "Diamonds"
               		}]
               	}]*)
let rec players_to_json (players:player list)
    (acc: Yojson.Basic.json list): Yojson.Basic.json list =
  match players with
  | [] -> List.rev acc
  | h::t -> let cards_to_json = get_card_list h.cards [] in
    let p_type = get_player_type h.player_type [] in
    let new_json =
      `Assoc[
        ("name", `String h.name);
        ("chips", `Int h.chips);
        ("amt_bet", `Int h.amt_bet);
        ("folded", `Bool h.folded);
        ("player_type", `List p_type);
        ("next" , `Int h.next);
        ("cards", `List cards_to_json)
      ]
    in players_to_json t (new_json::acc)

(**[get_log_list logs] returns a valid json format of the logs
   Raises: Nothing
   Requires: Nothing
   Example: [get_log_list ["Piccard Matched"]] returns ["Piccard Matched"] *)
let rec get_log_list (logs:string list)
    (acc: Yojson.Basic.json list): Yojson.Basic.json list  =
  match logs with
  | [] -> List.rev acc
  | h::t -> let new_string = (`String h) in
    get_log_list t (new_string::acc)

(** [game_to_json g] returns the Yojson representation of
    the entire game, i.e it converts a game to json
    Requires: g to be a valid game
    Raises: Nothing
*)
let game_to_json (g:game) : Yojson.Basic.json =
  let players_json = players_to_json g.players [] in
  let deck_to_json = get_card_list g.deck [] in
  let table_to_json = get_card_list g.table [] in
  let game_log = get_log_list g.log [] in
  `Assoc
    [ ("players", `List players_json)
    ; ("pot", `Int g.pot)
    ; ("min_bet", `Int g.min_bet)
    ; ("current_player", `Int g.current_player)
    ; ("deck", `List deck_to_json)
    ; ("table", `List table_to_json)
    ; ("log", `List game_log)
    ; ("open_cards", `Bool g.open_cards)
    ]
(** [save_game game file_name] writes the game to a file named file_name
    Requires: g to be a valid game
    Raises: Nothing*)
let save_game (g:game) (file_name:string) : unit =
  let game_json = game_to_json g in
  Yojson.Basic.to_file (file_name ^ ".json") game_json

(** [extract_card_list cards acc] extracts card list from Yojson list.
    Requires: [cards] is a valid Yojson.Basic.json representation.
    Raises: Nothing *)
let rec extract_card_list (cards:Yojson.Basic.json list) (acc: card list)
  : (card list)=
  match cards with
  | [] -> List.rev acc
  | h::t ->
    let s_string =  to_string (List.assoc "suit" (to_assoc h)) in
    let card_s = string_to_suit s_string in
    let card_v = to_int (List.assoc "value" (to_assoc h)) in
    let new_card = {
      value = card_v;
      suit = card_s
    } in
    extract_card_list t (new_card::acc)

(** [extract_player_type types acc] returns a tuple of strings describing
    a valid player
    Raises: Nothing
    Requires: types to be a valid Yojson.Basic.json*)
let rec extract_player_type (types: Yojson.Basic.json list)
    (acc:(string * string ) list) : (string*string) =
  match types with
  | [] -> List.hd acc
  | h::t ->
    try let human = to_list  (List.assoc "Human" (to_assoc h)) in
      ("","")
    with Not_found -> let ai = to_list  (List.assoc "AI" (to_assoc h))  in
      (to_string (List.hd ai), to_string (List.hd ai))
(**[get_player_type tup] returns a valid player type 
   Requires: Nothing
   Raises: Nothing
   Exmaple:[get_player_type ("","")] returns Human 
*)
let get_player_type (tup : string*string) : player_type =
  if (fst tup) = "" && (snd tup) = "" 
  then Human else AI (int_of_string(fst tup)
                     , int_of_string (snd tup))


(** [extract_player_list cards acc] extracts player list from Yojson list.
    Requires: [cards] is a valid Yojson.Basic.json representation.
    Raises: Nothing
*)
let rec extract_players(players:Yojson.Basic.json list) (acc: player list)
  : (player list) =
  match players with
  | [] -> List.rev acc
  | h::t ->
    let player_name = to_string (List.assoc "name" (to_assoc h)) in
    let player_chips = to_int (List.assoc "chips" (to_assoc h)) in
    let player_amt_bet = to_int (List.assoc "amt_bet" (to_assoc h)) in
    let player_folded = to_bool (List.assoc "folded" (to_assoc h)) in
    let type_list = to_list (List.assoc "player_type" (to_assoc h)) in
    let player_type = get_player_type (extract_player_type type_list []) in
    let player_next = to_int (List.assoc "next" (to_assoc h)) in
    let card_list = to_list (List.assoc "cards" (to_assoc h)) in
    let player_cards = extract_card_list card_list [] in
    let new_player =
      {
        name = player_name;
        chips = player_chips;
        amt_bet = player_amt_bet;
        folded = player_folded;
        player_type = player_type;
        next = player_next;
        cards = player_cards
      }
    in extract_players t (new_player::acc)

(** [extract_log_list logs acc] returns a string list of the logs
    Raises: Nothing
    Requires: logs to be a valid Yojson.Basic.json  *)
let rec extract_log_list (logs: Yojson.Basic.json list) (acc: string list)
  : (string list)=
  match logs with
  | [] -> List.rev acc
  | h::t -> let new_string = to_string h in
    extract_log_list t (new_string::acc)

(** [from_json j] is the game that [j] represents.
    Requires: [j] is a valid JSON game representation.
    Raises: Nothing *)
let from_json json : game =
  let game_pot = to_int (List.assoc "pot" (to_assoc json)) in
  let game_min_bet = to_int (List.assoc "min_bet" (to_assoc json)) in
  let game_current_player = to_int(List.assoc "current_player"(to_assoc json))in
  let player_list = to_list (List.assoc "players" (to_assoc json)) in
  let game_players = extract_players player_list [] in
  let deck_list = to_list (List.assoc "deck" (to_assoc json)) in
  let game_deck = extract_card_list deck_list [] in
  let table_cards = to_list (List.assoc "table" (to_assoc json)) in
  let game_table = extract_card_list table_cards  [] in
  let json_log = to_list (List.assoc "log"(to_assoc json)) in
  let game_log = extract_log_list json_log [] in
  let game_open_cards = to_bool(List.assoc "open_cards" (to_assoc json)) in
  {
    players = game_players;
    pot = game_pot;
    current_player = game_current_player;
    deck = game_deck;
    min_bet = game_min_bet;
    table = game_table;
    log = game_log;
    open_cards = game_open_cards
  }
(** [retrieve_game file_name] reads the game from a file named file_name
    and returns a valid game representation
    Raises: FileNotFound s if found is not found
    Requires: Nothing*)
let retrieve_game (file_name:string) : game =
  try
    let j = Yojson.Basic.from_file (file_name ^ ".json")   in
    from_json j
  with Sys_error s -> raise (FileNotFound s)
