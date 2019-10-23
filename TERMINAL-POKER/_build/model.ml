exception NoPlayersLeft
exception NotEnoughChips
exception CantRaise
exception CantCheck
exception IllegalPlay
exception InvalidInstantiation

type suit = Hearts | Clubs | Diamonds | Spades

type card = {
  value:int;
  suit:suit;
}

type player_type = Human | AI of (int * int)

type player_info = string * int * player_type

type player = {
  name:string;
  cards:card list;
  chips:int;
  amt_bet:int;
  folded:bool;
  player_type:player_type;
  next:int;
}

type game = {
  pot:int;
  players:player list;
  table:card list;
  deck:card list;
  min_bet:int;
  current_player:int;
  log:string list;
  open_cards:bool;
}

(**[build_deck ()] is deck [d] where [d] contains all cards in a standard
   52-card deck.
   NOTE: Aces are input with value 14, but in hand evaluation they are also given a
   value of 1 if needed for a straight *)
let build_deck () = [
  {value=14;suit=Hearts};{value=14;suit=Clubs};{value=14;suit=Diamonds};
  {value=14;suit=Spades};
  {value=13;suit=Hearts};{value=13;suit=Clubs};{value=13;suit=Diamonds};
  {value=13;suit=Spades};
  {value=12;suit=Hearts};{value=12;suit=Clubs};{value=12;suit=Diamonds};
  {value=12;suit=Spades};
  {value=11;suit=Hearts};{value=11;suit=Clubs};{value=11;suit=Diamonds};
  {value=11;suit=Spades};
  {value=10;suit=Hearts};{value=10;suit=Clubs};{value=10;suit=Diamonds};
  {value=10;suit=Spades};
  {value=9;suit=Hearts};{value=9;suit=Clubs};{value=9;suit=Diamonds};
  {value=9;suit=Spades};
  {value=8;suit=Hearts};{value=8;suit=Clubs};{value=8;suit=Diamonds};
  {value=8;suit=Spades};
  {value=7;suit=Hearts};{value=7;suit=Clubs};{value=7;suit=Diamonds};
  {value=7;suit=Spades};
  {value=6;suit=Hearts};{value=6;suit=Clubs};{value=6;suit=Diamonds};
  {value=6;suit=Spades};
  {value=5;suit=Hearts};{value=5;suit=Clubs};{value=5;suit=Diamonds};
  {value=5;suit=Spades};
  {value=4;suit=Hearts};{value=4;suit=Clubs};{value=4;suit=Diamonds};
  {value=4;suit=Spades};
  {value=3;suit=Hearts};{value=3;suit=Clubs};{value=3;suit=Diamonds};
  {value=3;suit=Spades};
  {value=2;suit=Hearts};{value=2;suit=Clubs};{value=2;suit=Diamonds};
  {value=2;suit=Spades};
]

let get_card_value (c:card) : int = c.value

let get_card_suit (c:card) : suit = c.suit

let has_folded (p:player) : bool = p.folded

let get_name (p:player) : string = p.name

let get_player_cards (p:player) : card list = p.cards

let get_player_type (p:player) : player_type = p.player_type

let get_chips (p:player) : int = p.chips

let get_player_bet (p:player) : int = p.amt_bet

let get_players (g:game) : player list = g.players

let get_pot (g:game) : int = g.pot

let get_deck (g:game) : card list = g.deck

let get_table (g:game) : card list = g.table

let get_min_bet (g:game) : int = g.min_bet

let get_log (g:game) : string list = g.log

let get_open_type (g:game) : bool = g.open_cards

let get_current_player (g:game) : player =
  try List.nth (g.players) (g.current_player)
  with _ -> raise NoPlayersLeft

let get_next_player (g:game) : player =
  if g.current_player = -1 then raise NoPlayersLeft
  else try
      let rec find_next_unfolded (p:player) : player =
        if p.folded then
          find_next_unfolded (List.nth (g.players) (p.next))
        else p
      in find_next_unfolded (List.nth (g.players) (g.current_player + 1))
    with _ -> raise NoPlayersLeft

(**[next_player_index g] is the index of the next player to go in [g]
   Requires: [g] is a valid game
*)
let next_player_index (g:game) : int =
  try let rec find_next_unfolded (n:int) : int =
        let p = List.nth (g.players) n in
        if p.folded then
          find_next_unfolded (n+1)
        else n
    in
    find_next_unfolded (g.current_player + 1)
  with _ -> -1

let raise_pot (g:game) (amt:int) : game =
  try let p = get_current_player g in
    if amt < 0 then raise IllegalPlay;
    let amt_1 = if p.amt_bet = g.min_bet then amt
      else (g.min_bet - p.amt_bet + amt) in
    let new_players = List.map (
        fun x -> if x = p then
            if p.chips >= amt_1 then {p with
                                      chips = p.chips - amt_1;
                                      amt_bet = p.amt_bet + amt_1
                                     }
            else if g.min_bet - p.amt_bet > p.chips then
              raise NotEnoughChips
            else raise CantRaise
          else x) g.players
    in {g with
        pot = g.pot + amt_1;
        min_bet = g.min_bet + amt;
        players = new_players;
        current_player = next_player_index g;
        log = (p.name ^ " raised by " ^ (string_of_int amt))::g.log
       }
  with
  | NotEnoughChips -> raise NotEnoughChips
  | NoPlayersLeft -> raise NoPlayersLeft
  | CantRaise -> raise CantRaise
  | IllegalPlay -> raise IllegalPlay

let match_pot (g:game) : game =
  try let p = get_current_player g in
    let amt = g.min_bet - p.amt_bet in
    let new_players = List.map (
        fun x -> if x = p then
            if p.chips >= amt then {p with
                                    chips = p.chips - amt;
                                    amt_bet = p.amt_bet + amt
                                   }
            else raise NotEnoughChips
          else x) g.players
    in {g with
        pot = g.pot + amt;
        min_bet = g.min_bet;
        players = new_players;
        current_player = next_player_index g;
        log = (p.name ^ " matched")::g.log
       }
  with
  | NoPlayersLeft -> raise NoPlayersLeft
  | NotEnoughChips -> raise NotEnoughChips

let check (g:game) : game =
  try let p = get_current_player g in
    if p.amt_bet >= g.min_bet then
      {g with
       current_player = next_player_index g;
       log = (p.name ^ " checked")::g.log
      }
    else raise CantCheck
  with
  | NoPlayersLeft -> raise NoPlayersLeft
  | CantCheck -> raise CantCheck

let fold (g:game) : game =
  try let p = get_current_player g in
    let new_players = List.map (
        fun x -> if x = p then {x with folded=true} else x
      ) g.players
    in {g with
        players = new_players;
        current_player = next_player_index g;
        log = (p.name ^ " folded")::g.log
       }
  with NoPlayersLeft -> raise NoPlayersLeft

(**[pop_card d] is a tuple [c,d'] where [c] is the first card of [d]
   and [d'] is [d] without its first card*)
let pop_card (d:card list) : card * (card list) =
  match d with
  | [] -> failwith "Empty deck"
  | h::t -> (h,t)

let deal_1_to_table (g:game) : game =
  let tup = pop_card g.deck in
  {g with table = (fst tup)::g.table; deck = snd tup;}

let deal_3_to_table (g:game) : game =
  g |> deal_1_to_table |> deal_1_to_table |> deal_1_to_table

(**[deal_2_to_players p_list acc d] is [d','p_list], where [d'] is the deck
   missing the cards dealt to players in [p_list] to form [p_list'], in which
   each player in [p_list] has been dealt two cards from [d]*)
let rec deal_2_to_players (p_list:player list) (acc:player list) (d:card list) :
  (player list) * (card list) =
  match p_list with
  | [] -> (acc,d)
  | h::t -> let tup1 = pop_card d in
    let tup2 = pop_card (snd tup1) in
    let acc = ({h with cards = (fst tup1)::[(fst tup2)]}::acc) in
    deal_2_to_players t acc (snd tup2)

(**[shuffle c] is [c] after shuffling*)
let shuffle (c:card list) : card list =
  Random.self_init ();
  let rand_times = (Random.int 450) + 450 in
  let rec shuffle_helper (c:card list) (n:int) : card list =
    let random_sort (c1:card) (c2:card) = (Random.int 3) - 1 in
    let shuffled = List.sort random_sort c in
    if n = 1 then shuffled
    else shuffle_helper shuffled (n-1) in
  shuffle_helper c rand_times

let get_new_deck () =
  shuffle (build_deck ())

(**[fix_next_player p_list] is [p_list'], such that the [next] field of each
   player in [p] list is the index of the player whose turn comes after [p],
   unless [p] is the last player, in which [-1] is used.*)
let fix_next_player (p_list:player list) : player list =
  List.mapi (fun i p ->
      if i = ((List.length p_list) - 1) then
        {p with next = -1}
      else
        {p with next = (i+1)}
    ) p_list


(**[bet_n g n] is [g] after each player has either bet [n] chips, or forfeited
   the game due to initially have less than [n] chips.
   Requires: [g] is a valid game
          At least one player has at least [n] chips*)
let bet_n (g:game) (n:int) : game =
  let players = get_players g in
  let players =
    List.filter (fun p -> if get_chips p < n then false else true) players in
  let players = fix_next_player players in
  let players = List.map (fun p ->
      {p with
       amt_bet = p.amt_bet + n;
       chips = p.chips - n;
      }) players in
  {g with
   players = players;
   pot = g.pot + (n * (List.length players));
   min_bet = g.min_bet + n;
  }

let end_round (g:game) (winner:player) : game =
  let winnings = g.pot in
  let players = List.map (
      (*gives the winner the pot*)
      fun p -> let new_chips = if p = winner then p.chips + g.pot else p.chips
        in {p with
            chips = new_chips;
            amt_bet = 0;
            folded = false;
           }
    ) g.players
  in
  (*[filter_and_log p_list acc log] is a tuple of the suriving players and a
    log describing the deaths of those that have lost*)
  let rec filter_and_log (p_list:player list) (acc:player list)
      (log:string list) : (player list) * (string list) =
    match p_list with
    | [] -> (List.rev acc),log
    | h::t -> if h.chips > 0 then filter_and_log t (h::acc) log
      else filter_and_log t acc ((h.name ^ " lost!")::log)
  in
  let players,log = filter_and_log players [] [] in
  let players = fix_next_player players in
  let players,deck = deal_2_to_players players [] (get_new_deck ()) in
  let round_description = winner.name ^ " won " ^ (string_of_int winnings) in
  let g = {g with
           pot = 0;
           players = players;
           table = [];
           deck = deck;
           min_bet = 0;
           current_player = 0;
           log = round_description::(List.append log g.log);
          } in
  bet_n g 1

let raise_resolved (g:game) : bool =
  let min_bet = g.min_bet in
  let rec helper (p_list:player list) (acc:bool) : bool =
    match p_list with
    | [] -> acc
    | h::t -> helper t acc && ((h.amt_bet = min_bet) || h.folded)
  in helper g.players true


let reset_current_player (g:game) : game =
  try let rec find_next_unfolded (n:int) : int =
        let p = List.nth (g.players) n in
        if p.folded then
          find_next_unfolded (n+1)
        else n
    in
    let current_player_index = find_next_unfolded 0 in
    {g with current_player = current_player_index}
  with _ -> raise NoPlayersLeft


(*[instantiate p_info] is a game [g] with [players] from p_info*)
let instantiate (info:(player_info list) * bool) : game =
  let print_type = snd info in
  let p_info = fst info in
  let rec build_players (n:int) (acc:player list) : player list =
    if n = -1 then match acc with
      | [] -> acc
      | h::t -> List.rev ({h with next = -1}::t)
    else let name,chips,info = List.nth p_info n in
      let p = {
        name = name;
        cards = [];
        amt_bet = 0;
        chips = chips;
        folded = false;
        player_type = info;
        next = (n+1);
      } in build_players (n-1) (p::acc)
  in
  let players = build_players ((List.length p_info) - 1) [] in
  let players,deck =  deal_2_to_players players [] (get_new_deck ()) in
  let g = {
    pot = 0;
    players = players;
    table = [];
    deck = deck;
    min_bet = 0;
    current_player = 0;
    log=[];
    open_cards = print_type;
  } in
  bet_n g 1
