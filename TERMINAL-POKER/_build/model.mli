(**A module to help represent the state of the game, updating the state of
a game, and getting information about the game*)

(**thrown when there are no longer players to move in a stage*)
exception NoPlayersLeft

(**thrown when a player possesses too few chips to stay in the game*)
exception NotEnoughChips

(**thrown when a player possesses too few chips to raise*)
exception CantRaise

(**thrown when a player has not bet enough to check*)
exception CantCheck

(**thrown when a player does an illegal play, eg. raising by a negative
   amount, etc.*)
exception IllegalPlay

(**thrown when a set of initialization parameters would create an invalid
   game*)
exception InvalidInstantiation

(**The suit of a card*)
type suit = Hearts | Clubs | Diamonds | Spades

(**[card] is the type of a card
   Abstraction function: A [card] c contains a suit that must be one of the 
   following: Hearts, Clubs, Diamonds, Spades. It must also have an integer 
   value. 
   Representation Invariants: A [card] c is a unique card meaning it has a
   unique suit and value combination. It must also be one of the
   following values: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 where 2 through
   10 represent the their respective values in a standard 52 card deck and 11
   through 14 represent Jack, Queen, King, and Ace respectively. There are 13
   possible values for each suite for a total of 52 total cards. **)
type card = {value:int;suit:suit}

(**A player can be either a human or an AI with an aggressiveness and a skill.
Due to our implementation of ai.ml, any integer value of either aggressiveness
or skill is acceptable, with 0 being a default.
*)
type player_type = Human | AI of (int * int)

(**[player] is the type of a player
   Abstraction function: A [player] p in a [game] g is a player with
   name [name], cards in hand [cards], chips owned [chips], amount bet
   per stage [amt_bet], folded flag [folded], person flag [is_person].
   and the index of the player that comes after in the game g [next].
   Representation Invariants:
   - A player must have two cards in their card list
   - A player's [chips] must be non-negative
   - A player's [amt_bet] must be non-negative
   - A player's [next] must be a valid index in the list of players
     in the game [g] in which a player plays, or -1. Additionally, [next]
     may not be zero or its own index in [g]'s list of players.
     -*)
type player = {
  name:string;
  cards:card list;
  chips:int;
  amt_bet:int;
  folded:bool;
  player_type:player_type;
  next:int;
}

(**[game] is the type of a game.
   Abstraction Function: A represents a game in with [pot] chips in its pot,
   players [players], cards on the table the cards of list [table], yet-undealt
   cards the cards of card list [deck], minimum bet is [min_bet], current player
   (index) [current_player], and log [log].
   Representation Invariants:
   - The pot must be positive
   - A game must have at least one player, and each player in [p_list]
     is a valid player
   - Each player in the game possesses exactly two cards
   - All the cards in the game (private to a player, on the table, or
     yet undealt) must together make up k standard 52-card decks, where k
     is a whole number.
   - min_bet must be a non-negative integer, and must be lest than
     -current_player is either a valid index of the players list, or -1*)
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

(**[get_suit c] is the suit of card [c]*)
val get_card_suit : card -> suit

(**[get_card_value c] is the value of card [c]*)
val get_card_value : card -> int

(**[get_name p] is the name of player [p]
   Requires: [g] is a valid game *)
val get_name : player -> string

(**[get_chips p] is the number of chips owned by player [p]*)
val get_chips : player -> int

(**[get_player_cards p] is a [card list] of the cards held by player
   [p]*)
val get_player_cards : player -> card list

(**[get_player_bet p] is the amount player [p] has bet*)
val get_player_bet : player -> int

(**[get_player_type p] the player_type of [p]*)
val get_player_type : player -> player_type

(**[has_folded p] is if [p] has folded*)
val has_folded : player -> bool

(**[get_current_player g] is [Some p] where [p] is the current
   player in [g], or [None] if there are no more players in [g] that
   can play in the stage*)
val get_current_player : game -> player

(**[get_next_player g] is [p] where [p] is the next player to go (and not folded)
   in [g]
   Raises: [NoPlayersLeft] if no players can move
   Requires: [g] is a valid game
 *)
val get_next_player : game -> player

(**[get_pot g] is the number of chips in the pot of [g]
   Requires: [g] is a valid game *)
val get_pot : game -> int

(**[get_players g] is the players of [g] as a [player list]*)
val get_players : game -> player list

(**[get_deck g] is the deck of [g] as a [deck]. Specifically, the
   cards that have not yet been dealt or put on the table.*)
val get_deck : game -> card list

(**[get_table g] is the cards on the table of [g] as a card list.*)
val get_table : game -> card list

(**[get_min_bet g] is the minimum bet of [g]*)
val get_min_bet : game -> int

(**[get_log g] is the log of the plays of [g]*)
val get_log : game -> string list

(**[get_open_type g] is [true] if [g] is an open game, that is,
all players can see the cards of other players*)
val get_open_type : game -> bool

(**[deal_1_to_table g] is [g] after 1 [card] has been added to the the table
of [g] from the deck of [g].
   Requires: [g] is a valid game, [deck] is not empty
 *)
val deal_1_to_table : game -> game

(**[deal_3_to_table g] is [g] after 3 [cards] have been added to the table
of [g] from the deck of [g].
   Requires: [g] is a valid game, [deck] has at least 3 [cards]
 *)
val deal_3_to_table : game -> game

(**[raise_pot g i] is [g] but after the player represented by the
   current player of [g] has raised by amount [i], and the
   current_player of [g] is set to the next player in [g] to take a
   turn in [g] (as determined by poker rules), and the the log of [g]
   records the raise.
   Requires: [g] is a valid game
   Returns: the updated game after the raise
   Raises: [NotEnoughChips] if the current player has too few chips to
        stay unfolded
        [NoPlayersLeft] if the current_player of [g] was -1
        [CantRaise] if the current player of [g] has too few chips
        to raise by [i], but enough to stay in the game
        [IllegalPlay] if the raise was by a negative amount*)
val raise_pot : game -> int -> game

(**[match_pot g] is [g] but after player the current player has
   matched the pot, and the current_player of [g] is set to the next
   player in [g] to take a turn in [g] (as determined by poker rules),
   and the the log of [g] records the match.
   Requires: [g] is a valid game
   Returns: the updated game after the match
   Raises: [NoPlayersLeft] if the current_player of [g] was -1
        [NotEnoughChips] if the current player of [g] has too few chips
        to match the minimum bet of [g]*)
val match_pot : game -> game

(**[check g] is [g] but after player the current player has
   checked, and the current_player of [g] is set to the next
   player in [g] to take a turn in [g] (as determined by poker rules),
   and the log of [g] records the check.
   Requires: [g] is a valid game
   Returns: the updated game after the check
   Raises: [NoPlayersLeft] if the current_player of [g] was -1
        [CantCheck] if the current player hasn't matched the minimum
        bet of [g]*)
val check : game -> game

(**[fold g] is [g] but after player the current player has
   folded, and the current_player of [g] is set to the next
   player in [g] to take a turn in [g] (as determined by poker rules), and the
   the log of [g] records the fold.
   Requires: [g] is a valid game
   Returns: the updated game after the fold
   Raises: [NoPlayersLeft] if the current_player of [g] was -1*)
val fold : game -> game

(**[get_new_deck ()] is a shuffled standard 52-card deck.*)
val get_new_deck : unit -> card list

(**[end_round g p] is [g] after distributing the pot to winning player [p],
   setting the mininum bet and each player's amount bet to zero, and deleting
   players with zero chips from [g], taking the cards from the table and players,
   and dealing new cards to the players.
   Requires: [g] is a valid game, [p] is a valid player of [g]
   Returns: [g] a valid game ready for a new round to be played *)
val end_round : game -> player -> game

(**[raise_resolved g] is if each player in [g] has either folded or has bet
   an amount equal to the minimum bet of [g]
   Requires: [g] is a valid game*)
val raise_resolved : game -> bool

(**[reset_current_player g] is [g] with the current player (index) set to zero
  Requires: [g] is a valid game*)
val reset_current_player : game -> game

(**The info needed to represent a player.
Abstraction Function: The tuple [n,c,p] represented here is
of the form name * chips * [player_info].
Representation Invariants:
  -The number of chips is zero (which may cause issues) or strictly positive*)
type player_info = string * int * player_type

(**[instantiate x] is a yet-unplayed game, initialized with [x] as its
   parameters.
   Returns: a new game [g]
   Requires: the [player_infos] in the passed in list are valid.*)
val instantiate : (player_info list) * bool -> game
