(**A module to provide functions for evaluating poker hands*)

open Model
(** A module to provide functions for evaluating poker hands**)

(** [InvalidPokerHand] is an exception that is raised when a invalid 
  poker hand is encountered when trying to classify a card list into
  one of the poker hands below. *)
exception InvalidPokerHand

(** A [pokerhand] is a variant type that identifies the type of 
  card list. It contains an int-card list tuple where the int is the numerical
  ranking of the hand and the card list is the hand that was classified into 
  that particular variant type. *)
type pokerhand =
  | StraightFlush of (int * card list)
  | FourOfKind of (int * card list)
  | FullHouse of (int * card list)
  | Flush of (int * card list)
  | Straight of (int * card list)
  | ThreeOfKind of (int * card list)
  | TwoPair of (int * card list)
  | Pair of (int * card list)
  | HighCard of (int * card list)

(** [identify_hand cl] is a pokerhand variant type that the hand
    is classified as.
    Requires: [cl] be a valid list of cards of length greater than or equal
    to 5.
    Raises: [InvalidPokerHand] if the card list has length less than or equal
    to 5 or if the hand contains invalid cards as defined by the card RI and AF.
    Example:
    let card_list =
      [{value=2; suit=Diamonds};{value=4; suit=Diamonds};
       {value=3; suit=Diamonds};{value=2; suit=Hearts};
       {value=9; suit=Diamonds};{value=7; suit=Diamonds};
       {value=6; suit=Hearts};]
    (identify_hand card_list) evaluates to
    Pair (2,[{value=2; suit=Diamonds};{value=2; suit=Hearts};
             {value=9; suit=Diamonds};{value=7; suit=Diamonds};
             {value=6; suit=Hearts};])
    This is because the best hand that can be formed contains a pair and the
    three highest cards are the kickers that form the last 3 cards. The pair
    is at the front of the list and the kickers are sorted by descending order.
    The suit order is irrelevant.
    Returns: A pokerhand with an int-card list tuple
    where the int is the integer representation of the hand and the
    card list is of length 5 representing the best possible poker hand.
*)
val identify_hand : card list -> pokerhand

(** [get_round_winner g] is the player that won the round based on the best
  possible poker hand. [get_round_winner] appends a copy of the cards in a
  [player] p's [hand] and appends it to a copy of the [game] g's [table].
  Then, it computes the best pokerhand in that card list by passing it through
  [identify_hand]. After computing this for each player, the pokerhands are
  sorted based on the integer representations of the pokerhands and if the
  pokerhands are of the same type (and consequently have the same integer
  representation), then the cards relevant to the pokerhand (i.e. the pair in
  a Pair, or the three of a kind and two of a kind in Full House, etc.) are
  sorted by value and strength if possible (i.e. Full House 3s over 4s is
  stronger then 2s over 5s so the triplet would be at the front of the list,
  and the pair would be at the end, etc.). Finally, the remaining cards
  (called kickers) are sorted in descending order where the head of the list
  is taken.
  Requires: [g] is a valid game as defined by the RI and AF.
  Raises: None
  Example: Given a game [g] with player list = [A; B; C] where A is a player
  with best pokerhand being a pair, B is a player with best pokerhand being a
  straight, and C is a player with best pokerhand being a straight flush, then
  get_round_winner g evalutes to C because the players get sorted by descending
  value of hands as follows [C;B;A] and taking the head of the list returns C.
  Returns: Player p in the game [g]'s [players] such that p has the best poker
  hand of all the players in [players].*)
val get_round_winner : Model.game -> Model.player


(** [get_straight_flush cl] is the best straight flush found in a card list 
  [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example:
    let flush =
    [{value=3; suit=Hearts}; {value=4; suit=Hearts};
     {value=5; suit=Hearts}; {value=6; suit=Hearts};
     {value=7; suit=Hearts}; {value=8; suit=Hearts}; {value=9; suit=Hearts}]
    get_straight_flush flush evalutes to
    [{value=9; suit=Hearts}; {value=8; suit=Hearts}; {value=7; suit=Hearts};
     {value=6; suit=Hearts}; {value=5; suit=Hearts};] because it is a flush
     (all cards are of one suit) and it is a straight (consecutive numbers), but
     note that it is the best straight since 8,7,6,5,4 and 7,6,5,4,3 are also
     valid straight flushes.
  Returns: A card list of length 5 that is sorted if there is a straight flush
  and the empty list if there does not exist a straight flush in [cl]. *)
val get_straight_flush : card list -> card list

(** [is_n_of_kind n cl] takes the best [n] of a kind sub-card list in [cl].
    Requires: [cl] is a valid card list of length greater than or equal to n.
    [n] is a positive number.
    Raises: None
    Example: 
    let x = 
    [{value=4; suit=Diamonds}; {value=4; suit=Hearts}; 
     {value 4; suit=Spades}; {value=6; suit=Hearts}; 
     {value=7; suit=Hearts}; {value=8; suit=Hearts}; {value=9; suit=Hearts}]
    is_n_of_kind 3 x evaluates to [{value=4; suit=Diamonds}; 
    {value=4; suit=Hearts}; {value 4; suit=Spades};] because there are 3 fours 
    and this is the only three of a kind.
    Returns: The best n of a kind in a card list if there is one. Returns 
    empty list otherwise.  *)
val is_n_of_kind : int -> card list -> card list

(** [get_full_house cl] is the best full house found in a card list [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example: 
    let fh = 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds}; 
     {value=3; suit=Spades}; {value=4; suit=Hearts}; 
     {value=4; suit=Spades}; {value=8; suit=Hearts}; {value=9; suit=Hearts}]
    get_full_house fh evalutes to 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds}; 
     {value=3; suit=Spades}; {value=4; suit=Hearts}; 
     {value=4; suit=Spades};] because it is a full house 3's over 4's 
     meaining there are three 3's and two 4's.
  Returns: A card list of length 5 that is sorted if there is a full house
  and the empty list if there does not exist a full house in [cl]. *)
val get_full_house : card list -> card list

(** [get_flush cl] is the best flush found in a card list [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example: 
    let flush = 
    [{value=7; suit=Hearts}; {value=3; suit=Hearts}; 
     {value=9; suit=Hearts}; {value=2; suit=Hearts}; 
     {value=11; suit=Hearts}; {value=4; suit=Hearts}; {value=13; suit=Hearts}]
    get_flush flush evalutes to 
    [{value=13; suit=Hearts}; {value=11; suit=Hearts}; 
     {value=9; suit=Hearts}; {value=7; suit=Hearts}; 
     {value=4; suit=Hearts};] because it is a flush with the highest cards.
  Returns: A card list of length 5 that is sorted if there is a flush
  and the empty list if there does not exist a flush in [cl]. *)
val get_flush : card list -> card list


(** [get_straight cl] is the best straight found in a card list [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example: 
    let straight = 
    [{value=3; suit=Hearts}; {value=4; suit=Hearts}; 
     {value=5; suit=Spades}; {value=6; suit=Diamonds}; 
     {value=7; suit=Hearts}; {value=8; suit=Clubs}; {value=9; suit=Spades}]
    get_straight straight evalutes to 
    [{value=9; suit=Spades}; {value=8; suit=Clubs};
     {value=7; suit=Hearts}; {value=6; suit=Diamonds}; {value=5; suit=Spades};] 
     because it is a straight (consecutive numbers), but
     note that it is the best straight since 8,7,6,5,4 and 7,6,5,4,3 are also
     valid straights.
  Returns: A card list of length 5 that is sorted if there is a straight
  and the empty list if there does not exist a straight in [cl]. *)
val get_straight : card list -> card list

(** [get_three_of_kind cl] is the best three of a kind found in a card list 
  [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example: 
    let tok = 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds}; 
     {value=3; suit=Spades}; {value=2; suit=Hearts}; 
     {value=7; suit=Spades}; {value=6; suit=Hearts}; {value=9; suit=Hearts}]
    get_three_of_kind tok evalutes to 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds}; 
     {value=3; suit=Spades}; {value=9; suit=Hearts}; 
     {value=7; suit=Spades};] because it contains three 3's and has kickers
     9 and 7 since they are the largest of the remaining cards.
  Returns: A card list of length 5 that is sorted if there is a three of a kind
  and the empty list if there does not exist a three of a kind in [cl]. *)
val get_three_of_kind : card list -> card list

(** [get_two_pair cl] is the best two pair found in a card list 
  [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example: 
    let tp = 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds}; 
     {value=2; suit=Spades}; {value=2; suit=Hearts}; 
     {value=7; suit=Spades}; {value=6; suit=Hearts}; {value=9; suit=Hearts}]
    get_two_pair tok evalutes to 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds};
    {value=2; suit=Hearts}; {value=2; suit=Spades};
    {value=9; suit=Hearts}] because it contains two 3's and two 2's
     9 since it is the largest of the remaining cards.
  Returns: A card list of length 5 that is sorted if there is a two pair.
  and the empty list if there does not exist a two pair in [cl]. *)
val get_two_pair : card list -> card list

(** [get_pair cl] is the best two pair found in a card list 
  [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Example: 
    let p = 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds}; 
     {value=2; suit=Spades}; {value=4; suit=Hearts}; 
     {value=7; suit=Spades}; {value=6; suit=Spades}; {value=9; suit=Hearts}]
    get_pair tok evalutes to 
    [{value=3; suit=Hearts}; {value=3; suit=Diamonds};
    {value=9; suit=Hearts}; {value=7; suit=Spades};
    {value=6; suit=Spades}] because it contains two 3's and 
     9, 7 and 6 since they are the largest of the remaining cards.
  Returns: A card list of length 5 that is sorted if there is a pair.
  and the empty list if there does not exist a pair in [cl]. *)
val get_pair : card list -> card list

(** [get_high_card cl] is the card list to return if no other
  higher pokerhand is found in a card list [cl].
  [cl].
  Requires: [cl] is a valid card list of length greater than or equal to 5.
  Raises: None
  Returns: Returns the greatest 5 cards. *)
val get_high_card : card list -> card list

(** [reverse_sort_card_list cl] is the sorted card list [cl] by descending
  value.
  Requires: [cl] is a valid card list.
  Raises: None
  Returns: Returns a descending order card list. *)
val reverse_sort_card_list : card list -> card list

(** [get_phand_and_clst cl] is the tuple containing pokerhand card list 
found in the card list [cl] and associated string with that poker hand.
  Requires: [cl] is a valid card list.
  Raises: None
  Returns: Returns tuple of string-card list where the strig is a poker hand
  type and the card list containg the poker hand. *)
val get_phand_and_clst : card list -> string * card list

(** [compare_hands cl1 cl2] is a comparator that returns a positive value 
  if the poker hand found in [cl1] is ranked higher than [cl2] and negative 
  value if the poker hand found in [cl2] is ranked higher than [cl1] and 
  zero if the poker hands are of the same ranking. 
  Requires: [cl1] and [cl2] are valid card lists.
  Raises: None
  Returns: Returns an integer based on the conditions stated above.*)
val compare_hands : card list -> card list -> int
