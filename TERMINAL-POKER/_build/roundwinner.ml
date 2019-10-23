(*TODO:
  -Make this not depend on knowing how a card is implemented
*)


open Model

exception InvalidPokerHand

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

(**[get_int_list c] is a list containing all card values in [c] *)
let get_int_list (c: card list) : int list = 
  List.map (fun a -> Model.get_card_value a) c

(**[unwrap x] is the value stored in an optional. Fails if there is nothing
   to unwrap i.e. the optional is variant type None. *)
let unwrap (x:'a option) : 'a =
  match x with
  | None -> failwith "cannot unwrap"
  | Some s -> s

(**[compare_cards c1 c2] is 1 if c1 < c2, 0 if c1 > c2, -1 otherwise *)
let compare_cards (c_1:card) (c_2:card) : int =
  if Model.get_card_value c_1 < Model.get_card_value c_2 then -1
  else if Model.get_card_value c_1 > Model.get_card_value c_2 then 1
  else if Model.get_card_value c_1 = Model.get_card_value c_2 then 0
  else match Model.get_card_suit c_1,Model.get_card_suit c_2 with
    | Diamonds,_ -> -1
    | Clubs,Diamonds -> 1
    | Clubs,_ -> -1
    | Hearts,Clubs -> 1
    | Hearts,Diamonds -> 1
    | Hearts,_ -> -1
    | Spades,_ -> 1

(**[take_first_n_cards c n] takes the first [n] cards from a card list
   [c]. This is a useful utility function for extracting hands and subhands.
   Precondition: [n] cannot be more than the length of card list [c].*)
let rec take_first_n_cards (n:int) (c: card list) : card list =
  if n = 0 then []
  else if List.length c < n then c
  else (List.hd c) :: take_first_n_cards (n-1) (List.tl c)

(**[reverse_sort_card_list c] sorts card list [c] in weak decreasing order by
   card value. The suit of the card does not affect its order*)
let reverse_sort_card_list (c: card list) : card list =
  let reverse_sort_card (c1: card) (c2: card) : int = 
    (Model.get_card_value) c2 - (Model.get_card_value c1)
  in List.sort reverse_sort_card c

(**[high_card c] returns the highest card in a card list [c] by
   reverse sorting and taking the first element*)
let high_card (c: card list) : int =
  Model.get_card_value (c |> reverse_sort_card_list |> List.hd)

(**[low_card c] is the card value of the lowest-valued card in [c] *)
let low_card (c: card list) : int =
  Model.get_card_value (c |> reverse_sort_card_list |> List.rev |> List.hd)

(**[is_straight c] is a 5-card list if the card list [c] contains 5 cards in 
   strictly decreasing order and returns list the contains straight and the 
   hand that contains the highest straight. *)
let rec get_straight (cl: card list) : card list =
  (* If there is an ace (represented by 15) include an ace with value 14.
     This is because ace can be used as a high card and low card. *)
  let plus_low_ace (cli: card list) =
    if (cli |> high_card) = 15 then {value=1; suit=Hearts} :: cli else cli in
  let c = cl |> plus_low_ace |> List.sort_uniq compare_cards |> 
          reverse_sort_card_list  in
  let is_straight_helper (c: card list): card list =
    if (List.length c >= 5 &&
        (List.nth c 0).value - (List.nth c 1).value = 1 &&
        (List.nth c 1).value - (List.nth c 2).value = 1 &&
        (List.nth c 2).value - (List.nth c 3).value = 1 &&
        (List.nth c 3).value - (List.nth c 4).value = 1)
    then take_first_n_cards 5 c
    else [] in
  let hand = is_straight_helper c in
  if hand = [] && List.length c >= 5 then get_straight (List.tl c)
  else hand

(**[get_flush c] is a 5-card list if the card list [c] contains 5 cards of the
   same suit and returns the highest hand that is a flush. *)
let get_flush (c: card list) : card list =
  (* [has_flush_helper] returns a true if there is a flush and
     the suit of the flush if it exists. It takes an accumulating tuple
     that counts how many cards of each suit there are. *)
  let rec has_flush_helper (c: card list) (acc: int*int*int*int)
    : suit option =
    match c with
    | [] -> (match acc with (a,b,c,d) ->
        if a >= 5 then Some Hearts
        else if b >= 5 then Some Clubs
        else if c >= 5 then Some Diamonds
        else if d >= 5 then Some Spades
        else None)
    | h::t ->
      (*[update_acc c old] is a tuple where the nth entry is updated
        depending on the suit. *)
      let update_acc (c: card) (old_acc: int*int*int*int) =
        match c.suit with
        | Hearts -> (match old_acc with (a, b, c, d) -> (a+1, b, c, d))
        | Clubs -> (match old_acc with (a, b, c, d) -> (a, b+1, c, d))
        | Diamonds -> (match old_acc with (a, b, c, d) -> (a, b, c+1, d))
        | Spades -> (match old_acc with (a, b, c, d) -> (a, b, c, d+1))
      in has_flush_helper t (update_acc h acc) in
  let possible_suit = has_flush_helper c (0,0,0,0) in
  (* Gets the cards of a certain suit in a card list. *)
  let get_flush_hand (s: suit) (c: card list): card list =
    List.filter (fun a -> a.suit = s) c in
  match possible_suit with
  | None -> []
  | Some x -> 
    c |> get_flush_hand x |> reverse_sort_card_list |> take_first_n_cards 5

(**[is_straight_flush c] is a 5-card list if the card list [c] is a straight 
   and a flush and returns the hand that satisfies these properties. 
   Otherwise, none. *)
let rec get_straight_flush (c: card list) : card list =
  (*This goes through all the possible subsets of the card list, but only
    subsets of size 5 or greater are valid. *)
  if List.length c >= 5 then
    let ordered = c |> reverse_sort_card_list in
    let straight = ordered |> get_straight in
    (* Checks if the card list contains a straight. *)
    if straight <> [] then
      let flush = get_flush straight in
      if flush <> [] then flush
      (* If not checks if there if there is another straight that is
         smaller but might be a flush. *)
      else get_straight_flush (List.tl ordered)
      (*If not checks if there if there is another straight that is
        smaller but might be a flush. *)
    else get_straight_flush (List.tl ordered)
  else []

(**[card_frequency clst] is an [int array] size 15, where [int array][i-1] is
   an integer corresponding to the number of cards in [clst] that have value i
   NOTE: An ace (default value 15) also has value 1
   Example:
   [card_frequency [{v=3, s=Hearts}; {v=4; s=Spades}; {v=4; s=Clubs};
                      {v=15; s=Diamonds}]]
                      returns [|1; 0; 1; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1|]
                    *)
let card_frequency (c: card list) : int array =
  let freq_array = Array.make 16 0 in
  let rec card_freq_helper (c: card list) : unit =
    match c with
    | [] -> ()
    | h::t -> freq_array.(h.value) <- freq_array.(h.value) + 1;
      card_freq_helper t in
  card_freq_helper c; freq_array

(**[is_n_of_kind n clst] returns the card list that contains this n_of_kind
   grouping otherwise returns [] *)
let is_n_of_kind (n:int) (clst:card list) : card list =
  let freq_distr = card_frequency clst in
  let desc_freq_lst = freq_distr |> Array.to_list
                      |> List.rev in
  let rec n_matcher (n:int) (v:int) (freqlst:int list) : int = (*returns the 
                                                                 value that 
                                                                 corresponds to 
                                                                 the best nok 
                                                                 hand*)
    match freqlst with
    | [] -> -1
    | h::t -> if h >= n then v else n_matcher n (v-1) t
  in
  (* Should be 14 because *)
  let nok_val = n_matcher n 15 desc_freq_lst in
  if nok_val = -1 then []
  else let rawlst = List.filter (fun c -> c.value = nok_val) clst in 
    (*This filters through clst and builds a cardlist where all cards have 
      value = nok_val*)
    take_first_n_cards n rawlst

(**[find_freq t freq] is an int list whose length is equal to freq[t] and
   whose ints are equal to t *)
let find_freq (target: int) (freq: int array) : int list =
  let rec get_kickers_helper (f: int array) (n: int) (acc: int list) =
    if (Array.length f) > n then
      if f.(n) = target then get_kickers_helper f (n+1) (n::acc)
      else  get_kickers_helper f (n+1) acc
    else acc
  in (get_kickers_helper freq 0 []) |> List.rev

(**[get_full_house c] is [] if there is no full house in c, and a card list
   containing the full house otherwise *)
let get_full_house (clist: card list): card list =
  let repeats3 = is_n_of_kind 3 clist in
  if is_n_of_kind 3 clist = [] then []
  else (
    let sublst = List.filter (fun c -> not (List.mem c repeats3)) clist in
    let repeats2 = is_n_of_kind 2 sublst in
    if repeats2 = [] then []
    else repeats3@repeats2
  )

(*[get_three_of_kind c] gets the three of a kind from a card list if
  there is one and returns the empty list otherwise. *)
let get_three_of_kind (c: card list): card list =
  let repeats3 = c |> is_n_of_kind 3 in
  if repeats3 = [] then []
  else
    let card_val = (List.hd repeats3).value in
    let kickers = c |> reverse_sort_card_list
                  |> List.filter (fun c -> c.value <> card_val) 
                  |> take_first_n_cards 2
    in repeats3@kickers

(**[get_two_pair c] is [] if there is no two pair in [c], otherwise it is the
   best hand containing the best two pair *)
let get_two_pair (c: card list) : card list =
  let fstrepeats2 = c |> is_n_of_kind 2 in
  if fstrepeats2 = [] then []
  else
    let fstcard_val = (List.hd fstrepeats2).value in
    let sublist = c |> reverse_sort_card_list |> 
                  List.filter (fun c -> c.value <> fstcard_val) in
    let sndrepeats2 = sublist |> is_n_of_kind 2 in
    if sndrepeats2 = [] then []
    else
      let sndcard_val = (List.hd sndrepeats2).value in
      let kickers = c |> reverse_sort_card_list
                    |> List.filter (fun c -> c.value <> sndcard_val) 
                    |> take_first_n_cards 1
      in fstrepeats2@sndrepeats2@kickers

(**[get_pair c] is [] if there is no pair in [c], otherwise it is a list
   representing the best hand containing the best pair *)
let get_pair (c: card list): card list  =
  let repeats2 = c |> is_n_of_kind 2 in
  if repeats2 = [] then []
  else
    let card_val = (List.hd repeats2).value in
    let kickers = c |> reverse_sort_card_list
                  |> List.filter (fun c -> c.value <> card_val) 
                  |> take_first_n_cards 3
    in repeats2@kickers

(**[get_high_card c] is a list representing best 5-card hand of [c] with 
   the highest-valued card as the head *)
let get_high_card (c: card list): card list =
  c |> reverse_sort_card_list |> take_first_n_cards 5

(**[get_identify_hand c] is the pokerhand corresponding to the best hand in
   [c] *)
let identify_hand (c: card list) : pokerhand =
  let sf = get_straight_flush c in
  let fh = get_full_house c in
  let f = get_flush c in
  let s = get_straight c in
  let tok = get_three_of_kind c in
  let tp = get_two_pair c in
  let p = get_pair c in
  let hc = get_high_card c in
  if sf <> [] then StraightFlush (9, sf)
  else if fh <> [] then FullHouse (7, fh)
  else if f <> [] then Flush (6, f)
  else if s <> [] then Straight (5, s)
  else if tok <> [] then ThreeOfKind (4, tok)
  else if tp <> [] then TwoPair (3, tp)
  else if p <> [] then Pair (2, p)
  else if hc <> [] then HighCard (2, hc)
  else raise InvalidPokerHand

(**[get_int_val ph] is an int representing the numerical ranking of ph *)
let get_int_val (ph: pokerhand) : int =
  match ph with
  | StraightFlush a -> fst a
  | FourOfKind a -> fst a
  | FullHouse a -> fst a
  | Flush a -> fst a
  | Straight a -> fst a
  | ThreeOfKind a -> fst a
  | TwoPair a -> fst a
  | Pair a -> fst a
  | HighCard a -> fst a

(**[get_card_list h] is a card list corresponding to the poker hand [h] *)
let get_card_list (h: pokerhand) : card list =
  match h with
  | StraightFlush a -> snd a
  | FourOfKind a -> snd a
  | FullHouse a -> snd a
  | Flush a -> snd a
  | Straight a -> snd a
  | ThreeOfKind a -> snd a
  | TwoPair a -> snd a
  | Pair a -> snd a
  | HighCard a -> snd a

(**[get_phand_and_clst c] is a tuple [(a,b)] where [a] is a string
   representation of the pokerhand and [b] is the corresponding card list  *)
let get_phand_and_clst (clst: card list) : string * card list =
  let phand = identify_hand clst in
  let pstring =
    match phand with
    | StraightFlush a -> "Straight Flush"
    | FourOfKind a -> "Four of a Kind"
    | FullHouse a -> "Full House"
    | Flush a -> "Flush"
    | Straight a -> "Straight"
    | ThreeOfKind a -> "Three of a Kind"
    | TwoPair a -> "Two Pair"
    | Pair a -> "Pair"
    | HighCard a -> "High Card"
  in
  pstring, get_card_list phand

(**[cl_cmp c1 c2] is a List.sort_uniq for cards *)
let cl_cmp (c1: card list) (c2: card list) : int =
  let rec cl_cmp_helper (c1: card list) (c2: card list) =
    match c1 with
    | [] -> 0
    | h::t ->
      let diff = (h |> get_card_value) - ((List.hd c2) |> get_card_value) in
      if diff <> 0 then diff
      else cl_cmp_helper t (List.tl c2) in
  cl_cmp_helper c1 c2

(**[tiebreak h1 h2] is a comparator function that breaks poker ties *)
let tiebreak (h1: pokerhand) (h2: pokerhand) : int =
  let c1 = get_card_list h1 in
  let c2 = get_card_list h2 in
  cl_cmp c1 c2

(**[compare_hands cl1 cl2] is a comparator function for card lists *)
let compare_hands (cl1: card list) (cl2: card list) : int =
  let hand1 = identify_hand cl1 in
  let hand2 = identify_hand cl2 in
  let diff = (get_int_val hand2) - (get_int_val hand1) in
  if diff <> 0 then diff else tiebreak hand1 hand2

(**[get_round_winner g] is the winner of [g]*)
let get_round_winner (g: game) : player =
  let table = Model.get_table g in
  let players = Model.get_players g in
  let players =
    List.filter (fun p -> not (Model.has_folded p)) players in
  let player_cl_tup =
    List.map (fun a -> (a, (get_player_cards a) @ table)) players in
  let sorted =
    List.sort (fun a b -> (compare_hands (snd a) (snd b))) player_cl_tup
  in fst(List.hd sorted)