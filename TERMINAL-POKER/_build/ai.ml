open Model
open Parser
open Roundwinner
open Random

(**[get_value g p] is the value of the hand of player[p] in game [g], where
   the value is the CDF(hand), where CDF is the CDF over poker hands of size
   number of cards private to [p] + number of cards on the table of [g].
   Thus, a good poker hand (which is rare) will have result in a value closer to
   one than zero, and the CDF is computed with the hands in increasing order,
   ordered by their rarity (and thus value).*)
let get_value (g:Model.game) (p:Model.player) : float =
  let cards = List.append (Model.get_player_cards p) (Model.get_table g) in
  match List.length (Model.get_table g) with
  | 0 ->
    (match Roundwinner.identify_hand cards with
     | StraightFlush _ -> 0.0
     | FourOfKind _ -> 0.0
     | FullHouse _ -> 0.0
     | Flush _ -> 0.0
     | Straight _ -> 0.0
     | ThreeOfKind _ -> 0.0
     | TwoPair _ -> 0.0
     | Pair _ -> 0.942
     | HighCard _ -> 0.058)
  | 3 ->
    (match Roundwinner.identify_hand cards with
     | StraightFlush _ -> 1.0
     | FourOfKind _ -> 0.999744
     | FullHouse _ -> 0.9983
     | Flush _ -> 0.99633
     | Straight _ -> 0.9924
     | ThreeOfKind _ -> 0.9713
     | TwoPair _ -> 0.9238
     | Pair _ -> 0.501
     | HighCard _ -> 0.0)
  | 4 ->
    (match Roundwinner.identify_hand cards with
     | StraightFlush _ -> 1.0
     | FourOfKind _ -> 0.99945
     | FullHouse _ -> 0.9913
     | Flush _ -> 0.9812
     | Straight _ -> 0.9634
     | ThreeOfKind _ -> 0.9274
     | TwoPair _ -> 0.803
     | Pair _ -> 0.325
     | HighCard _ -> 0.0)
  | 5 ->
    (match Roundwinner.identify_hand cards with
     | StraightFlush _ -> 1.0
     | FourOfKind _ -> 0.99801
     | FullHouse _ -> 0.972
     | Flush _ -> 0.9418
     | Straight _ -> 0.896
     | ThreeOfKind _ -> 0.847
     | TwoPair _ -> 0.612
     | Pair _ -> 0.174
     | HighCard _ -> 0.0)
  | _ -> failwith ""

(**[get_normal_move g p agg] is the response of AI [p] to game [g], given
   the (heaviliy modified) aggression of [p].
   Requires: if [true_aggression] is the aggression of [p], the modified
   aggression input here is 0.5 - (logistic(true_aggression)) / 2.
 **)
let get_normal_move (g:Model.game) (p:Model.player) (aggression:float) : 
  Parser.response =
  let hand_value = get_value g p in
  let num_chips = Model.get_chips p in
  let min_bet = Model.get_min_bet g in
  let amt_bet = Model.get_player_bet p in
  match num_chips with
  | 0 ->
    if amt_bet >= min_bet then Check
    else Fold
  | 1 ->
    if amt_bet > min_bet then Check
    else if amt_bet = min_bet then
      if hand_value > aggression then Raise 1
      else Check
    else
      let diff = min_bet - amt_bet in
      if  diff > num_chips then Fold
      else
      if  hand_value > aggression then Match
      else Check
  | n ->
    if amt_bet > min_bet then Check
    else if amt_bet = min_bet then
      if hand_value > aggression then Raise (1 + (n/2))
      else Check
    else
      let diff = min_bet - amt_bet in
      if  diff > num_chips then Fold
      else
      if  hand_value > aggression then Match
      else Check

(*Documented in the .mli *)
let get_move (g:game) (p:player) : response =
  match Model.get_player_type p with
  | Human -> failwith "ai - get_move - Some weird android borg thing happened"
  | AI (aggression,skill) ->
    let aggression = float_of_int aggression in
    let skill = float_of_int skill in
    (*sigmoidify aggression and skill*)
    let aggression = aggression /. (1.0 +. (abs_float aggression)) in
    let skill = skill /. (1.0 +. 3.0 *. (abs_float skill)) in
    (*map aggression and skill to values between zero and one, with a larger
      original value giving a value closer to zero*)
    let aggression = 0.5 -. (aggression /. 2.0) in
    let skill = 0.5 -. (skill /. 2.0) in
    let move = get_normal_move g p aggression in
    let p_mistake = Random.float 1.0 in
    if p_mistake < skill then Fold else move
