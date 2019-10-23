open OUnit2
open Model
open Main
open Roundwinner
open Parser

(**An unplayed game to test with*)
let unplayed_game =
  let x = ([("Steve",30,Human);("Chad",30,Human);("Dan",30,Human)]),false in
  Model.instantiate x

(**A list of players, such that the game the players are in would
be in a raise, and raise_resolved would be false*)
let p_list_unresolved = [
{
  name = "Steve";
  cards = [];
  amt_bet = 2;
  chips = 29;
  folded = false;
  player_type = Human;
  next = 1;
};
{
  name = "Chad";
  cards = [];
  amt_bet = 1;
  chips = 29;
  folded = false;
  player_type = Human;
  next = 2;
};
{
  name = "Dan";
  cards = [];
  amt_bet = 1;
  chips = 29;
  folded = false;
  player_type = Human;
  next = -1;
};
]

(**A list of players*)
let p_list = [
{
  name = "Steve";
  cards = [];
  amt_bet = 1;
  chips = 29;
  folded = false;
  player_type = Human;
  next = 1;
};
{
  name = "Chad";
  cards = [];
  amt_bet = 1;
  chips = 29;
  folded = false;
  player_type = Human;
  next = 2;
};
{
  name = "Dan";
  cards = [];
  amt_bet = 1;
  chips = 29;
  folded = false;
  player_type = Human;
  next = -1;
};
]

(**Tests dealing one card to the table*)
let make_deal_1_to_table_test
    (name:string)
    (g:game) : test =
  name>:: fun _-> (
      let initial_size = List.length (Model.get_table g) in
      let newgame = Model.deal_1_to_table g in
      let table = Model.get_table newgame in
      let x = ((List.length table) - 1) in
      assert_equal x initial_size
    )

(**Tests dealing one card to the table*)
let make_deal_1_to_table_tests = [
      make_deal_1_to_table_test "(0)" unplayed_game ;
]

(**Tests dealing three cards to the table*)
let make_deal_3_to_table_test
    (name:string)
    (g:game) : test =
  name>:: fun _-> (
    let initial_size = List.length (Model.get_table g) in
    let newgame = Model.deal_3_to_table g in
    let table = Model.get_table newgame in
    let x = ((List.length table) - 3) in
    assert_equal x initial_size
    )

(**Tests dealing three cards to the table*)
let make_deal_3_to_table_tests = [
    make_deal_3_to_table_test "(0)" unplayed_game ;
]

(**Tests raising the pot*)
let make_raise_pot_test
    (name:string)
    (g:game)
    (amt:int)
    (expected:(int*player)) : test =
  name >:: fun _-> (
      let updgame = Model.raise_pot g amt in
      let tup = (Model.get_pot updgame, Model.get_current_player updgame) in
      assert_equal expected tup
    )

(**Tests raising the pot*)
let make_raise_pot_tests = [
    make_raise_pot_test "(0)" unplayed_game 10
      (13,(Model.get_next_player unplayed_game));
]

(**Tests a player checking*)
let make_check_test
    (name:string)
    (g:game)
    (expected_player:Model.player) : test =
  name >:: fun _-> (
      let updgame = Model.check g  in
      let next_player = (Model.get_current_player updgame)in
      assert_equal expected_player next_player
    )

(**Tests raising the pot*)
let make_check_tests = [
  make_check_test "(0)" unplayed_game (Model.get_next_player unplayed_game);
]

(**Tests a player folding*)
let make_fold_test
    (name:string)
    (g:game)
    (expected_player:Model.player) : test =
  name >:: fun _-> (
    let updgame = Model.fold g  in
    let next_player = (Model.get_current_player updgame)in
    assert_equal expected_player next_player
    )

(**Tests raising the pot*)
let make_fold_tests = [
  make_fold_test "(0)" unplayed_game
    (List.nth (Model.get_players unplayed_game) 1)
]

(**Tests a round ending*)
let make_end_round_test
    (name:string)
    (g:game)
    (winner:Model.player)
    (expected_players: (Model.player) list) : test =
  name >:: fun _-> (
      let updgame = Model.end_round g winner in
      let updated_players = (Model.get_players updgame)in
      assert_equal expected_players updated_players
    )

(***)

(**Tests if a raise_resolved works as docoumented*)
let make_raise_resolved_test
    (name:string)
    (g:game)
    (expected_bool: bool) : test =
  name >:: fun _-> (
      let raised = Model.raise_resolved g  in
      assert_equal expected_bool raised
    )

let raise_resolved_tests = [
  make_raise_resolved_test "0" unplayed_game true;
  make_raise_resolved_test "1"
    {unplayed_game with players=p_list_unresolved} false;
]

(**Tests if a reset_current_players works as docoumented*)
let make_reset_current_player_test
    (name:string)
    (g:game)
    (expected_player: Model.player) : test =
  name >:: fun _-> (
      let new_game = Model.reset_current_player g  in
      let player = Model.get_current_player new_game in
      assert_equal expected_player player
    )

(*ROUNDWINNER TESTING*)

(**[make_straight_flush_test nm clst] tests Roundwinner.get_straight_flush *)
let make_straight_flush_test
    (name:string)
    (clst:card list): test =
  name >:: fun _-> (
      let possible = Roundwinner.get_straight_flush clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_fok_test nm clst] tests Roundwinner.is_n_of_kind with n = 4 *)
let make_fok_test
    (name:string)
    (clst:card list) : test =
  name >:: fun _-> (
      let possible = Roundwinner.is_n_of_kind 4 clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_fh_test nm clst] tests Roundwinner.get_full_house *)
let make_fh_test
    (name:string)
    (clst:card list) : test =
  name >:: fun _-> (
      let possible = Roundwinner.get_full_house clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_flush_test nm clst] tests Roundwinner.get_flush *)
let make_flush_test
    (name:string)
    (clst:card list) : test =
  name >:: fun _-> (
      let possible = Roundwinner.get_flush clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_tok_test nm clst] tests Roundwinner.get_three_of_kind *)
let make_tok_test
    (name:string)
    (clst:card list): test =
  name >:: fun _-> (
      let possible = Roundwinner.get_three_of_kind clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_straight_test nm clst] tests Roundwinner.get_straight *)
let make_straight_test
    (name:string)
    (clst:card list) : test =
  name >:: fun _-> (
      let possible = Roundwinner.get_straight clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_tp_test nm clst] tests Roundwinner.get_two_pair *)
let make_tp_test
    (name:string)
    (clst:card list): test =
  name >:: fun _-> (
      let possible = Roundwinner.get_two_pair clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(**[make_pair_test nm clst] tests Roundwinner.get_pair *)
let make_pair_test
    (name:string)
    (clst:card list) : test =
  name >:: fun _-> (
      let possible = Roundwinner.get_pair clst in
      let rec listchecker lst1 lst2 bl =
        match lst1 with
        | [] -> bl
        | h::t -> if List.mem h lst2 then listchecker t lst2 true
          else false
      in
      assert_equal (listchecker possible clst false) true
    )

(*STRAIGHT FLUSH TESTERS *)
let strflush0 =
  [{value=9; suit=Hearts}; {value=8; suit=Hearts}; {value=7; suit=Hearts};
  {value=6; suit=Hearts};
   {value=5; suit=Hearts}; {value=12; suit=Spades}; {value=6; suit=Clubs}]
let strflush2 =
  [{value=1; suit=Hearts}; {value=2; suit=Hearts}; {value=3; suit=Hearts};
   {value=4; suit=Hearts};
   {value=5; suit=Hearts}; {value=12; suit=Spades}; {value=6; suit=Clubs}]
let strflush4 =
  [{value=2; suit=Hearts}; {value=7; suit=Hearts}; {value=11; suit=Clubs};
  {value=12; suit=Clubs};
   {value=13; suit=Clubs}; {value=14; suit=Clubs}; {value=15; suit=Clubs}]

(*FOUR OF A KIND TESTERS *)
let fok0 =
  [{value=9; suit=Hearts}; {value=9; suit=Clubs}; {value=9; suit=Spades};
  {value=9; suit=Diamonds};
   {value=5; suit=Hearts}; {value=12; suit=Spades}; {value=6; suit=Clubs}]
let fok2 =
  [{value=13; suit=Spades}; {value=13; suit=Clubs}; {value=3; suit=Hearts};
  {value=4; suit=Hearts};
   {value=5; suit=Hearts}; {value=13; suit=Diamonds}; {value=13; suit=Hearts}]
let fok4 =
  [{value=7; suit=Spades}; {value=7; suit=Hearts}; {value=11; suit=Clubs};
  {value=7; suit=Clubs};
   {value=13; suit=Clubs}; {value=7; suit=Diamonds}; {value=15; suit=Clubs}]

(*FULLHOUSE TESTERS *)
let fh0 =
  [{value=9; suit=Hearts}; {value=9; suit=Clubs}; {value=9; suit=Spades};
  {value=9; suit=Diamonds};
   {value=5; suit=Hearts}; {value=2; suit=Spades}; {value=2; suit=Clubs}]
let fh2 =
  [{value=13; suit=Spades}; {value=13; suit=Clubs}; {value=3; suit=Hearts};
  {value=3; suit=Spades};
   {value=5; suit=Hearts}; {value=12; suit=Diamonds}; {value=13; suit=Hearts}]
let fh4 =
  [{value=7; suit=Spades}; {value=7; suit=Hearts}; {value=11; suit=Clubs};
  {value=7; suit=Clubs};
   {value=15; suit=Clubs}; {value=15; suit=Diamonds}; {value=15; suit=Clubs}]

(*FLUSH TESTERS *)
let flush0 =
  [{value=9; suit=Hearts}; {value=9; suit=Hearts}; {value=9; suit=Spades};
   {value=9; suit=Hearts};
   {value=5; suit=Hearts}; {value=12; suit=Hearts}; {value=6; suit=Clubs}]
let flush2 =
  [{value=13; suit=Clubs}; {value=13; suit=Clubs}; {value=3; suit=Hearts};
   {value=4; suit=Clubs};
   {value=5; suit=Hearts}; {value=13; suit=Clubs}; {value=13; suit=Clubs}]
let flush4 =
  [{value=7; suit=Spades}; {value=7; suit=Spades}; {value=11; suit=Clubs};
   {value=7; suit=Clubs};
   {value=13; suit=Spades}; {value=7; suit=Spades}; {value=15; suit=Spades}]

(*STRAIGHT TESTERS *)
let straight0 =
  [{value=9; suit=Hearts}; {value=8; suit=Hearts}; {value=7; suit=Hearts};
   {value=6; suit=Hearts}; {value=6; suit=Clubs}; {value=12; suit=Spades};
   {value=5; suit=Clubs}]
let straight2 =
  [{value=15; suit=Hearts}; {value=2; suit=Diamonds}; {value=3; suit=Hearts};
  {value=4; suit=Hearts};
   {value=5; suit=Hearts}; {value=12; suit=Spades}; {value=6; suit=Clubs}]
let straight4 =
  [{value=2; suit=Hearts}; {value=7; suit=Hearts}; {value=11; suit=Clubs};
  {value=12; suit=Clubs};
   {value=13; suit=Spades}; {value=14; suit=Clubs}; {value=15; suit=Clubs}]

(*THREE OF A KIND TESTERS *)
let tok0 =
  [{value=9; suit=Hearts}; {value=9; suit=Spades}; {value=9; suit=Diamonds};
  {value=6; suit=Hearts};
   {value=5; suit=Hearts}; {value=12; suit=Spades}; {value=6; suit=Clubs}]
let tok2 =
  [{value=13; suit=Hearts}; {value=13; suit=Diamonds}; {value=3; suit=Hearts};
  {value=4; suit=Hearts};
   {value=5; suit=Hearts}; {value=13; suit=Spades}; {value=13; suit=Clubs}]
let tok4 =
  [{value=3; suit=Hearts}; {value=7; suit=Hearts}; {value=3; suit=Clubs};
  {value=12; suit=Clubs};
   {value=13; suit=Spades}; {value=14; suit=Clubs}; {value=3; suit=Clubs}]

(*TWO PAIR TESTERS *)
let tp0 =
  [{value=9; suit=Hearts}; {value=9; suit=Spades}; {value=6; suit=Diamonds};
  {value=6; suit=Hearts};
   {value=5; suit=Hearts}; {value=12; suit=Spades}; {value=2; suit=Clubs}]
let tp2 =
  [{value=13; suit=Hearts}; {value=13; suit=Diamonds}; {value=3; suit=Hearts};
  {value=4; suit=Hearts};
   {value=5; suit=Hearts}; {value=4; suit=Spades}; {value=15; suit=Clubs}]
let tp4 =
  [{value=3; suit=Hearts}; {value=3; suit=Hearts}; {value=8; suit=Clubs};
  {value=8; suit=Clubs};
   {value=4; suit=Spades}; {value=6; suit=Clubs}; {value=12; suit=Clubs}]

let identify_straight_flush = [
  make_straight_flush_test "(0) identify Straight Flush" strflush0;
  make_straight_flush_test "(2) identify Straight Flush" strflush2;
  make_straight_flush_test "(4) identify Straight Flush" strflush4;
]

let identify_fok = [
  make_fok_test "(0) identify fok" fok0;
  make_fok_test "(2) identify fok" fok2;
  make_fok_test "(4) identify fok" fok4;
]

let identify_fh = [
  make_fh_test "(0) identify full house" fh0;
  make_fh_test "(2) identify full house" fh2;
  make_fh_test "(4) identify full house" fh4;
]

let identify_flush = [
  make_flush_test "(0) identify flush" flush0;
  make_flush_test "(2) identify flush" flush2;
  make_flush_test "(4) identify flush" flush4;
]

let identify_straight = [
  make_straight_test "(0) identify straight" straight0;
  make_straight_test "(2) identify straight" straight2;
  make_straight_test "(4) identify straight" straight4;
]

let identify_tok = [
  make_tok_test "(0) identify tok" tok0;
  make_tok_test "(2) identify tok" tok2;
  make_tok_test "(4) identify tok" tok4;
]

let identify_tp = [
  make_tp_test "(0) identify tp" tp0;
  make_tp_test "(2) identify tp" tp2;
  make_tp_test "(4) identify tp" tp4;
]

let roundwinner_tests =
  List.flatten [
    identify_straight_flush;
    identify_fok;
    identify_fh;
    identify_flush;
    identify_straight;
    identify_tok;
    identify_tp;
    make_deal_1_to_table_tests;
    make_deal_3_to_table_tests;
    make_raise_pot_tests;
    make_check_tests;
    make_fold_tests;
    raise_resolved_tests
  ]
let suite =
  "test suite for Poker"  >::: List.flatten [
    roundwinner_tests;
  ]

let _ = print_string "hi"
let _ = run_test_tt_main suite
