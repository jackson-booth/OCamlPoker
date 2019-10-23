open Model

let get_winner (g:Model.game) : (Model.player option) =
  (*[helper p_list acc] is the number of players in [p_list] with nonzero
  chips*)
  let rec helper (p_list:Model.player list) (acc:int) : int =
    match p_list with
    | [] -> acc
    | h::t -> if Model.get_chips h > 0 then helper t (acc + 1) else
      helper t acc
  in let x = helper (Model.get_players g) 0
  in if x > 1 then None
  else if x == 1 then
  let rec helper_2 (p_list:Model.player list) : Model.player option =
    match p_list with
    | [] -> failwith "Analytics - game_ended - 'living player not found'"
    | h::t -> if Model.get_chips h > 0 then Some h else helper_2 t
  in helper_2 (Model.get_players g)
  else failwith "Analytics - game_ended - 'zero players remaining alive'"
