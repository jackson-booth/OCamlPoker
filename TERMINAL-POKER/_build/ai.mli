(**A module to provide the moves of an AI player*)
open Model
open Parser
open Roundwinner

(**[get_move g p] is the move of [p] in response to game [g]*)
val get_move : Model.game -> Model.player -> Parser.response
