(**A module to provide analytic information about a computer game*)
open Model

(**[get_winner g] is either Some of the winner of game [g], or should no
player have won yet, none.*)
val get_winner : Model.game -> Model.player option
