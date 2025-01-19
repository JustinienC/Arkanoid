(* lib/game.mli *)
open Type
open State

module Game : sig
  val game_hello : unit -> unit
  val create_game_state : float * bool -> GameState.game_state
  val game_loop : unit -> GameState.game_state Iterator.flux
end