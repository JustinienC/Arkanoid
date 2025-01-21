open Brick
open Type
(* État du jeu: game_state *)
type etat = {
  ball: etat_balle;
  paddle: etat_racket;
  bricks: Brick.brick list;
  score: int;
  lives: int;
  running: bool;
}
