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

type etat_win = {
  quitButton : bool;
  replayButton : bool;
  scoreV : int}

type etat_lose = {
  bQuit : bool;
  bReplay : bool;
  scoreL : int
}
