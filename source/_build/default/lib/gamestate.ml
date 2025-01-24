open Brick
open Power
open Type
(* État du jeu: game_state *)
type etat = {
  balls: etat_balle list;
  paddle: etat_racket;
  bricks: Brick.brick list;
  power: Power.power list;
  actif_power : (Brick.power_up * float) list;
  power_time : float;
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
