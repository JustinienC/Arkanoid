(* bin/newtonoid.ml mis à jour *)
open Libnewtonoid
open Iterator
open State
open Brick
open Type
open Game
open Gamestate
open Graphics


module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

(* Fonctions de dessin *)
let draw_brick brick =
  let (x, y) = Brick.get_position brick in
  let (w, h) = Brick.get_dimensions brick in
  Graphics.set_color (
    match Brick.get_power_up brick with
    | Some Brick.EnlargePaddle -> Graphics.red
    | Some Brick.SpeedUp -> Graphics.yellow
    | Some Brick.MultiplyBall -> Graphics.green
    | None -> Graphics.blue
  );
  Graphics.fill_rect (int_of_float x) (int_of_float y) 
                    (int_of_float w) (int_of_float h)

let draw_ball ball =
  let (x, y) = ball.pos in
  Graphics.set_color Graphics.white;
  Graphics.fill_circle 
    (int_of_float x) 
    (int_of_float y) 
    (int_of_float ball.radius)

let draw_paddle (paddle:etat_racket) =
  let (x, y) = paddle.pos in
  let (w, h) = paddle.dim in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 
    (int_of_float x) 
    (int_of_float y) 
    (int_of_float w) 
    (int_of_float h)

let draw_state (state : etat)=
  (* Fond de couleur *)
  Graphics.set_color (Graphics.rgb 40 40 40);  
  Graphics.fill_rect 0 0 800 600;
  
  (* Dessiner tous les éléments *)
  List.iter draw_brick state.bricks;
  draw_paddle state.paddle;
  draw_ball state.ball;
  
  (* Interface utilisateur *)
  Graphics.set_color Graphics.white;
  Graphics.moveto 10 570;
  Graphics.draw_string (Printf.sprintf "Score: %d   Lives: %d" 
                       state.score state.lives)

let score state = state.score

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.uncons flux_etat with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      draw_state etat;
      Graphics.synchronize ();
      Unix.sleepf (1.0 /. 60.0);  (* 60 FPS *)
      loop flux_etat' (score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let final_score = loop flux_etat 0 in
  Format.printf "Score final : %d@." final_score;
  Graphics.close_graph ()

let () =
  Random.self_init ();
  game_hello ();
  let flux_etat = Game.game_loop () in
  draw flux_etat