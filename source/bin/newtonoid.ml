(* bin/newtonoid.ml mis à jour *)
open Libnewtonoid
open Iterator
open State
open Game
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
let draw_paddle state =
  let (x, y) = state.paddle.pos in
  let (w, h) = state.paddle.dim in
  set_color white;
  fill_rect (int_of_float x) (int_of_float y) 
            (int_of_float w) (int_of_float h)

let draw_ball state =
  let (x, y) = state.ball.pos in
  set_color white;
  fill_circle (int_of_float x) (int_of_float y) 
              (int_of_float state.ball.radius)

let draw_brick brick =
  let (x, y) = Brick.get_position brick in
  let (w, h) = Brick.get_dimensions brick in
  match Brick.get_power_up brick with
  | Some _ -> set_color (rgb 255 215 0)  (* Or pour les briques bonus *)
  | None -> set_color white;
  fill_rect (int_of_float x) (int_of_float y)
            (int_of_float w) (int_of_float h)

let draw_score state =
  set_color white;
  moveto 10 10;
  draw_string (Printf.sprintf "Score: %d   Lives: %d" state.score state.lives)

let draw_state state =
  List.iter draw_brick state.bricks;
  draw_paddle state;
  draw_ball state;
  draw_score state

(* Fonction score *)
let score state = 
  state.score

(* Boucle principale *)
let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.uncons flux_etat with
    | None -> last_score
    | Some (etat, flux_etat') ->
      clear_graph ();
      draw_state etat;
      synchronize ();
      Unix.sleepf (1. /. 60.);
      loop flux_etat' (last_score + score etat)
  in
  open_graph graphic_format;
  auto_synchronize false;
  let final_score = loop flux_etat 0 in
  Format.printf "Score final : %d@." final_score;
  close_graph ()

let () = 
  Random.self_init ();
  game_hello ();
  let game_flux = game_loop () in
  draw game_flux
