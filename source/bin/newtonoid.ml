(* newtonoid.ml *)
open Libnewtonoid
open Iterator
open Game
open Gamestate
open Ball
open Paddle
open Brick
open Config
open Bonus
open GameEngine

module Init = struct
  let dt = 1.0 /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx =fst (snd Config.screen_bounds) 
  let supy =snd (snd Config.screen_bounds) 
end

let graphic_format =
  Format.sprintf " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

(* Drawing functions remain the same as in your original implementation *)
let draw_ball ball =
  let (x, y) = Ball.get_position ball in
  let radius = Ball.get_radius ball in
  Graphics.set_color Graphics.white;
  Graphics.fill_circle
    (int_of_float x)
    (int_of_float y)
    (int_of_float radius)

let draw_brick brick =
  let (x, y) = Brick.get_position brick in
  let (w, h) = Brick.get_dimensions brick in
  Graphics.set_color (
    match Brick.get_type brick with
    | Brick.Reinforced _ -> Graphics.rgb 128 0 128 
    | Brick.Indestructible -> Graphics.rgb 105 105 105
    | Brick.Explosive -> Graphics.rgb 255 69 0 
    | Brick.Bonus bonus_effect ->
        (match bonus_effect with
        | Brick.Normal -> Graphics.blue
        | Brick.StretchPaddle -> Graphics.red
        | Brick.ShrinkPaddle -> Graphics.rgb 255 192 203
        | Brick.SpeedUpBall -> Graphics.yellow
        | Brick.SlowDownBall -> Graphics.cyan
        | Brick.ExtraLife -> Graphics.black
        | Brick.RestoreLife -> Graphics.rgb 89 128 117
        | Brick.MultiplyBall -> Graphics.rgb 22 255 118
        | Brick.ScoreBonus _ -> Graphics.rgb 255 215 212)

    | _ -> Graphics.blue
  );
  Graphics.fill_rect
    (int_of_float x)
    (int_of_float y)
    (int_of_float w)
    (int_of_float h)

let draw_paddle paddle =
  let (x,y) = Paddle.get_position paddle in
  let ((x1, y1), (x2, y2)) = Paddle.get_bounds paddle in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect
    (int_of_float x)
    (int_of_float y)
    (int_of_float (x2 -. x1))
    (int_of_float (y2 -. y1))

let draw_effects  (etat : Gamestate.t) =
        (* Draw falling bonus items *)
        List.iter (fun bonus ->
          let (x, y) = Bonus.get_position bonus in
          let (w, h) = Bonus.get_dimensions bonus in
          Graphics.set_color (
            match Bonus.get_effect bonus with
            | Brick.StretchPaddle -> Graphics.red
            | Brick.ShrinkPaddle -> Graphics.black
            | Brick.ExtraLife -> Graphics.green
            | Brick.RestoreLife -> Graphics.yellow
            | Brick.MultiplyBall -> Graphics.magenta
            | Brick.ScoreBonus _ -> Graphics.white
            | _ -> Graphics.blue
          );
          Graphics.fill_rect 
            (int_of_float x) 
            (int_of_float y) 
            (int_of_float w) 
            (int_of_float h)
        ) etat.bonus_items

let draw_state (etat : Gamestate.t) =
  Graphics.set_color (Graphics.rgb 40 40 40);
  Graphics.fill_rect 0 0 (int_of_float Box.supx) (int_of_float Box.supy);
  List.iter draw_brick etat.bricks;
  draw_paddle etat.paddle;
  draw_ball etat.ball;
  draw_effects etat;
  Graphics.set_color Graphics.white;
  (* Graphics.moveto 10 570;
  Graphics.draw_string (Printf.sprintf "Score: %d Lives: %d" etat.score etat.lives) *)
  let text = Printf.sprintf "Level :%d  Score: %d Lives: %d" etat.level etat.score etat.lives in
  let text_width = Graphics.text_size text |> fst in
  let text_x = (int_of_float Box.supx - text_width) / 2 in
  let text_y = 10 in 
  Graphics.moveto text_x text_y;
  Graphics.draw_string text

let score (etat : Gamestate.t) = etat.score

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
        Graphics.clear_graph ();
        draw_state etat;
        Graphics.synchronize ();
        Unix.sleepf Init.dt;
        loop flux_etat' (last_score + score etat)
    | _ -> assert false
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Final Score: %d@\n" score;
  Graphics.close_graph ()

let () =
  let game_stream = GameEngine.create_game_stream Init.dt () in
  draw game_stream