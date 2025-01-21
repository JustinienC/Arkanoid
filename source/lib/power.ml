open Game
open Brick
open Type

type power = {
  pos : float*float;
  width : float;
  height : float;
  speed : float;
  typeP : Brick.power_up; 
}
let create_power pos width height speed typeP = 
  { pos; width; height; speed; typeP }

let getType power = power.typeP

let getSpeed power = power.speed

let get_position power = power.pos

let get_dimensions power = (power.width, power.height)

let get_bounds power =
  let (x, y) = power.pos in
  ((x, y), (x +. power.width, y +. power.height))

let check_paddle_collision power (paddle:etat_racket) = 
  let ((px1, py1), (px2, py2)) = get_bounds power in
  let (paddle_x, paddle_y) = paddle.pos in
  let (paddle_width, paddle_height) = paddle.dim in
   1.0 (*Inutile, pour pas qu'il y ait une erreur de compilation*)
  (* (* Distance la plus proche entre le powerup et la raquette *)
  let closest_x = max px1 (min ball_x bx2) in
  let closest_y = max by1 (min ball_y by2) in
  
  (* Distance entre le point le plus proche et le centre de la raquette *)
  let dx = ball_x -. closest_x in
  let dy = ball_y -. closest_y in
  
  (* Si la distance est inférieure au rayon, il y a collision *)
  (dx *. dx) +. (dy *. dy) <= r *. r *)