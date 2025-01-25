open Config
open Brick
open Paddle

module type BONUS= sig
  type t

  val create : float * float -> Brick.bonus_effect -> t
  val update : float -> t -> t
  val is_expired : t -> bool
  val get_position : t -> float * float
  val get_dimensions : t -> float * float
  val get_bounds : t -> (float * float) * (float * float)
  val apply_effect : t -> Paddle.t -> Paddle.t
  val check_collision : t -> Paddle.t -> bool
  val get_effect : t -> Brick.bonus_effect
end

module Bonus : BONUS = struct

  type t = {
    position: float * float;
    velocity: float;
    effect: Brick.bonus_effect;
    lifetime: float; 
  

  }

  let create (x, y) effect = {
    position = (x, y);
    velocity = Config.bonus_velocity;  
    effect = effect;
    lifetime = Config.bonus_lifetime;  
  }

  let update dt bonus = {
    bonus with 
    position = (fst bonus.position, snd bonus.position -. bonus.velocity *. dt);
    lifetime = bonus.lifetime -. dt;
  }

  let get_effect bonus = bonus.effect
  (* let is_expired bonus = bonus.lifetime <= 0. *)

  let is_expired bonus = 
    let (_, y) = bonus.position in
    bonus.lifetime <= 0. || y <= snd Config.paddle_position 
  let get_position bonus = bonus.position

  let get_dimensions _ = Config.bonus_dimensions
  let get_bounds bonus =
    let (x, y) = bonus.position in
    let (w, h) = get_dimensions bonus in
    ((x, y), (x +. w, y +. h))

  let apply_effect bonus paddle = 
    match bonus.effect with
    | Brick.StretchPaddle -> Paddle.apply_effect Paddle.StretchPaddle paddle
    | Brick.ShrinkPaddle -> Paddle.apply_effect Paddle.ShrinkPaddle paddle
    |_-> paddle

  let check_collision bonus paddle =
    let ((bonus_x1, bonus_y1), (bonus_x2, bonus_y2)) = get_bounds bonus in
    let ((paddle_x1, paddle_y1), (paddle_x2, paddle_y2)) = Paddle.get_bounds paddle in
    
    bonus_x1 < paddle_x2 && 
    bonus_x2 > paddle_x1 && 
    bonus_y1 < paddle_y2 && 
    bonus_y2 > paddle_y1
end