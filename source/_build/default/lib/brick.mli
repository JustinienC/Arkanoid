(* lib/brick.mli *)
open Type

module type BRICK = sig
  type brick
  type brick_type = 
    | Normal
    | Indestructible
    | PowerUp of power_up

  and power_up = 
    | EnlargePaddle
    | SpeedUp
    | MultiplyBall

  val create_brick : float * float -> float -> float -> int -> int -> brick_type -> brick
  val is_destroyed : brick -> bool
  val hit : brick -> brick
  val get_value : brick -> int
  val get_position : brick -> float * float 
  
  val get_dimensions : brick -> float * float
  val get_bounds : brick -> (float * float) * (float * float)
  val check_collision : brick -> etat_balle -> bool
  val get_power_up : brick -> power_up option

end

module Brick : BRICK

module BrickSet : sig
  type t = Brick.brick list
  val create_grid : int -> int -> float -> float -> float -> t
  val update_bricks : t -> etat_balle -> t
  val get_colliding_bricks : t -> etat_balle -> t
end