open Brick
open Config

module type PADDLE = sig
  type t
  
  val create : float * float -> float -> float -> float -> t

  val move : float->((float * float) * (float * float)) -> float -> t -> t

  val apply_effect : Brick.bonus_effect -> t -> t

  val get_position : t -> float*float
  val get_width : t -> float

  val get_dimensions : t -> float*float
  val get_bounds : t -> (float * float) * (float * float)

  val remove_effect : Brick.bonus_effect -> t -> t
end


module Paddle : PADDLE = struct

  type effect_state = {
    stretch_factor: float;
    effect_active: bool;
  }
  
  type t = {
    position: float*float;
    width: float;
    height: float;
    speed: float;
    effect_state: effect_state;
    
  }
  

 
  

  let create (x, y) width height speed = {
    position = (x, y);
    width;
    height;
    speed;
    effect_state = { stretch_factor = 1.; effect_active = false }
  }

  let move  margin screen_bounds mouse_x paddle =
      let ((min_x, _), (max_x, _)) = screen_bounds in
      let new_x = 
        
            max margin (min (max_x -. paddle.width -. margin ) mouse_x)           
      in
      { paddle with position = (new_x, snd paddle.position) }

  
      let apply_effect effect paddle =
        match effect with
        | Brick.StretchPaddle -> 
            let new_width = min 
              (paddle.width *. Config.paddle_stretch_factor)
              ((fst (snd Config.screen_bounds)) /. 2.0) in 
            { paddle with 
              width = new_width;
              effect_state = { stretch_factor = Config.paddle_stretch_factor; 
                              effect_active = true }
            }
        | Brick.ShrinkPaddle ->
          let new_width = max 
            (paddle.width *. Config.paddle_shrink_factor)
            (Config.paddle_width /. 4.0) in 
          { paddle with 
            width = new_width;
            effect_state = { stretch_factor = Config.paddle_shrink_factor;
                            effect_active = true }
          }
      | _ -> paddle
      let remove_effect effect paddle =
        if paddle.effect_state.effect_active then
          match effect with
          | Brick.StretchPaddle when paddle.width > Config.paddle_width ->
              (* Inversion du stretch mais pas en dessous de la largeur initiale *)
              let new_width = max
                (paddle.width /. Config.paddle_stretch_factor)
                Config.paddle_width in
              { paddle with
                width = new_width;
                effect_state = { stretch_factor = 1.0; effect_active = false }
              }
          | Brick.ShrinkPaddle when paddle.width < Config.paddle_width ->
              (* Pour un shrink, on revient directement à la largeur initiale *)
              { paddle with
                width = Config.paddle_width;
                effect_state = { stretch_factor = 1.0; effect_active = false }
              }
          | _ -> paddle  
        else paddle
  let get_position paddle = paddle.position
  
  let get_width paddle = paddle.width
  
  let get_bounds paddle =
    let (x, y) = paddle.position in
    let half_width = paddle.width /. 2. in
    let paddle_min_x = x -. half_width in
    let paddle_max_x = x +. half_width in
    let paddle_min_y = y in
    let paddle_max_y = y +. paddle.height in
    ((paddle_min_x, paddle_min_y), (paddle_max_x, paddle_max_y))

  let get_dimensions paddle = (paddle.width, paddle.height)

end