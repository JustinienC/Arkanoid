


module type PADDLE = sig
  type t
  
  type paddle_effect = 
    | StretchPaddle
    | ShrinkPaddle


  val create : float * float -> float -> float -> float -> t

  val move : float->((float * float) * (float * float)) -> float -> t -> t

  val apply_effect : paddle_effect -> t -> t

  val get_position : t -> float*float
  val get_width : t -> float

  val get_dimensions : t -> float*float
  val get_bounds : t -> (float * float) * (float * float)
end


module Paddle : PADDLE = struct
  type t = {
    position: float*float;
    width: float;
    height: float;
    speed: float;
    
  }
  
  type paddle_effect = 
    | StretchPaddle
    | ShrinkPaddle
 

  let create (x, y) width height speed = {
    position = (x, y);
    width;
    height;
    speed;
  }

  let move  margin screen_bounds mouse_x paddle =
      let ((min_x, _), (max_x, _)) = screen_bounds in
      let new_x = 
        
            max margin (min (max_x -. paddle.width -. margin ) mouse_x)           
      in
      { paddle with position = (new_x, snd paddle.position) }
  
  let apply_effect effect paddle =
    match effect with
    | StretchPaddle -> { paddle with width = paddle.width *. 1.5 }
    | ShrinkPaddle -> { paddle with width = paddle.width *. 0.5 }
  

  
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