(* lib/config.ml *)
module Config = struct
  let initial_lives = 100
  let initial_score = 0
  let initial_level = 1
  let base_bonus=10
  let paddle_position=(350. , 50. )
  let paddle_width = 100.
  let paddle_height = 10.
  let paddle_magin = 0.

  let paddle_velocity = 10.


  let paddle_stretch_factor = 1.1
  let paddle_shrink_factor = 0.75
  let ball_radius = 5.
  let ball_mass = 0.85
  let initial_ball_speed = 400.
  let brick_width =40. 
  let brick_height = 10.
  let brick_spacing = 15.
  let rows = 7
  let cols = 14
  let gravity = 9.81
  let screen_bounds =  ((0., 0.), (790., 590.))

  let bonus_velocity = 200.
  let bonus_lifetime = 3.

  let bonus_dimensions = (20., 12.)

  let ball_factor = 1.1

end