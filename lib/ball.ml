open Brick 
open Config 

module type BALL = sig
  type t
  

  (* Crée une nouvelle balle *)
  val create : (float * float) ->  (float * float) -> float -> float -> t

  (* Met à jour la position et la vitesse de la balle *)
  (*gravity->dt->t->t*)
  val update : float -> float -> t -> t

  (* Gère les rebonds *)
  val bounce : (float * float) * (float * float) -> (float * float) * (float * float) -> t -> t

  (* Application d'effets spéciaux *)
  val apply_effect : Brick.bonus_effect -> t -> t

  (* Vérifie si la balle est perdue *)
  val is_lost : (float * float) * (float * float) -> t -> bool

  (* Récupère la position actuelle *)
  val get_position : t -> float * float

  (* Récupère la vitesse actuelle *)
  val get_velocity : t -> float * float

  (*recupère le rayon*)
  val get_radius : t -> float

  (* Récupère les limites de la balle *)
  val get_bounds : t -> (float * float) * (float * float)

    (* Définit la nouvelle vitesse de la balle *)
  val set_velocity : float * float -> t->t

  val set_position : t -> float * float -> t

  val calculate_reflection_angle : t -> float * float -> float
  val dynamic_bounce : t -> float * float -> t
  val handle_wall_collision : t -> (float * float) * (float * float) -> t
  val handle_paddle_collision : t -> float * float -> float * float -> (float * float) * (float * float) -> t

  val handle_brick_collision : t -> float * float -> t

  val get_mass : t -> float

end


module Ball : BALL = struct
  type t = {
    position: float * float;
    velocity: float * float;
    radius: float;
    mass: float;
  }
  
  type ball_effect = 
    | SpeedUpBall
    | SlowDownBall
    | MultiplyBall
   

  let create (x,y) (vx,vy) radius mass = {
    position = (x, y);
    velocity = (vx, vy);
    radius;
    mass;
  }

  let update gravity dt ball = 
    let (x, y) = ball.position
    and (vx, vy) = ball.velocity in
    {
      ball with 
      position = (x +. vx *. dt, y +. vy *. dt -. 0.5 *. gravity *. dt *. dt);
      velocity = (vx, vy -. gravity *. dt)
    }

  let bounce wall_bounds paddle_bounds ball =
    let (x, y) = ball.position
    and (vx, vy) = ball.velocity in
    let (wx_min, wx_max) = fst wall_bounds
    and (wy_min, wy_max) = snd wall_bounds
    and (px_min, px_max) = fst paddle_bounds
    and (py_min, py_max) = snd paddle_bounds in
    
    let new_vx = 
      if x -. ball.radius <= wx_min || x +. ball.radius >= wx_max then -. vx
      else vx
    in
    let new_vy = 
      if y +. ball.radius >= wy_max then -. vy
      else if y -. ball.radius <= py_max && 
              x >= px_min && x <= px_max then 
        -. vy *. 1.1
      else vy
    in
    { ball with velocity = (new_vx, new_vy) }

  let apply_effect effect ball =
    match effect with
    | Brick.SpeedUpBall -> 
        let (vx, vy) = ball.velocity in
        { ball with velocity = (vx *. 1.2, vy *. 1.2) }
    | Brick.SlowDownBall  -> 
        let (vx, vy) = ball.velocity in
        { ball with velocity = (vx *. 0.8, vy *. 0.8) }
    | Brick.MultiplyBall -> ball
    | _ -> ball


  let is_lost wall_bounds ball =
    let (_, y) = ball.position
    and (_, wy_min) = fst wall_bounds in
    ( (y -. ball.radius <= wy_min) || y < snd Config.paddle_position )

  let get_position ball = ball.position

  let get_velocity ball = ball.velocity

  let get_bounds ball =
    let (x, y) = ball.position in
    ((x -. ball.radius, y -. ball.radius), 
     (x +. ball.radius, y +. ball.radius))

  let get_radius ball = ball.radius

  let get_mass ball = ball.mass

  let set_velocity new_velocity ball  = 
    { ball with velocity = new_velocity }


  let set_position ball new_position = 
    { ball with position = new_position }



  (* Calcule l'angle de réflexion en fonction du point d'impact *)
  let calculate_reflection_angle ball surface_point =
    let (ball_x, ball_y) = ball.position 
    and (surf_x, surf_y) = surface_point in
    let dx = ball_x -. surf_x 
    and dy = ball_y -. surf_y in
    atan2 dy dx

  (* Réflexion dynamique avec conservation partielle de l'énergie *)
  let dynamic_bounce ball surface_point =
    let (vx, vy) = ball.velocity in
    let impact_angle = calculate_reflection_angle ball surface_point in
    let speed = sqrt (vx *. vx +. vy *. vy) in
    let new_speed = speed *. ball.mass in
    
    let new_vx = new_speed *. cos impact_angle 
    and new_vy = new_speed *. sin impact_angle in
    
    { ball with velocity = (new_vx, new_vy) }

  (* Gestion des collisions avec les murs *)
let handle_wall_collision ball screen_bounds =
  let (x, y) = ball.position
  and (vx, vy) = ball.velocity in
  let ((min_x, min_y), (max_x, max_y)) = screen_bounds in
  
  let current_speed = sqrt(vx *. vx +. vy *. vy) in
  
  (* Rebonds sur les murs latéraux *)
  let (new_x, new_vx, new_vy) = 
    if x -. ball.radius < min_x then 
      let angle = Float.pi /. 6. +. Random.float (Float.pi /. 6.) in
      (min_x +. ball.radius, 
       current_speed *. cos angle,
       -. current_speed *. sin angle)
    else if x +. ball.radius > max_x then 
      let angle = Float.pi -. (Float.pi /. 6. +. Random.float (Float.pi /. 6.)) in
      (max_x -. ball.radius, 
       current_speed *. cos angle,
       -. current_speed *. sin angle)
    else 
      (x, vx, vy)
  in
  
  (* Rebond pour le mur du haut uniquement *)
  let (new_y, final_vy) = 
    if y +. ball.radius > max_y then
      (max_y -. ball.radius, -.abs_float new_vy)
    else
      (y, new_vy)
  in
  
  { ball with 
    position = (new_x, new_y);
    velocity = (new_vx, final_vy)
  }

  (* Collision avec la raquette avec angle variable *)
  let handle_paddle_collision ball (px, py) (pw, ph) screen_bounds =
    let (ball_x, ball_y) = ball.position 
    and (vx, vy) = ball.velocity 
    and ((min_x, min_y), (max_x, max_y)) = screen_bounds in
    

    (* Vérification de la collision *)
    if ball_y -. ball.radius <= py +. ph && 
       ball_y +. ball.radius >= py &&
       ball_x >= px && 
       ball_x <= px +. pw then
      
      (* Position relative sur la raquette *)
      let relative_x = (ball_x -. px) /. pw in
      
      (* Calcul de l'angle de rebond *)
      let angle_multiplier = (relative_x -. 0.5) *. 2.0 in (* -1 to 1 *)
      
      (* Conservation de la vitesse totale *)
      let current_speed = sqrt (vx *. vx +. vy *. vy) in
      let new_speed = current_speed *. 1.0 in (* Léger boost *)
      
      (* Nouvelle trajectoire *)
      let new_vy = abs_float (new_speed *. cos (Float.pi /. 4. *. angle_multiplier)) in
      let new_vx = new_speed *. sin (Float.pi /. 4. *. angle_multiplier) in
      
      (* Ajustement de la position pour rester dans l'écran *)
      let adjusted_x = 
        max min_x (min (max_x -. ball.radius) ball_x)
      in
      let adjusted_y = 
        max (py +. ph +. ball.radius) (min (max_y -.min_y-. ball.radius) (py +. ph +. ball.radius)) 
      in
      
      { ball with 
        position = (adjusted_x, adjusted_y);
        velocity = (new_vx, new_vy) 
      }
    else 
      ball

  (* Collision avec les briques *)
  let handle_brick_collision ball (x,y) =
    let (vx, vy) = ball.velocity in
    let impact_point = (x,y) in
    let collision_angle = calculate_reflection_angle ball impact_point in
    
    let new_speed = sqrt (vx *. vx +. vy *. vy) in
    
    let new_vx = new_speed *. cos collision_angle
    and new_vy = -. (abs_float (new_speed *. sin collision_angle)) in
    { ball with velocity = (new_vx, new_vy) }

end