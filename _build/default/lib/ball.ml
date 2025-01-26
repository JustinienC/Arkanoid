open Brick 
open Config 

module type BALL = sig
  type t
    
    (* 
   create : float*float -> float*float -> float -> float -> t
   crée une nouvelle instance de balle
   Paramètre (x,y) : float*float, la position initiale de la balle
   Paramètre (vx,vy) : float*float, la vitesse initiale de la balle
   Paramètre radius : float, le rayon de la balle (> 0)
   Paramètre mass : float, la masse de la balle (> 0)
   Résultat : une nouvelle instance de balle t
*)
  val create : (float * float) ->  (float * float) -> float -> float -> t



(*
   get_position : t -> float * float
   récupère la position actuelle de la balle
   Paramètre ball : t, la balle dont on veut la position
   Résultat : (x,y), coordonnées du centre de la balle
*)
  val get_position : t -> float * float

 
(*
   get_velocity : t -> float * float  
   récupère le vecteur vitesse actuel de la balle
   Paramètre ball : t, la balle dont on veut la vitesse
   Résultat : (vx,vy), composantes du vecteur vitesse
*)
  val get_velocity : t -> float * float

 (*
   get_radius : t -> float
   récupère le rayon de la balle
   Paramètre ball : t, la balle dont on veut le rayon 
   Résultat : rayon de la balle (> 0)
*) (*recupère le rayon*)
  val get_radius : t -> float
(*
   get_bounds : t -> (float * float) * (float * float)
   calcule les limites de la balle (rectangle englobant)
   Paramètre ball : t, la balle dont on veut les limites
   Résultat : ((x_min,y_min), (x_max,y_max)), coins du rectangle englobant
*)
  val get_bounds : t -> (float * float) * (float * float)

   (*
   set_velocity : float * float -> t -> t
   modifie le vecteur vitesse de la balle
   Paramètre (vx,vy) : float * float, nouvelles composantes du vecteur vitesse
   Paramètre ball : t, la balle à modifier
   Résultat : la balle avec sa vitesse mise à jour
*)
  val set_velocity : float * float -> t->t


(*
   is_lost : (float*float)*(float*float) -> t -> bool
   vérifie si la balle est sortie des limites du jeu
   Paramètre bounds : (float*float)*(float*float), limites du terrain
   Paramètre ball : t, la balle à tester
   Résultat : true si la balle est perdue, false sinon
*)  (* Vérifie si la balle est perdue *)
  val is_lost : (float * float) * (float * float) -> t -> bool

  (*
   set_position : t -> float*float -> t
   modifie la position de la balle
   Paramètre ball : t, la balle à modifier
   Paramètre (x,y) : float*float, nouvelle position
   Résultat : la balle avec sa position mise à jour
*)
  val set_position : t -> float * float -> t



  (*
    get_mass : t -> float
    récupère la masse de la balle
    Paramètre ball : t, la balle dont on veut la masse
    Résultat : la masse de la balle (> 0)
  *)
  val get_mass : t -> float


  (*
   calculate_reflection_angle : t -> float*float -> float
   calcule l'angle de réflexion lors d'une collision
   Paramètre ball : t, la balle concernée
   Paramètre (nx,ny) : float*float, vecteur normal à la surface de collision
   Résultat : angle de réflexion en radians
   Précondition : le vecteur normal doit être normalisé
*)
  val calculate_reflection_angle : t -> float * float -> float



(*
   dynamic_bounce : t -> float*float -> t
   calcule le rebond avec conservation de l'énergie cinétique
   Paramètre ball : t, la balle à faire rebondir
   Paramètre (nx,ny) : float*float, vecteur normal à la surface de collision
   Résultat : la balle avec sa vitesse mise à jour après rebond
   Précondition : le vecteur normal doit être normalisé
   Postcondition : ||v_finale|| = ||v_initiale|| (conservation de l'énergie)
*)
  val dynamic_bounce : t -> float * float -> t

  (*
   handle_wall_collision : t -> (float*float)*(float*float) -> t 
   gère la collision de la balle avec les murs
   Paramètre ball : t, la balle à traiter
   Paramètre bounds : (float*float)*(float*float), les limites du terrain ((x_min,y_min), (x_max,y_max))
   Résultat : la balle avec sa vitesse mise à jour après collision
*)
  val handle_wall_collision : t -> (float * float) * (float * float) -> t

  (*
   handle_paddle_collision : t -> float*float -> float*float -> (float*float)*(float*float) -> t
   gère la collision de la balle avec la raquette
   Paramètre ball : t, la balle à traiter  
   Paramètre paddle_pos : float*float, position de la raquette
   Paramètre paddle_dims : float*float, dimensions de la raquette (largeur, hauteur)
   Paramètre bounds : (float*float)*(float*float), les limites du terrain
   Résultat : la balle avec sa vitesse mise à jour après collision avec la raquette
*)
  val handle_paddle_collision : t -> float * float -> float * float -> (float * float) * (float * float) -> t


  (*
   handle_brick_collision : t -> float*float -> t
   gère la collision de la balle avec une brique
   Paramètre ball : t, la balle à traiter
   Paramètre brick_pos : float*float, position de la brique
   Résultat : la balle avec sa vitesse mise à jour après collision
   Précondition : la balle doit être en collision avec la brique
   Postcondition : conservation de l'énergie cinétique
*)
  val handle_brick_collision : t -> float * float -> t


  (*
   update : float -> float -> t -> t
   met à jour la position et la vitesse de la balle selon la gravité
   Paramètre gravity : float, accélération de la gravité
   Paramètre dt : float, intervalle de temps depuis la dernière mise à jour
   Paramètre ball : t, la balle à mettre à jour
   Résultat : la balle avec position et vitesse mises à jour
   Postcondition : conservation de l'énergie mécanique
*)
  val update : float -> float -> t -> t

 

(*
   bounce : (float*float)*(float*float) -> (float*float)*(float*float) -> t -> t
   gère le rebond de la balle sur une surface
   Paramètre surface_bounds : (float*float)*(float*float), limites de la surface ((x1,y1),(x2,y2))
   Paramètre normal : (float*float)*(float*float), vecteur normal à la surface au point d'impact
   Paramètre ball : t, la balle à faire rebondir
   Résultat : la balle avec sa trajectoire mise à jour après rebond
*)
  val bounce : (float * float) * (float * float) -> (float * float) * (float * float) -> t -> t


  (*
   apply_effect : Brick.bonus_effect -> t -> t
   applique un effet spécial à la balle
   Paramètre effect : Brick.bonus_effect, l'effet à appliquer
   Paramètre ball : t, la balle à modifier
   Résultat : la balle avec l'effet appliqué
*)
  val apply_effect : Brick.bonus_effect -> t -> t
end


module Ball : BALL = struct
  (*
   type t
   Représente une balle dans le jeu
   Champs :
   - position : float * float, coordonnées (x,y) du centre de la balle
   - velocity : float * float, vecteur vitesse (vx,vy) de la balle
   - radius : float, rayon de la balle (> 0)
   - mass : float, masse de la balle (> 0)  mais nous n'avons pas vraiment tenu en compte
   Invariants :
   - La balle doit rester dans les limites de l'écran 
   - La vitesse est bornée par une valeur maximale Config.max_ball_speed
*)
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
    (* Extraire les positions et vitesses *)
    let (x, y) = ball.position
    and (vx, vy) = ball.velocity in
     (* Extraire les limites des murs et de la raquette *)
    let (wx_min, wx_max) = fst wall_bounds
    and (wy_min, wy_max) = snd wall_bounds
    and (px_min, px_max) = fst paddle_bounds
    and (py_min, py_max) = snd paddle_bounds in
    
    (* Gestion du rebond horizontal *)
    let new_vx = 
      if x -. ball.radius <= wx_min || x +. ball.radius >= wx_max then -. vx
      else vx
    in
    (* Gestion du rebond vertical et avec la raquette *)
    (* Le facteur 1.1 accélère légèrement la balle à chaque rebond *)
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
      (* Augmente la vitesse de 20% en multipliant les composantes par 1.2 *)
        let (vx, vy) = ball.velocity in
        { ball with velocity = (vx *. 1.2, vy *. 1.2) }
    | Brick.SlowDownBall  -> 
       (* Réduit la vitesse de 20% en multipliant les composantes par 0.8 *)
        let (vx, vy) = ball.velocity in
        { ball with velocity = (vx *. 0.8, vy *. 0.8) }
        
    | Brick.MultiplyBall -> ball    (*nous ne l'avons pas implémenté*)
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
    (* Calcul des composantes du vecteur entre la balle et le point d'impact *)
    let (ball_x, ball_y) = ball.position 
    and (surf_x, surf_y) = surface_point in
    let dx = ball_x -. surf_x 
    and dy = ball_y -. surf_y in
    (* Retourne l'angle en radians par rapport à l'horizontale *)
    atan2 dy dx

  (* Réflexion dynamique avec conservation partielle de l'énergie *)
  let dynamic_bounce ball surface_point =
     (* Calcul de la vitesse initiale *)
    let (vx, vy) = ball.velocity in
    let impact_angle = calculate_reflection_angle ball surface_point in
    let speed = sqrt (vx *. vx +. vy *. vy) in
    (* Application de la conservation de l'énergie avec la masse *)
    let new_speed = speed *. ball.mass in
    (* Calcul des nouvelles composantes de vitesse *)
    let new_vx = new_speed *. cos impact_angle 
    and new_vy = new_speed *. sin impact_angle in
    
    { ball with velocity = (new_vx, new_vy) }

  (* Gestion des collisions avec les murs *)
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
      (* Le relative_x varie de 0 à 1 selon le point d'impact sur la raquette *)
      let relative_x = (ball_x -. px) /. pw in
      
      (* Calcul de l'angle de rebond *)
      (* angle_multiplier varie de -1 à 1 pour déterminer l'angle de rebond *)
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
    (* Récupération du vecteur vitesse actuel de la balle *)
    let (vx, vy) = ball.velocity in
     (* Point d'impact sur la brique pour le calcul de l'angle de réflexion *)
    let impact_point = (x,y) in
    (* Calcul de l'angle de réflexion basé sur la position de la balle 
       et le point d'impact sur la brique *)
    let collision_angle = calculate_reflection_angle ball impact_point in
    
    (* Conservation de l'énergie cinétique : la vitesse totale reste constante *)
    let new_speed = sqrt (vx *. vx +. vy *. vy) in
    
    (* Calcul des nouvelles composantes de la vitesse :
       - new_vx : composante horizontale basée sur l'angle de collision
       - new_vy : composante verticale, toujours vers le haut (d'où le -. abs_float) 
         pour éviter que la balle ne reste coincée dans la brique *)
    let new_vx = new_speed *. cos collision_angle
    and new_vy = -. (abs_float (new_speed *. sin collision_angle)) in

    { ball with velocity = (new_vx, new_vy) }

end