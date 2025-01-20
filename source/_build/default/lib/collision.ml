(* collision.ml *)
open Type
open Brick
open Gamestate

module Collision = struct
  let check_ball_brick_collision (ball: etat_balle) (brick: Brick.brick) =
    let ((bx1, by1), (bx2, by2)) = Brick.get_bounds brick in
    let (ball_x, ball_y) = ball.pos in
    let r = ball.radius in
    
    (* Trouver le point le plus proche de la ball sur la brique *)
    let closest_x = max bx1 (min ball_x bx2) in
    let closest_y = max by1 (min ball_y by2) in
    
    (* Calculer la distance entre la ball et ce point *)
    let dx = ball_x -. closest_x in
    let dy = ball_y -. closest_y in
    let distance = sqrt(dx *. dx +. dy *. dy) in
    
    if distance <= r then
      (* Calculer la normale de collision *)
      let normal_x = dx /. distance in
      let normal_y = dy /. distance in
      (true, (normal_x, normal_y))
    else
      (false, (0., 0.))

  let check_ball_paddle_collision (ball: etat_balle) (racket: etat_racket) =
    let (rx, ry) = racket.pos in
    let (rw, rh) = racket.dim in
    let (ball_x, ball_y) = ball.pos in
    let r = ball.radius in
    
    (* Rectangle englobant de la raquette *)
    let rx1 = rx -. rw /. 2. in
    let rx2 = rx +. rw /. 2. in
    let ry1 = ry -. rh /. 2. in
    let ry2 = ry +. rh /. 2. in
    
    (* Même logique que pour les briques *)
    let closest_x = max rx1 (min ball_x rx2) in
    let closest_y = max ry1 (min ball_y ry2) in
    
    let dx = ball_x -. closest_x in
    let dy = ball_y -. closest_y in
    let distance = sqrt(dx *. dx +. dy *. dy) in
    
    if distance <= r then
      let normal_x = dx /. distance in
      let normal_y = dy /. distance in
      (true, (normal_x, normal_y))
    else
      (false, (0., 0.))

  let handle_ball_collision velocity normal =
    let (vx, vy) = velocity in
    let (nx, ny) = normal in
    let dot_product = vx *. nx +. vy *. ny in
    let reflected_x = vx -. (2. *. dot_product *. nx) in
    let reflected_y = vy -. (2. *. dot_product *. ny) in
    (reflected_x, reflected_y)

  let handle_collisions state bricks =
    let ball = state.ball in
    
    (* Vérifier la collision avec la raquette *)
    let (paddle_collision, paddle_normal) = 
      check_ball_paddle_collision ball state.paddle in
    
    (* Mettre à jour la vitesse de la ball si collision avec la raquette *)
    let ball = 
      if paddle_collision then
        let new_vel = handle_ball_collision ball.vel paddle_normal in
        {ball with vel = new_vel}
      else ball in
    
    (* Vérifier les collisions avec les briques *)
    let (collided_bricks, remaining_bricks) = 
      List.partition (fun brick -> 
        fst (check_ball_brick_collision ball brick)) bricks in
    
    (* Mettre à jour la vitesse de la ball pour la première brique touchée *)
    let ball = 
      match collided_bricks with
      | brick :: _ ->
          let (_, normal) = check_ball_brick_collision ball brick in
          let new_vel = handle_ball_collision ball.vel normal in
          {ball with vel = new_vel}
      | [] -> ball in
    
    (* Calculer les points gagnés *)
    let points_gained = 
      List.fold_left (fun acc brick -> 
        acc + Brick.get_value brick) 0 collided_bricks in
    
    (* Retourner le nouvel état *)
    let new_state = {
      state with 
      ball = ball;
      score = state.score + points_gained
    } in
    
    (new_state, remaining_bricks)
end
