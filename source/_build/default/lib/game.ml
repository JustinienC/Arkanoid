open Iterator
open Type
open Brick
open Gamestate
open Menu
open Power

let game_hello () = 
  print_endline "Bienvenue dans Newtonoid!"

module BrickInit = struct
  let rows =7
  let cols=14
  let brick_width=40.
  let brick_height=10.
  let spacing=15.
  let power_ratio = 0.1
end

module Game = struct
  (* Configuration initiale *)
  let initial_state = 
    let b = BrickSet.create_grid BrickInit.rows BrickInit.cols BrickInit.brick_width BrickInit.brick_height BrickInit.spacing BrickInit.power_ratio in
  {
    ball = {
      pos = (400., 300.);
      vel = (400.0, -400.);
      radius = 5.
    };
    paddle = {
      pos = (350., 50.);
      dim = (100., 10.);
      vel = (0., 0.)
    };
    bricks = b;
    power = PowerSet.create_from_bricks b;
    actif_power = [];
    score = 0;
    lives = 300;
    running = true
  }

  let norme (x, y) = sqrt(x *. x +. y *. y)

  (* Création d'un nouvel état à partir de l'entrée souris *)
  let create_game_state (mouse_x, _) =
    let paddle_x = max 10. (min (790. -. 100.) mouse_x) in
    { initial_state with
      paddle = { initial_state.paddle with pos = (paddle_x, 50.) }
    }

  (* Gestion des collisions avec les murs *)
  let handle_wall_collision ball =
    let (x, y) = ball.pos in
    let (vx, vy) = ball.vel in
    let new_vx = if x <= 10. || x >= 790. then -.vx else vx in
    let new_vy = if y <= 10. || y >= 590. then -.vy else vy in
    { ball with vel = (new_vx, new_vy) }

  let handle_paddle_collision ball (paddle: etat_racket) =
        let (px, py) = paddle.pos in
        let (pw, ph) = paddle.dim in
        let (bx, by) = ball.pos in
        let (vx, vy) = ball.vel in
        
        if by <= py +. ph then

          (* Modification ici : on vérifie la collision sur le dessus de la raquette *)
          if by >= py && by <= py +. ph && bx >= px && bx <= px +. pw then
            let relative_x = (bx -. px) /. pw in  (* Position relative sur la raquette *)
            let angle = (relative_x -. 0.5) *. 1.0 in  (* Angle de rebond basé sur la position *)
            let speed = sqrt (vx *. vx +. vy *. vy) in
            let new_vx = speed *. sin angle in
            let new_vy = abs_float (speed *. cos angle) in  (* On s'assure que la balle part vers le haut *)
            { ball with vel = (new_vx, new_vy) }
          else
            ball
        else
            ball

  let check_ball_lost ball =
            snd ball.pos < snd initial_state.paddle.pos  (* Ball en dessous de la raquette *)

  (* Mise à jour de l'état du jeu *)
  let update_state dt (mouse_x, _) state =
    if not state.running then 
      state
    else
      (* Mise à jour de la raquette *)
      let new_paddle_x = max 10. (min (790. -. 100.) mouse_x) in
      let new_paddle = { state.paddle with pos = (new_paddle_x, 50.) } in

      (* Mise à jour de la balle *)
      let (x, y) = state.ball.pos in
      let (vx, vy) = state.ball.vel in
      let new_ball_pos = (x +. vx *. dt, y +. vy *. dt) in
      let new_ball = { state.ball with pos = new_ball_pos } in

      (* Application des collisions *)
      let ball_after_walls = handle_wall_collision new_ball in
      let ball_after_paddle = handle_paddle_collision ball_after_walls new_paddle in

      (* Gestion des collisions avec les briques *)
      let colliding_bricks = BrickSet.get_colliding_bricks state.bricks ball_after_paddle in
      let new_bricks = BrickSet.update_bricks state.bricks ball_after_paddle in
      let score_increment = List.length colliding_bricks * 10 in
      
      (* Gestion des collisions entre raquette et power *)
      let colliding_powers = PowerSet.get_colliding_powers state.power state.paddle in
      let new_power = PowerSet.update_powers state.paddle dt state.power new_bricks in

      
      let activatePower = List.map (fun power -> Power.get_type power) colliding_powers in

      let ball_after_bricks =
        if List.length colliding_bricks > 0 then
          { ball_after_paddle with vel = let (vx, vy) = ball_after_paddle.vel in (vx, -.vy) }
        else
          ball_after_paddle
      in

      let ball_after_activating_powers = 
      if List.mem Brick.SpeedUp activatePower then
        let (vx, vy) = ball_after_bricks.vel in
        {ball_after_bricks with vel = (1.5 *. vx, 1.5 *. vy)}
      else 
        ball_after_bricks
      in

      let paddle_after_activating_power = 
        if List.mem Brick.EnlargePaddle activatePower then
          let (dimx, dimy) = new_paddle.dim in
          {new_paddle with dim = (dimx *. 1.5, dimy)}
        else
          new_paddle
      in

      (*let ball_after_power_dissipate = 
        if*) 

      (* Vérification de la perte de la balle ou qu'il n'y a plus de brique*)
      if snd ball_after_activating_powers.pos < snd initial_state.paddle.pos then
        if state.lives <= 1 then
          begin
            (*show_defeat ();*)
            { state with running = false }
          end
      else
        let direction_x = fst paddle_after_activating_power.pos -. fst initial_state.ball.pos +. fst paddle_after_activating_power.dim /. 2.0 in
        let direction_y = snd paddle_after_activating_power.pos -. snd initial_state.ball.pos in
        let norm = norme (direction_x, direction_y) in
        let velocity_x = sqrt (400. *. 400. +. 400. *. 400.) *. (direction_x /. norm) in
        let velocity_y = sqrt (400. *. 400. +. 400. *. 400.) *. (direction_y /. norm) in
        { state with
          ball = { state.ball with
          pos = initial_state.ball.pos; 
          vel = (velocity_x, velocity_y) 
        };
        lives = state.lives - 1;
        score = state.score + score_increment
        }
      else if List.length state.bricks == 0 then
        begin
          (*show_victory ();*)
          { state with running = false }
        end
      else
        { state with
          ball = ball_after_activating_powers;
          paddle = paddle_after_activating_power;
          bricks = new_bricks;
          power = new_power;
          score = state.score + score_increment }

  

  (* Création du flux d'états du jeu *)
  let game_loop () =
    let dt = 1. /. 60. in  (* 60 FPS *)
    Flux.unfold
      (fun state ->
        Some (state, update_state dt ((fst (Graphics.mouse_pos ()) |> float_of_int) -. fst state.paddle.dim /. 2.0, false) state))
      (create_game_state (400., false))

      
end
