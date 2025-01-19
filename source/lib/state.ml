open Type
open Brick
open Collision

  module GameState = struct
      type game_state = {
        ball: etat_balle;
        paddle: etat_racket;
        bricks: Brick.brick list;
        score: int;
        lives: int;
        running: bool;
      }
    
      let initial_state = {
        ball = {
          pos = (400., 300.);
          vel = (2., -2.);
          radius = 5.
        };
        paddle = {
          pos = (350., 50.);
          dim = (100., 10.);
          vel = (0., 0.)
        };
        bricks = BrickSet.create_grid 5 8 60. 20. 10.;
        score = 0;
        lives = 3;
        running = true;
      }

      let update_state dt input_state current_state =
        if not current_state.running then current_state
        else
          (* Mise à jour de la raquette *)
          let mouse_x = fst input_state in
          let new_paddle = 
            let (_, y) = current_state.paddle.pos in
            let target_x = mouse_x -. (fst current_state.paddle.dim) /. 2. in
            let current_x = fst current_state.paddle.pos in
            let dx = target_x -. current_x in
            let new_vx = dx *. 5. in
            {current_state.paddle with 
              pos = (current_x +. new_vx *. dt, y);
              vel = (new_vx, 0.)
            } in
          
          (* Mise à jour de la balle *)
          let new_ball = 
            let (x, y) = current_state.ball.pos in
            let (vx, vy) = current_state.ball.vel in
            {current_state.ball with
              pos = (x +. vx *. dt, y +. vy *. dt)
            } in
          
          (* Gestion des collisions *)
          let (state_after_collisions, remaining_bricks) = 
            Collision.handle_collisions 
              {current_state with ball = new_ball; paddle = new_paddle}
              current_state.bricks in
          
          (* Vérifier si la balle est perdue *)
          let (ball_lost, new_lives) =
            if snd state_after_collisions.ball.pos < 0. then
              (true, current_state.lives - 1)
            else
              (false, current_state.lives) in
          
          if ball_lost then
            if new_lives <= 0 then
              {state_after_collisions with
                running = false}
            else
              {state_after_collisions with
                ball = {
                  pos = (400., 300.);
                  vel = (2., -2.);
                  radius = 5.
                };
                lives = new_lives}
          else
            {state_after_collisions with
              bricks = remaining_bricks}
    end
    