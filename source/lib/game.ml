open Iterator
open Config
open Ball
open Paddle
open Brick
open Gamestate
open Quadtree
open Bonus

module GameEngine = struct
  (* Configuration initiale *)
  let initial_state = Gamestate.create ()



  let apply_bonus_score effect (state:Gamestate.t)=
    match effect with
    | Brick.ScoreBonus sc -> 
        { state with score = state.score + sc }
    | _ -> state
  let apply_bonus_life effect (state:Gamestate.t) =
    match effect with
    | Brick.ExtraLife -> 
        { state with lives = state.lives + 1 }
    | Brick.RestoreLife -> 
        { state with lives = Config.initial_lives }
    | _ -> state

let game_loop dt mouse_state (state:Gamestate.t) =
    let (mouse_x, _) = mouse_state in
      
    (* Mise à jour de la raquette *)
  let new_paddle = Paddle.move Config.paddle_magin Config.screen_bounds ( mouse_x) state.paddle in


    (* Mise à jour de la balle *)
    let (x, y) = Ball.get_position state.ball in
    let (vx, vy) = Ball.get_velocity state.ball in
    let new_ball_pos = (x +. vx *. dt, y +. vy *. dt) in
    let new_ball = Ball.set_position state.ball new_ball_pos in

    (* Application des collisions *)
    let ball_after_walls = Ball.handle_wall_collision new_ball  Config.screen_bounds in
    let ball_after_paddle = Ball.handle_paddle_collision ball_after_walls (Paddle.get_position new_paddle) 
    (Paddle.get_dimensions new_paddle)   Config.screen_bounds
  in

    let (candidate_bricks,others_brick) = Quadtree.query state.quadtree ball_after_paddle in
      let (colliding_bricks,candidate_bricks_without )= List.partition (fun brick -> Brick.check_collision 
      brick (Ball.get_position  ball_after_paddle)  (Ball.get_radius ball_after_paddle )) candidate_bricks 
    in

          (* Gérer les collisions avec chaque brique *)
    let ball_after_bricks = List.fold_left (fun current_ball brick ->
      (* Gérer la collision avec la brique *)
      let ball_after_brick_collision = Ball.handle_brick_collision current_ball (Brick.get_position brick) in
      ball_after_brick_collision
    ) ball_after_paddle colliding_bricks in

    let (destroyed_bricks, hit_bricks) = 
    List.partition (fun brick -> Brick.is_destroyed brick) (List.filter_map Brick.hit colliding_bricks)
    in

    let score_increment =( List.fold_left (fun acc brick -> acc +Brick.get_value brick ) 0 destroyed_bricks) in

    let new_bricks =others_brick @ candidate_bricks_without @ hit_bricks in

    let new_quadtree = 
      if List.length new_bricks != List.length state.bricks then
        Quadtree.build_quadtree new_bricks Config.screen_bounds
      else 
        state.quadtree
    in
      
    (* liste des effets bonus du jeu *)
    let bonus_items = 
      List.filter_map (fun brick ->
        match Brick.get_type brick with
        | Brick.Bonus effect -> 
            Some (Bonus.create (Brick.get_position brick) effect)
        | _ -> None
      ) destroyed_bricks
    in

    
  (* Mise à jour des bonus *)
  let updated_bonus_items = 
    List.filter (fun bonus -> 
      not (Bonus.is_expired bonus)
    ) (List.map (Bonus.update dt) (state.bonus_items @ bonus_items))
  in


  (*verifier collision effet bonus -raquette*)
  let (collected_bonuses, remaining_bonus_items) = 
  List.partition 
    (fun bonus -> Bonus.check_collision bonus state.paddle) 
    updated_bonus_items
  in


  let state_with_updated_effects = 
    { state with 
      active_effects = Gamestate.update_active_effects dt state 
    } 
  in

  (* Appliquer les effets bonus *)
  let new_paddle = 
    List.fold_left 
      (fun paddle bonus -> Bonus.apply_effect bonus paddle) 
      new_paddle
      collected_bonuses
  in


  (* let pre_state =
    List.fold_left (fun current_state (bonus:Bonus.t) ->
      let state_with_life =
        match (Bonus.get_effect bonus) with
        | Brick.ExtraLife | Brick.RestoreLife ->
            apply_bonus_life (Bonus.get_effect bonus) current_state
        | _ -> current_state
      in
      
      let state_with_score =
        match (Bonus.get_effect bonus) with
        | Brick.ScoreBonus _ ->
            apply_bonus_score (Bonus.get_effect bonus) state_with_life
        | _ -> state_with_life
      in
      
      let updated_effects =
        match Bonus.get_effect bonus with
        | Brick.StretchPaddle | Brick.ShrinkPaddle ->
            (* Remove existing stretch/shrink effects before adding *)
            let filtered_effects = 
              List.filter (fun (e, _) -> 
                e <> Brick.StretchPaddle && e <> Brick.ShrinkPaddle
              ) current_state.active_effects
            in
            (Bonus.get_effect bonus, Config.bonus_effect_duration) :: filtered_effects
        | _ -> current_state.active_effects
      in
      
      let state_with_ball_effect =
        { state_with_score with
          ball = Ball.apply_effect (Bonus.get_effect bonus) state_with_score.ball;
          active_effects = updated_effects
        }
      in
      
      state_with_ball_effect
    ) state_with_updated_effects collected_bonuses
  in *)
(* 
  let final_state = 
    let has_stretch_shrink_effect = 
      List.exists (fun (effect, duration) -> 
        (effect = Brick.StretchPaddle || effect = Brick.ShrinkPaddle) && duration > 0.
      ) pre_state.active_effects 
    in
    
    if not has_stretch_shrink_effect then
      { pre_state with 
        paddle = Paddle.create 
          Config.paddle_position 
          Config.paddle_width 
          Config.paddle_height 
          Config.paddle_velocity
      }
    else 
      pre_state

    in  *)

  (*etat avec bonus*)
  let pre_state = 
    List.fold_left (fun current_state (bonus:Bonus.t) ->
      let state_with_life = 
        match (Bonus.get_effect bonus) with
        | Brick.ExtraLife | Brick.RestoreLife -> 
            apply_bonus_life (Bonus.get_effect bonus) current_state
        | _ -> current_state
      in
      
      let state_with_score = 
        match (Bonus.get_effect bonus) with
        | Brick.ScoreBonus _ -> 
            apply_bonus_score (Bonus.get_effect bonus) state_with_life
        | _ -> state_with_life
      in
      
      let state_with_ball_effect = 
        { state_with_score with 
          ball = Ball.apply_effect (Bonus.get_effect bonus) state_with_score.ball 
        }
      in
      
      state_with_ball_effect
    ) state_with_updated_effects collected_bonuses
  in

    (* Vérification de la perte de la balle *)
    let new_state = 
      if Ball.is_lost Config.screen_bounds ball_after_bricks then
        if pre_state.lives <= 1 then
          { pre_state with status = Gamestate.GameOver }
        else
          { pre_state with 
            ball =  Ball.create 
            (fst (Paddle.get_position state.paddle) +. fst (Paddle.get_dimensions state.paddle) /. 2., 
            snd (Paddle.get_position state.paddle) +. snd (Paddle.get_dimensions state.paddle) +. 10.)
            ( -.Config.initial_ball_speed, Config.initial_ball_speed)
            Config.ball_radius 
            Config.ball_mass;
            paddle = Paddle.create Config.paddle_position Config.paddle_width Config.paddle_height Config.paddle_velocity;
            lives = state.lives - 1;
            score = state.score + score_increment;
            bonus_items = remaining_bonus_items;
            quadtree = new_quadtree;
            status = Playing
          }
      else
        { pre_state with 
          ball = ball_after_bricks;
          paddle = new_paddle;
          bricks = new_bricks;
          quadtree = new_quadtree;
          bonus_items = remaining_bonus_items;
          score = state.score + score_increment
        }
    in
    new_state

  (* Mise à jour de l'état du jeu *)
  let update_state dt mouse_state (state:Gamestate.t) =

 
  match state.bricks with
  | [] -> 
      (* Create new level state *)
      let next_level_state = {
        state with
        level = state.level + 1;
        ball = Ball.create 
          (fst (Paddle.get_position state.paddle) +. fst (Paddle.get_dimensions state.paddle) /. 2.,
           snd (Paddle.get_position state.paddle) +. snd (Paddle.get_dimensions state.paddle) +. 10.)
          (-.Config.initial_ball_speed *. (1.0 +. 0.1 *. float_of_int state.level),
           Config.initial_ball_speed *. (1.0 +. 0.1 *. float_of_int state.level))
          Config.ball_radius
          Config.ball_mass;
        bricks = Gamestate.create_brick_grid 
          Config.rows 
          Config.cols 
          Config.brick_width 
          Config.brick_height 
          Config.brick_spacing
          (state.level + 1);
        status = Gamestate.Playing;
        bonus_items = [];
      } in
      next_level_state

  | _ -> game_loop dt mouse_state state

     
  

  (* Création du flux d'états du jeu *)
  let create_game_stream dt () =
    Flux.unfold
      (fun (state:Gamestate.t) ->
        Some (state, update_state dt ( (fst (Graphics.mouse_pos ()) |> float_of_int) -. fst (Paddle.get_dimensions state.paddle) /. 2.0, false) state))
        initial_state

end
