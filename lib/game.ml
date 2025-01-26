open Iterator
open Config
open Ball
open Paddle
open Brick
open Gamestate
open Quadtree
open Bonus

module GameEngine = struct

  let initial_state = Gamestate.create ()


    (* Gestion des effets de vie et score *)
    let apply_special_effects (state:Gamestate.t) collected_bonuses =
      let apply_single_effect (state:Gamestate.t)  effect =
        match effect with
        | Brick.ExtraLife -> 
            { state with lives = (state.lives + 1) }
        | Brick.RestoreLife -> 
            { state with lives = Config.initial_lives }
        | Brick.ScoreBonus score_bonus -> 
            { state with score = state.score + score_bonus }
        | _ -> state
      in
      
      (* Récupère tous les effets des bonus collectés *)
      let effects = List.map Bonus.get_effect collected_bonuses in
      
      (* Applique chaque effet séquentiellement *)
      List.fold_left apply_single_effect state effects
  (* Mise à jour de la position de la balle *)
  let update_ball_position dt ball =
    let (x, y) = Ball.get_position ball in
    let (vx, vy) = Ball.get_velocity ball in
    let new_ball_pos = (x +. vx *. dt, y +. vy *. dt) in
    Ball.set_position ball new_ball_pos

  (* Gestion des collisions de la balle *)
  let handle_ball_collisions ball paddle bounds =
    let ball_after_walls = Ball.handle_wall_collision ball bounds in
    Ball.handle_paddle_collision 
      ball_after_walls 
      (Paddle.get_position paddle) 
      (Paddle.get_dimensions paddle) 
      bounds

  (* Gestion des collisions avec les briques *)
  let handle_brick_collisions ball quadtree =
    let (candidate_bricks, others_brick) = Quadtree.query quadtree ball in
    let (colliding_bricks, candidate_bricks_without) = 
      List.partition 
        (fun brick -> Brick.check_collision brick 
          (Ball.get_position ball) 
          (Ball.get_radius ball)) 
        candidate_bricks in
    (colliding_bricks, candidate_bricks_without, others_brick)

  (* Mise à jour de l'état des briques *)
  let update_bricks_state ball colliding_bricks =
    let ball_after_bricks = 
      List.fold_left 
        (fun current_ball brick ->
          Ball.handle_brick_collision current_ball (Brick.get_position brick))
        ball 
        colliding_bricks in
    
    let (destroyed_bricks, hit_bricks) = 
      List.partition Brick.is_destroyed (List.filter_map Brick.hit colliding_bricks) in
    
    let score_increment = 
      List.fold_left (fun acc brick -> acc + Brick.get_value brick) 0 destroyed_bricks in
    
    (ball_after_bricks, destroyed_bricks, hit_bricks, score_increment)

  (* Gestion des bonus *)
  let handle_bonuses dt (state:Gamestate.t) destroyed_bricks new_paddle =
    let bonus_items = 
      List.filter_map 
        (fun brick ->
          match Brick.get_type brick with
          | Brick.Bonus effect -> Some (Bonus.create (Brick.get_position brick) effect)
          | _ -> None)
        destroyed_bricks in
    
    let updated_bonus_items = 
      List.filter_map 
        (fun bonus ->
          let updated = Bonus.update dt bonus in
          if Bonus.is_fallen updated then None else Some updated)
        (state.bonus_items @ bonus_items) in
    
    let (collected_bonuses, remaining_bonus_items) = 
      List.partition 
        (fun bonus -> Bonus.check_collision bonus new_paddle) 
        updated_bonus_items in
    
    (collected_bonuses, remaining_bonus_items)

  (* Gestion des effets actifs *)
  let handle_active_effects dt (state:Gamestate.t) new_paddle collected_bonuses =
    let new_effects = List.map Bonus.get_effect collected_bonuses in
    
    let (expired_effects, active_effects) = 
      List.partition (fun (effect, time) -> time <= 0.0) state.active_effects in
    
    let updated_active_effects = 
      List.map (fun (effect, time) -> (effect, time -. dt)) active_effects in
    
    let all_active_effects = 
      (List.map (fun effect -> (effect, Config.bonus_lifetime)) new_effects) 
      @ updated_active_effects in
    
    let paddle_without_expired = 
      List.fold_left 
        (fun paddle (effect, _) -> Paddle.remove_effect effect paddle)
        new_paddle 
        expired_effects in
    
    let new_paddle_with_effects = 
      List.fold_left 
        (fun paddle (effect, _) -> Bonus.apply_effect effect paddle)
        paddle_without_expired 
        all_active_effects in
    
    (new_paddle_with_effects, all_active_effects)

  (* Création d'un nouvel état après perte de balle *)
  let create_lost_ball_state (state:Gamestate.t) score_increment new_quadtree =
    { state with 
      ball = Ball.create 
        (fst (Paddle.get_position state.paddle) +. fst (Paddle.get_dimensions state.paddle) /. 2., 
         snd (Paddle.get_position state.paddle) +. snd (Paddle.get_dimensions state.paddle) +. 10.)
        (-.Config.initial_ball_speed, Config.initial_ball_speed)
        Config.ball_radius 
        Config.ball_mass;
      paddle = Paddle.create Config.paddle_position Config.paddle_width Config.paddle_height Config.paddle_velocity;
      lives = state.lives - 1;
      score = state.score + score_increment;
      bonus_items = [];
      active_effects = [];
      quadtree = new_quadtree;
      status = Playing
    }





  (* Nouvelle fonction pour gérer les effets de la balle *)
  let handle_ball_effects dt (state:Gamestate.t) collected_bonuses ball =
    (* Sépare les effets de la balle des autres effets *)
    let is_ball_effect = function
      | Brick.SpeedUpBall | Brick.SlowDownBall -> true
      | _ -> false
    in
    
    (* Récupère tous les effets des bonus collectés *)
    let effects = List.map Bonus.get_effect collected_bonuses in
    
    (* Sépare les effets de la balle des autres *)
    let (ball_effects, other_effects) = 
      List.partition is_ball_effect effects in
    
    (* Met à jour les effets actifs de la balle *)
    let (expired_ball_effects, active_ball_effects) = 
      List.partition 
        (fun (effect, time) -> time <= 0.0)
        state.active_ball_effects in
    
    (* Met à jour le temps des effets actifs *)
    let updated_ball_effects = 
      List.map 
        (fun (effect, time) -> (effect, time -. dt))
        active_ball_effects in
    
    (* Ajoute les nouveaux effets avec leur durée *)
    let all_ball_effects = 
      (List.map (fun effect -> (effect, Config.bonus_lifetime)) ball_effects)
      @ updated_ball_effects in
    
    (* Applique les effets sur la balle *)
    let new_ball = 
      List.fold_left 
        (fun ball (effect, _) -> Ball.apply_effect effect ball)
        ball
        all_ball_effects in
    
    (new_ball, all_ball_effects, other_effects)


    let update_bricks bricks ball =
      let update_brick brick =
        if Brick.check_collision brick (Ball.get_position ball) (Ball.get_radius ball) then
          match Brick.hit brick with
          | Some new_brick when not (Brick.is_destroyed new_brick) -> Some new_brick
          | _ -> None
        else Some brick
      in
      List.filter_map update_brick bricks
    
  (* Boucle principale du jeu *)
  let game_loop dt mouse_state (state:Gamestate.t) =
    let (mouse_x, _) = mouse_state in
    let new_paddle = Paddle.move Config.paddle_magin Config.screen_bounds mouse_x state.paddle in
    
    (* Mise à jour de la balle et collisions *)
    let new_ball = update_ball_position dt state.ball in
    let ball_after_paddle = handle_ball_collisions new_ball new_paddle Config.screen_bounds in
    
    (* Gestion des briques *)
    let (colliding_bricks, candidate_bricks_without, others_brick) = 
      handle_brick_collisions ball_after_paddle state.quadtree in
    let (ball_after_bricks, destroyed_bricks, hit_bricks, score_increment) = 
      update_bricks_state ball_after_paddle colliding_bricks in
    
      
      let new_bricks = update_bricks state.bricks ball_after_paddle in
    (* let new_bricks = others_brick @ candidate_bricks_without @ hit_bricks in *)
    let new_quadtree = 
      if List.length new_bricks != List.length state.bricks then
        Quadtree.build_quadtree new_bricks Config.screen_bounds
      else state.quadtree in
    
    (* Gestion des bonus et effets *)
    let (collected_bonuses, remaining_bonus_items) = 
      handle_bonuses dt state destroyed_bricks new_paddle in



        (* Application des effets sur la balle *)
    let (ball_with_effects, new_ball_effects, remaining_effects) = 
        handle_ball_effects dt state collected_bonuses ball_after_bricks in

    (* Application des effets spéciaux (vies et score) *)
    let state_with_special_effects = 
        apply_special_effects state collected_bonuses in

    (* Gestion des effets de la raquette *)
    let (new_paddle_with_effects, all_active_effects) = 
      handle_active_effects dt state_with_special_effects new_paddle collected_bonuses in
    
    (* Mise à jour de l'état du jeu *)
    if Ball.is_lost Config.screen_bounds ball_after_bricks then
      if state_with_special_effects.lives <= 1 then
        { state with status = Gamestate.GameOver }
      else
        create_lost_ball_state state_with_special_effects score_increment new_quadtree
    else
      { state_with_special_effects with 
        ball = ball_with_effects;
        paddle = new_paddle_with_effects;
        
        bricks = new_bricks;
        quadtree = new_quadtree;
        bonus_items = remaining_bonus_items;
        active_effects = all_active_effects;
        score = state_with_special_effects.score  + score_increment
      }


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
