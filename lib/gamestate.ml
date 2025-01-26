(* lib/gamestate.ml *)

open Ball
open Brick
open Paddle
open Config
open Quadtree
open Bonus

module Gamestate = struct
  (* États possibles du jeu *)
  type game_status =
    | Playing
    | Paused
    | GameOver
    | LevelComplete
    | NextLevel
    | Starting

(*
   type t
   Représente l'état complet du jeu
   Champs :
   - ball : Ball.t, état de la balle
   - paddle : Paddle.t, état de la raquette
   - bricks : Brick.t list, liste des briques restantes
   - score : int, score actuel (>= 0)
   - lives : int, vies restantes (>= 0)
   - level : int, niveau actuel (>= 1)
   - status : game_status, état actuel du jeu
   - last_update : float, timestamp de la dernière mise à jour
   - quadtree : arbre spatial pour la détection des collisions
   - bonus_items : bonus actifs en jeu
   - active_effects : effets actifs sur la raquette
   - active_ball_effects : effets actifs sur la balle
*)
  type t = {
    ball: Ball.t;
    paddle: Paddle.t;
    bricks: Brick.t list;
    score: int;
    lives: int;
    level: int;
    status: game_status;
    last_update: float;  (* Pour le calcul du dt *)

    quadtree: Brick.t Quadtree.node; (* Arbre pour la détection des collisions *)
    bonus_items: Bonus.t list;
    active_effects: (Brick.bonus_effect * float) list;
    active_ball_effects: (Brick.bonus_effect * float) list; (*pour la ball*)
  }


(*
   type brick_layout
   Représente la disposition des briques
   Champs :
   - brick_count : int, nombre total de briques
   - cols : int, nombre de colonnes
   - rows : int, nombre de lignes
   - spacing : float * float, espacement horizontal et vertical
*)

  type brick_layout = {
  brick_count: int;
  cols: int;
  rows: int;
  spacing: float * float;
}


(*
   calculate_brick_layout : float -> float -> (float*float)*(float*float) -> float -> brick_layout
   calcule la disposition optimale des briques
   Paramètre brick_width : largeur d'une brique
   Paramètre brick_height : hauteur d'une brique
   Paramètre screen_bounds : limites de l'écran
   Paramètre spacing : espacement entre les briques
   Résultat : layout optimal pour l'écran
*)
let calculate_brick_layout brick_width brick_height screen_bounds spacing  : brick_layout =
  let horizontal_spacing=spacing in
  let vertical_spacing = spacing in 
  let ((x_min, y_min), (x_max, y_max)) = screen_bounds in
  let screen_width = x_max -. x_min in
  let screen_height = y_max -. y_min in
  (* Réserver la moitié supérieure de l'écran *)
  let available_height = screen_height /. 2. in
  
  (* Calculer le nombre de colonnes en incluant l'espacement horizontal *)
  let cols = int_of_float ((screen_width +. horizontal_spacing) /. (brick_width +. horizontal_spacing)) in
  (* Calculer le nombre de lignes en incluant l'espacement vertical *)
  let rows = int_of_float ((available_height +. vertical_spacing) /. (brick_height +. vertical_spacing)) in
  
  {
    brick_count = cols * rows;
    cols;
    rows;
    spacing = (horizontal_spacing, vertical_spacing)
  }
  

 
(*
   create_brick_grid : int -> int -> float -> float -> float -> int -> Brick.t list
   crée une grille de briques
   Paramètre rows : nombre de rangées
   Paramètre cols : nombre de colonnes
   Paramètre brick_width : largeur des briques
   Paramètre brick_height : hauteur des briques
   Paramètre spacing : espacement entre les briques
   Paramètre level : niveau actuel (pour le score)
   Résultat : liste des briques créées
   Postcondition : les briques sont disposées en grille avec espacement régulier
*)
let create_brick_grid rows cols brick_width brick_height spacing level =
  let ((_, _), (_, y_max)) = Config.screen_bounds in
  let create_row y row_idx =
    let rec create_cols x col_idx acc =
      if col_idx >= cols then acc
      else
        let brick_type = 
          let r = Random.float 1.0 in
          if r < 0.1 then 
            Brick.Bonus (
              match Random.int 1 with
              (* | 0-> Brick.StretchPaddle
              | 1 -> Brick.ShrinkPaddle *)
              | 2 -> Brick.SpeedUpBall 
              | 3 -> Brick.SlowDownBall
              | 4 -> Brick.ExtraLife
              | 5 -> Brick.RestoreLife
              (* | 6 -> Brick.MultiplyBall *)
              | _ -> Brick.ScoreBonus (Random.int 100)
            )
          (* else if r < 0.2 then
            Explosive
          else if r < 0.3 then
            Indestructible *)
          else if r < 0.5 then
            Reinforced (Random.int 2 + 2)
          else
            Classic
        in
        let new_brick = Brick.create 
          (x, y_max -. y +. 0.) 
          brick_width 
          brick_height
          (match brick_type with
          | Brick.Reinforced n -> n
          (* | Brick.Indestructible -> max_int *)
          | _ -> 1)
          (Config.base_bonus * level)
          brick_type
        in
        create_cols (x +. brick_width +. spacing) (col_idx + 1) (new_brick :: acc)
    in
    create_cols spacing 0 []
  in
  
  (* Fonction interne pour créer plusieurs rangées de briques *)
  let rec create_rows y row_idx acc =
    if row_idx >= rows then acc
    else
      let row_bricks = create_row y row_idx in
      create_rows (y +. brick_height +. spacing) (row_idx + 1) (row_bricks @ acc)
  in
  create_rows spacing 0 []

(*
   create : unit -> t
   crée un nouvel état de jeu initial
   Résultat : état initialisé avec les valeurs par défaut de Config
   Postcondition : 
   - score = initial_score
   - lives = initial_lives
   - level = initial_level
   - status = Starting
*)
  let create () =
    let layout = calculate_brick_layout Config.brick_width Config.brick_height Config.screen_bounds Config.brick_spacing in

    (* let bricks =create_brick_grid Config.rows Config.cols Config.brick_width Config.brick_height Config.brick_spacing in *)
    let bricks =create_brick_grid layout.rows layout.cols Config.brick_width Config.brick_height Config.brick_spacing Config.initial_level in

    let quadtree = Quadtree.build_quadtree bricks Config.screen_bounds in
    {
      ball = Ball.create 
      (fst Config.paddle_position +. Config.paddle_width /. 2., 
      snd Config.paddle_position +. Config.paddle_height  +. 10.)
        (-.Config.initial_ball_speed, Config.initial_ball_speed)
        Config.ball_radius
        Config.ball_mass;
      paddle = Paddle.create Config.paddle_position Config.paddle_width  Config.paddle_height Config.paddle_velocity;
      bricks;
      score = Config.initial_score;
      lives = Config.initial_lives;
      level = Config.initial_level;
      status = Starting;
      last_update = Unix.gettimeofday ();

      quadtree;
      bonus_items = [];
      active_effects = [];
      active_ball_effects = [];  (* Initialisation du nouveau champ *)
    }

end



