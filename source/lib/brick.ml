(**manipuler les briques dans le jeu **)
open Type

module type BRICK = sig


type brick
  type brick_type = 
  | Normal                      (* Une brique standard qui peut être détruite *)
  | Indestructible              (* Une brique qui ne peut pas être détruite *)
  | PowerUp of power_up         (* Une brique qui donne un bonus *)

  and power_up = 
    | EnlargePaddle
    | SpeedUp
    | MultiplyBall

  (* Signatures des fonctions *)
  val create_brick : float * float -> float -> float -> int -> int -> brick_type -> brick
  val is_destroyed : brick -> bool
  val hit : brick -> brick
  val get_value : brick -> int
  val get_position : brick -> float * float 
  val get_dimensions : brick -> float * float
  val get_bounds : brick -> (float * float) * (float * float)
  val check_collision : brick -> etat_balle -> bool
  val get_power_up : brick -> power_up option
end


module Brick : BRICK = struct

  (* Définition des types *)
  type brick = {
    pos: float * float;        (* Position (x, y) de la brique *)
    width: float;              (* Largeur de la brique *)
    height: float;             (* Hauteur de la brique *)
    health: int;               (* Points de vie de la brique *)
    value: int;                (* Points gagnés en détruisant cette brique *)
    brick_type: brick_type;    (* Type spécifique de la brique *)
  }

  and brick_type = 
    | Normal
    | Indestructible
    | PowerUp of power_up

  and power_up = 
    | EnlargePaddle
    | SpeedUp
    | MultiplyBall

  (* Implémentation des fonctions *)
  let create_brick pos width height health value brick_type = 
    { pos; width; height; health; value; brick_type }

  let is_destroyed brick = 
    match brick.brick_type with
    | Indestructible -> false
    | _ -> brick.health <= 0

  let hit brick = 
    match brick.brick_type with
    | Indestructible -> brick
    | _ -> { brick with health = brick.health - 1 }

  let get_value brick = brick.value

  (* let get_value brick = 
    if is_destroyed brick then brick.value else 0 *)
  let get_position brick = brick.pos

  let get_dimensions brick = (brick.width, brick.height)
  
  let get_bounds brick =
      let (x, y) = brick.pos in
      ((x, y), (x +. brick.width, y +. brick.height))
  
  let check_collision brick (ball:etat_balle) =
      let ((bx1, by1), (bx2, by2)) = get_bounds brick in
      let (ball_x, ball_y) = ball.pos in
      let r = ball.radius in
      
      (* Distance la plus proche entre le centre de la balle et la brique *)
      let closest_x = max bx1 (min ball_x bx2) in
      let closest_y = max by1 (min ball_y by2) in
      
      (* Distance entre le point le plus proche et le centre de la balle *)
      let dx = ball_x -. closest_x in
      let dy = ball_y -. closest_y in
      
      (* Si la distance est inférieure au rayon, il y a collision *)
      (dx *. dx) +. (dy *. dy) <= r *. r
  
  let get_power_up brick =
      match brick.brick_type with
      | PowerUp p -> Some p
      | _ -> None
    
end


(* Module pour gérer un ensemble de briques *)
module BrickSet = struct
  type t = Brick.brick list

  let create_grid rows cols brick_width brick_height spacing =
    let create_row y row_idx =
      let rec create_cols x col_idx acc =
        if col_idx >= cols then acc
        else
          let brick_type = 
            if Random.float 1.0 < 0.1 then 
              Brick.PowerUp (
                match Random.int 3 with
                | 0 -> Brick.EnlargePaddle
                | 1 -> Brick.SpeedUp
                | _ -> Brick.MultiplyBall
              )
            else Brick.Normal
          in
          let new_brick = Brick.create_brick 
            (x, y +. 400.)  (* Ajout de 400 pour monter les briques *)
            brick_width 
            brick_height
            1  
            (10 * (rows - row_idx))
            brick_type
          in
          create_cols (x +. brick_width +. spacing) (col_idx + 1) (new_brick :: acc)
      in
      create_cols spacing 0 []
    (* let create_row y row_idx =
      let rec create_cols x col_idx acc =
        if col_idx >= cols then acc
        else
          let brick_type = 
            if Random.float 1.0 < 0.1 then 
              Brick.PowerUp (
                match Random.int 3 with
                | 0 -> Brick.EnlargePaddle
                | 1 -> Brick.SpeedUp
                | _ -> Brick.MultiplyBall
              )
            else Brick.Normal
          in
          let new_brick = Brick.create_brick 
            (x, y)
            brick_width 
            brick_height
            1  (* health *)
            (10 * (rows - row_idx))  (* value *)
            brick_type
          in
          create_cols (x +. brick_width +. spacing) (col_idx + 1) (new_brick :: acc)
      in *)
      (* create_cols spacing 0 [] *)
    in
    
    let rec create_rows y row_idx acc =
      if row_idx >= rows then acc
      else
        let row_bricks = create_row y row_idx in
        create_rows (y +. brick_height +. spacing) (row_idx + 1) (row_bricks @ acc)
    in
    
    create_rows spacing 0 []

  let update_bricks bricks ball =
    let update_brick brick =
      if Brick.check_collision brick ball then
        match Brick.hit brick with
        | new_brick when Brick.is_destroyed new_brick -> None
        | new_brick -> Some new_brick
      else Some brick
    in
    List.filter_map update_brick bricks

  let get_colliding_bricks bricks ball =
    List.filter (fun brick -> Brick.check_collision brick ball) bricks
end







