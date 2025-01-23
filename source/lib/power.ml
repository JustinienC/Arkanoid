open Brick
open Type

module type POWER = 
sig
  type power
  val getType : power -> Brick.power_up
  val getSpeed : power -> float
  val get_position : power -> float * float
  val set_position : power -> (float * float) -> power
  val set_velocity: power -> float -> power
  val get_dimensions : power -> float * float
  val get_bounds : power -> (float * float) * (float * float)
  val check_paddle_collision : power -> etat_racket -> bool
  val create_power_from_brick : Brick.brick -> power option
  val is_brick_destroyed_at_power_position : power -> Brick.brick list -> bool
end

module Power : POWER = struct
  type power = {
    posit : float*float;
    dim1 : float;
    dim2 : float;
    velocity : float;
    typeP : Brick.power_up; 
  }
  let getType power = power.typeP

  let getSpeed power = power.velocity

  let set_position power pos = {power with posit = pos}
  let set_velocity power velo = {power with velocity = velo}

  let get_position power = power.posit

  let get_dimensions power = (power.dim1, power.dim2)

  let get_bounds power =
    let (x, y) = power.posit in
    ((x, y), (x +. power.dim1, y +. power.dim2))

  let check_paddle_collision power (paddle:etat_racket) = 
    let ((px1, py1), (px2, py2)) = get_bounds power in
    let (paddle_x, paddle_y) = paddle.pos in
    let (paddle_width, paddle_height) = paddle.dim in
    (* Distance la plus proche entre le powerup et la raquette *)
    let closest_x = max px1 (min paddle_x px2) in
    let closest_y = max py1 (min paddle_y py2) in
    
    (* Distance entre le point le plus proche et le centre de la raquette *)
    let dx = paddle_x -. closest_x in
    let dy = paddle_y -. closest_y in
    
    (* Si la distance est inférieure a l'espace de la raquette, il y a collision *)
    (dx *. dx) +. (dy *. dy) <= paddle_width *. paddle_height
  
  let create_power_from_brick brick =
    match Brick.get_power_up brick with
    | Some power_up ->
      let (x, y) = Brick.get_position brick in
      Some {posit = (x, y); dim1 = 20.; dim2 = 10.; velocity = 0.; typeP = power_up}
    | None -> None
  
  (* Fonction pour mettre à jour les power-ups *)
  (*let update_power (paddle:etat_racket) dt power =
    if check_paddle_collision power paddle then None
    else 
      let (x, y) = power.posit in
      let new_y = y +. power.velocity *. dt in
      Some { power with posit = (x, new_y) }  *)
  let is_brick_destroyed_at_power_position power bricks =
    let (power_x, power_y) = get_position power in
    List.exists
      (fun brick ->
        let (brick_x, brick_y) = Brick.get_position brick in
        brick_x = power_x && brick_y = power_y && Brick.is_destroyed brick)
      bricks

end

module PowerSet = struct
  type t = Power.power list

  (* Fonction pour créer une liste de power-ups à partir d'une liste de briques *)
  let create_from_bricks bricks =
    List.filter_map Power.create_power_from_brick bricks

  (* Fonction pour vérifier les collisions entre les power-ups et la raquette *)
  let check_paddle_collisions power_ups paddle =
    List.filter (fun power -> Power.check_paddle_collision power paddle) power_ups

  let update_powers paddle dt powers bricks =
    let update_power power = 
      if Power.check_paddle_collision power paddle then None
      else if Power.is_brick_destroyed_at_power_position power bricks then
         Some (Power.set_velocity power (-50.0))
      else
      let (x, y) = Power.get_position power in
      let new_y = y +. (Power.getSpeed power) *. dt in
      Some (Power.set_position power (x, new_y))
    in List.filter_map (update_power) powers
end