open Brick
open Type

module type POWER = 
sig
  type power
  val get_type : power -> Brick.power_up
  val get_speed : power -> float
  val get_move : power -> bool
  val get_position : power -> float * float
  val set_position : power -> (float * float) -> power
  val startMove: power -> float -> power
  val get_dimensions : power -> float * float
  val get_bounds : power -> (float * float) * (float * float)
  val check_paddle_collision : power -> etat_racket -> bool
  val create_power_from_brick : Brick.brick -> power option
  val is_no_brick_at_power_position : power -> Brick.brick list -> bool
end

module Power : POWER = struct
  type power = {
    posit : float*float;
    dim1 : float;
    dim2 : float;
    velocity : float;
    moving : bool;
    typeP : Brick.power_up; 
  }
  let get_type power = power.typeP
  let get_move power = power.moving

  let get_speed power = power.velocity

  let set_position power pos = {power with posit = pos}
  let startMove power velo = {power with velocity = velo; moving = true}

  let get_position power = power.posit

  let get_dimensions power = (power.dim1, power.dim2)

  let get_bounds power =
    let (x, y) = power.posit in
    ((x, y), (x +. power.dim1, y +. power.dim2))

  let check_paddle_collision power (paddle:etat_racket) = 
    let ((px1, py1), (px2, py2)) = get_bounds power in
    let (paddle_x, paddle_y) = paddle.pos in
    let (paddle_width, paddle_height) = paddle.dim in
    
    (* Vérifier si la position du power-up est à l'intérieur de la zone de la raquette *)
    let horizontal_overlap = px2 > paddle_x && px1 < paddle_x +. paddle_width in
    let vertical_overlap = py2 > paddle_y && py1 < paddle_y +. paddle_height in
    
    (* Si le power-up se trouve dans la zone de la raquette, il y a collision *)
    horizontal_overlap && vertical_overlap
    
  
  let create_power_from_brick brick =
    match Brick.get_power_up brick with
    | Some power_up ->
        let (x, y) = Brick.get_position brick in
        Some { posit = (x, y); dim1 = 20.; dim2 = 10.; velocity = 0.; moving = false; typeP = power_up }
    | None -> None
  
  let is_no_brick_at_power_position power bricks =
    let (power_x, power_y) = get_position power in
    let epsilon = 1e-5 in
    not (List.exists
      (fun brick ->
        let ((bx1, by1), (bx2, by2)) = Brick.get_bounds brick in
        let inside_x = power_x >= bx1 -. epsilon && power_x <= bx2 +. epsilon in
        let inside_y = power_y >= by1 -. epsilon && power_y <= by2 +. epsilon in
        inside_x && inside_y)
      bricks)

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
      else if Power.is_no_brick_at_power_position power bricks && Power.get_move power = false then
        Some (Power.startMove power (-50.0))
      else
      let (x, y) = Power.get_position power in
      let new_y = y +. (Power.get_speed power) *. dt in
      Some (Power.set_position power (x, new_y))
    in List.filter_map (update_power) powers

  let get_colliding_powers powers paddle =
    List.filter (fun power -> Power.check_paddle_collision power paddle) powers
end