open Brick
open Type

module type POWER = 
sig
  type power
  (* get_type: Obtient le type du power-up d'un power
   Paramètres:
     - power : un power dont on veut obtenir le type (Brick.power_up)
   Retourne:
     - Le type du power-up (Brick.power_up)*)
  val get_type : power -> Brick.power_up

  (* get_speed: Obtient la vitesse du power
   Paramètres:
     - power : un power dont on veut obtenir la vitesse
   Retourne:
     - La vitesse du power en tant que flottant*)
  val get_speed : power -> float

  (* get_move: Vérifie si le power est en mouvement
   Paramètres:
     - power : un power à vérifier
   Retourne:
     - true si le power est en mouvement, false sinon*)
  val get_move : power -> bool

  (* get_position: Obtient la position actuelle du power
   Paramètres:
     - power : le power dont on veut obtenir la position
   Retourne:
     - La position du power sous forme de tuple (float * float)*)
  val get_position : power -> float * float

  (* set_position: Modifie la position d'un power.
   Paramètres:
     - power : le power à modifier
     - pos : la nouvelle position sous forme de tuple (float * float)
   Retourne:
     - Un nouveau power avec la position mise à jour*)
  val set_position : power -> (float * float) -> power

  (* startMove: Lance le mouvement d'un power avec une certaine vitesse
   Paramètres:
     - power : le power à modifier
     - velo : la vitesse du mouvement (flottant)
   Retourne:
     - Un nouveau power avec la vitesse et le mouvement activés*)
  val startMove: power -> float -> power

  (*get_dimensions: Obtient les dimensions (largeur, hauteur) du power
   Paramètres:
     - power : le power dont on veut obtenir les dimensions
   Retourne:
     - Un tuple (float * float) représentant les dimensions du power*)
  val get_dimensions : power -> float * float

  (* get_bounds: Obtient les bords du power, représentés par deux coins opposés
   Paramètres:
     - power : le power dont on veut obtenir les bords
   Retourne:
     - Un tuple de tuples représentant les coins (x1, y1) et (x2, y2)
  *)
  val get_bounds : power -> (float * float) * (float * float)

  (* check_paddle_collision: Vérifie si un power entre en collision avec une raquette en utilisant la méthode AABB
   Paramètres:
     - power : le power à vérifier.
     - paddle : la raquette à vérifier pour la collision
   Retourne:
     - true si le power entre en collision avec la raquette, false sinon*)
  val check_paddle_collision : power -> etat_racket -> bool

  (* create_power_from_brick: Crée un power à partir d'une brique(en prenant les position de la brique)
   Paramètres:
     - brick : la brique à partir de laquelle on veut créer un power
   Retourne:
     - Some power si la brique contient un power-up, None sinon*)
  val create_power_from_brick : Brick.brick -> power option

  (* is_no_brick_at_power_position: Vérifie s'il n'y a pas de brique à la position du power ie la brique
    ou se trouve le power a été détruite par la balle
   Paramètres:
     - power : le power à vérifier
     - bricks : la liste des briques du jeu
   Retourne:
     - true s'il n'y a pas de brique à la position du power, false sinon*)
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
        Some { posit = (x, y); dim1 = 20.; dim2 = 10.; velocity = 0.; moving = false; typeP = power_up } (*Crée un power utilisant les position x, y de la brique*)
    | None -> None
  
  let is_no_brick_at_power_position power bricks =
    let (power_x, power_y) = get_position power in
    let epsilon = 1e-5 in
    not (List.exists
      (fun brick ->  (*Fonction qui regarde si une brique et un power sont en collision(on l'applique à toutes les briques avec List.exists)*)
        let ((bx1, by1), (bx2, by2)) = Brick.get_bounds brick in
        let inside_x = power_x >= bx1 -. epsilon && power_x <= bx2 +. epsilon in
        let inside_y = power_y >= by1 -. epsilon && power_y <= by2 +. epsilon in
        inside_x && inside_y)
      bricks)

end

module PowerSet = struct
  type t = Power.power list

  (* create_from_bricks: Crée une liste de power-ups à partir d'une liste de briques
   Paramètres:
     - bricks : une liste de briques à partir desquelles on veut extraire les power-ups
   Retourne:
     - Une liste de power-ups créés à partir des briques*)
  let create_from_bricks bricks =
    List.filter_map Power.create_power_from_brick bricks

  (* check_paddle_collisions: Vérifie les collisions entre une liste de power-ups et la raquette
   Paramètres:
     - power_ups : une liste de power-ups à vérifier
     - paddle : la raquette à vérifier pour les collisions
   Retourne:
     - Une liste de power-ups qui sont en collision avec la raquette*)
  let check_paddle_collisions power_ups paddle =
    List.filter (fun power -> Power.check_paddle_collision power paddle) power_ups

  (* update_powers: Met à jour les positions des power-ups en fonction du temps et de l'état du jeu.
   Paramètres:
     - paddle : la raquette.
     - dt : le delta de temps (temps écoulé entre les frames)
     - powers : la liste des power-ups à mettre à jour
     - bricks : les briques du jeu
   Retourne:
     - Une nouvelle liste de power-ups mise à jour*)
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

  (* get_colliding_powers: Obtient une liste de power-ups en collision avec la raquette
   Paramètres:
     - powers : une liste de power-ups
     - paddle : la raquette à vérifier
   Retourne:
     - Une liste de power-ups en collision avec la raquette*)
  let get_colliding_powers powers paddle =
    List.filter (fun power -> Power.check_paddle_collision power paddle) powers
end