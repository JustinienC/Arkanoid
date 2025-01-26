open Config
open Brick
open Paddle

module type BONUS= sig
  type t

  (*
   create : float * float -> Brick.bonus_effect -> t
   crée un nouveau bonus
   Paramètre (x,y) : position initiale du bonus
   Paramètre effect : effet du bonus
   Résultat : nouveau bonus avec vitesse et durée de vie par défaut
*)
  val create : float * float -> Brick.bonus_effect -> t

  (*
   is_expired : t -> bool
   vérifie si le bonus a expiré (durée de vie écoulée ou sorti de l'écran)
   Paramètre bonus : t, bonus à tester
   Résultat : true si durée de vie <= 0 ou position.y <= paddle_position
*)
  val is_expired : t -> bool


(*
   get_position : t -> float * float
   récupère la position actuelle du bonus
   Paramètre bonus : t, bonus dont on veut la position  
   Résultat : (x,y), coordonnées du coin supérieur gauche
*)
  val get_position : t -> float * float

  (*  
   get_dimensions : t -> float * float
   récupère les dimensions du bonus
   Paramètre bonus : t, bonus dont on veut les dimensions
   Résultat : (width,height), dimensions fixes définies dans Config
*)

  val get_dimensions : t -> float * float

  (*
   get_bounds : t -> (float * float) * (float * float)
   calcule les limites du rectangle englobant du bonus
   Paramètre bonus : t, bonus dont on veut les limites
   Résultat : ((x1,y1), (x2,y2)), coins supérieur gauche et inférieur droit
*)
  val get_bounds : t -> (float * float) * (float * float)

  (*
   update : float -> t -> t 
   met à jour l'état du bonus
   Paramètre dt : float, temps écoulé depuis la dernière mise à jour
   Paramètre bonus : t, bonus à mettre à jour
   Résultat : bonus mis à jour (position, durée de vie)
*)
  val update : float -> t -> t

  (*
   is_expired : t -> bool 
   vérifie si le bonus a expiré (durée de vie écoulée ou sorti de l'écran)
   Paramètre bonus : t, bonus à tester
   Résultat : true si expiré
*)

  (*  
   apply_effect : Brick.bonus_effect -> Paddle.t -> Paddle.t
   applique l'effet du bonus sur la raquette
   Paramètre effect : effet à appliquer
   Paramètre paddle : raquette à modifier
   Résultat : raquette avec l'effet appliqué
*)
  val apply_effect: Brick.bonus_effect -> Paddle.t -> Paddle.t
  
(*
   check_collision : t -> Paddle.t -> bool
   vérifie si le bonus est en collision avec la raquette 
   Paramètre bonus : t, bonus à tester
   Paramètre paddle : Paddle.t, raquette à tester
   Résultat : true si collision détectée
*)
  val check_collision : t -> Paddle.t -> bool
  val get_effect : t -> Brick.bonus_effect

  (*
   is_effect_active : t -> bool
   vérifie si l'effet du bonus est actif
   Paramètre bonus : t, le bonus à tester
   Résultat : true si l'effet est actif, false sinon
*)
  val is_effect_active: t -> bool


(*
   is_fallen : t -> bool 
   vérifie si le bonus est tombé hors de l'écran
   Paramètre bonus : t, le bonus à tester
   Résultat : true si le bonus est tombé sous la raquette
*)
  val is_fallen : t -> bool


(*
   set_is_active : bool -> t -> t
   modifie l'état d'activation du bonus
   Paramètre is_active : bool, nouvel état d'activation
   Paramètre bonus : t, bonus à modifier
   Résultat : bonus avec son état d'activation mis à jour
*)
  val set_is_active : bool -> t -> t
end

module Bonus : BONUS = struct

  (* 
   type t
   Représente un bonus dans le jeu
   Champs :
   - position : float * float, coordonnées (x,y) du bonus
   - velocity : float, vitesse de chute (> 0)
   - effect : Brick.bonus_effect, effet du bonus
   - lifetime : float, durée de vie restante (>= 0)
   - is_active : bool, indique si l'effet est actif
*)
  type t = {
    position: float * float;
    velocity: float;
    effect: Brick.bonus_effect;
    lifetime: float; 
    is_active :bool;
  }

  let create (x, y) effect = {
    position = (x, y);
    velocity = Config.bonus_velocity;  
    effect = effect;
    lifetime = Config.bonus_lifetime;  
    is_active = false;
  
  }

let update dt bonus = 
  let new_lifetime =
    if bonus.is_active then 
      max 0. (bonus.lifetime -. dt)
    else 
      bonus.lifetime
  in
  {
    bonus with
    position = (fst bonus.position, snd bonus.position -. bonus.velocity *. dt);
    lifetime = new_lifetime;
    is_active = new_lifetime > 0.;
  }
  let get_effect bonus = bonus.effect
  (* let is_expired bonus = bonus.lifetime <= 0. *)

  let is_expired bonus = 
    let (_, y) = bonus.position in
    bonus.lifetime <= 0. || y <= snd Config.paddle_position 
  let get_position bonus = bonus.position

  let is_fallen bonus = 
    let (_, y) = bonus.position in
    y <= snd Config.paddle_position (*comparaison avec l ordonnée de la raquette*)
  
  let is_effect_active bonus = bonus.is_active
  let get_dimensions _ = Config.bonus_dimensions
  let get_bounds bonus =
    let (x, y) = bonus.position in
    let (w, h) = get_dimensions bonus in
    ((x, y), (x +. w, y +. h))

  let apply_effect effect paddle = 
    match effect with
    | Brick.StretchPaddle -> Paddle.apply_effect Brick.StretchPaddle paddle
    | Brick.ShrinkPaddle -> Paddle.apply_effect Brick.ShrinkPaddle paddle
    |_-> paddle

  let check_collision bonus paddle =
    let ((bonus_x1, bonus_y1), (bonus_x2, bonus_y2)) = get_bounds bonus in
    let ((paddle_x1, paddle_y1), (paddle_x2, paddle_y2)) = Paddle.get_bounds paddle in
    
    bonus_x1 < paddle_x2 && 
    bonus_x2 > paddle_x1 && 
    bonus_y1 < paddle_y2 && 
    bonus_y2 > paddle_y1

  let  set_is_active is_active bonus= {bonus with is_active}
end