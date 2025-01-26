open Brick
open Config

module type PADDLE = sig
  type t
  
  (*
   create : float*float -> float -> float -> float -> t
   crée une nouvelle instance de raquette
   Paramètre (x,y) : float*float, position initiale de la raquette 
   Paramètre width : float, largeur de la raquette (> 0)
   Paramètre height : float, hauteur de la raquette (> 0)
   Paramètre speed : float, vitesse de déplacement de la raquette (> 0)
   Résultat : une nouvelle instance de raquette t
*)
  val create : float * float -> float -> float -> float -> t


  (*
   move : float -> (float*float)*(float*float) -> float -> t -> t
   déplace la raquette horizontalement en suivant la position de la souris
   Paramètre margin : float, marge par rapport aux bords (> 0)
   Paramètre bounds : (float*float)*(float*float), limites du terrain
   Paramètre mouse_x : float, position horizontale de la souris
   Paramètre paddle : t, la raquette à déplacer
   Résultat : la raquette avec sa position mise à jour
*)

  val move : float->((float * float) * (float * float)) -> float -> t -> t

  (*
   apply_effect : Brick.bonus_effect -> t -> t
   applique un effet spécial à la raquette
   Paramètre effect : Brick.bonus_effect, l'effet à appliquer, peut être:
     - StretchPaddle : agrandit la raquette (facteur Config.paddle_stretch_factor)
     - ShrinkPaddle : rétrécit la raquette (facteur Config.paddle_shrink_factor)
   Paramètre paddle : t, la raquette à modifier
   Résultat : la raquette avec l'effet appliqué et ses propriétés mises à jour
   Précondition : la largeur finale doit rester entre paddle_width/4.0 et screen_width/2.0
   Postcondition : l'effet est actif (effect_state.effect_active = true) et 
                  le facteur d'étirement est mis à jour selon l'effet
*)
  val apply_effect : Brick.bonus_effect -> t -> t

  (*
   get_position : t -> float * float  
   récupère la position actuelle de la raquette
   Paramètre paddle : t, la raquette dont on veut la position
   Résultat : float * float, coordonnées (x,y) du centre de la raquette
*)
  val get_position : t -> float*float

  (*
   get_width : t -> float
   récupère la largeur actuelle de la raquette
   Paramètre paddle : t, la raquette dont on veut la largeur
   Résultat : float, la largeur courante de la raquette
*)
  val get_width : t -> float

  (*
   get_dimensions : t -> float * float
   récupère les dimensions actuelles de la raquette
   Paramètre paddle : t, la raquette dont on veut les dimensions
   Résultat : (width,height), la largeur et la hauteur de la raquette
*)
  val get_dimensions : t -> float*float

  (*
   get_bounds : t -> (float * float) * (float * float)
   calcule les limites de la raquette sous forme de rectangle
   Paramètre paddle : t, la raquette dont on veut les limites
   Résultat : ((x_min,y_min), (x_max,y_max)), les coordonnées des coins
              supérieur gauche et inférieur droit du rectangle englobant
*)
  val get_bounds : t -> (float * float) * (float * float)

  (*
   remove_effect : Brick.bonus_effect -> t -> t
   retire un effet actif de la raquette
   Paramètre effect : Brick.bonus_effect, l'effet à retirer
   Paramètre paddle : t, la raquette à modifier
   Résultat : la raquette avec l'effet retiré et ses propriétés restaurées
*)
  val remove_effect : Brick.bonus_effect -> t -> t
end


module Paddle : PADDLE = struct

  type effect_state = {
    stretch_factor: float;
    effect_active: bool;
  }
  
  (*
   type t 
   Représente une raquette dans le jeu
   Champs :
   - position : float * float, coordonnées (x,y) du centre de la raquette
   - width : float, largeur actuelle de la raquette (> 0)
   - height : float, hauteur de la raquette (> 0)
   - speed : float, vitesse de déplacement horizontale (> 0)
   - effect_state : effect_state, état des effets actifs sur la raquette
        * stretch_factor : float, facteur d'étirement actuel (1.0 = normal)
        * effect_active : bool, indique si un effet est actif
   Invariants :
   - width >= paddle_width/4.0 && width <= screen_width/2.0 dans son état actuel 
   - La position x reste dans les limites de l'écran ± margin
*)
  type t = {
    position: float*float;
    width: float;
    height: float;
    speed: float;
    effect_state: effect_state;
    
  }
  

  let create (x, y) width height speed = {
    position = (x, y);
    width;
    height;
    speed;
    effect_state = { stretch_factor = 1.; effect_active = false }
  }

  let move  margin screen_bounds mouse_x paddle =
       (* Extrait les limites horizontales de l'écran *)
      let ((min_x, _), (max_x, _)) = screen_bounds in
      (* Calcule la nouvelle position x en s'assurant que la raquette reste dans les limites *)
      let new_x = max margin (min (max_x -. paddle.width -. margin ) mouse_x)           
      in
      { paddle with position = (new_x, snd paddle.position) }

  let get_dimensions paddle = (paddle.width, paddle.height)

  let get_position paddle = paddle.position
      
  let get_width paddle = paddle.width

  let apply_effect effect paddle =
        match effect with
        | Brick.StretchPaddle -> 
            let new_width = min 
              (paddle.width *. Config.paddle_stretch_factor)
              ((fst (snd Config.screen_bounds)) /. 2.0) in 
            { paddle with 
              width = new_width;
              effect_state = { stretch_factor = Config.paddle_stretch_factor; 
                              effect_active = true }
            }
        | Brick.ShrinkPaddle ->
          let new_width = max 
            (paddle.width *. Config.paddle_shrink_factor)
            (Config.paddle_width /. 4.0) in 
          { paddle with 
            width = new_width;
            effect_state = { stretch_factor = Config.paddle_shrink_factor;
                            effect_active = true }
          }
      | _ -> paddle

  let remove_effect effect paddle =
        if paddle.effect_state.effect_active then
          match effect with
          | Brick.StretchPaddle when paddle.width > Config.paddle_width ->
              (* Inversion du stretch mais pas en dessous de la largeur initiale *)
              let new_width = max
                (paddle.width /. Config.paddle_stretch_factor)
                Config.paddle_width in
              { paddle with
                width = new_width;
                effect_state = { stretch_factor = 1.0; effect_active = false }
              }
          | Brick.ShrinkPaddle when paddle.width < Config.paddle_width ->
              (* Pour un shrink, on revient directement à la largeur initiale *)
              { paddle with
                width = Config.paddle_width;
                effect_state = { stretch_factor = 1.0; effect_active = false }
              }
          | _ -> paddle  
        else paddle

  
  let get_bounds paddle =
    let (x, y) = paddle.position in
    let half_width = paddle.width /. 2. in
    let paddle_min_x = x -. half_width in
    let paddle_max_x = x +. half_width in
    let paddle_min_y = y in
    let paddle_max_y = y +. paddle.height in
    ((paddle_min_x, paddle_min_y), (paddle_max_x, paddle_max_y))



end