(* lib/config.ml *)
module Config = struct

  (* Nombre initial de vies du joueur *)
  let initial_lives = 100

  (* Score initial au début du jeu *)
  let initial_score = 0

  (* Niveau initial du jeu *)
  let initial_level = 1


  (* Points de base gagnés pour chaque brique détruite *)
  let base_bonus=10

 
  (* Position initiale de la raquette (x,y) *)
  let paddle_position=(350. , 50. )

   (* Largeur de la raquette en pixels *)
  let paddle_width = 100.


  (* Hauteur de la raquette en pixels *)
  let paddle_height = 10.
  let paddle_magin = 0. (* Marge minimale entre la raquette et les bords de l'écran *)

  let paddle_velocity = 10.  (* Vitesse de déplacement horizontal de la raquette. Nous ne l'avons pas vraiment utilisé*)


  let paddle_stretch_factor = 1.1 (* Facteur d'agrandissement de la raquette avec le bonus StretchPaddle *)
  let paddle_shrink_factor = 0.75 (* Facteur de rétrécissement de la raquette avec le bonus ShrinkPaddle *)
  let ball_radius = 5. (* Rayon de la balle en pixels *)
  let ball_mass = 0.85 (* Masse de la balle pour les calculs physiques . Nous ne l'avons pas vraiment utilisé.*)
  let initial_ball_speed = 400. (* Vitesse initiale de la balle *)

  (* Dimensions des briques *)
  let brick_width =40.   
  let brick_height = 10.
  let brick_spacing = 15.   (* Espacement entre les briques *)

  (* Nombre de rangées et colonnes de briques *)
  let rows = 7 
  let cols = 14
  let gravity = 9.81
  let screen_bounds =  ((0., 0.), (790., 590.)) (* Limites de l'écran ((x_min,y_min), (x_max,y_max)) *)

  let bonus_velocity = 200. 
  (* Vitesse de chute des bonus *)
  let bonus_lifetime = 3.
  (* Durée de vie des bonus en secondes *)

  (* Dimensions des bonus (largeur, hauteur) *)
  let bonus_dimensions = (20., 12.)

   (* Facteur de multiplication de la vitesse de la balle après rebond *)
  let ball_factor = 1.1

end