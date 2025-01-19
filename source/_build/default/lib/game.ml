
(* game.ml *)
open Type
open Iterator
open Input

let initial_state = {
  balle = {
    pos = (400., 300.);
    vel = (2., -2.);
    radius = 5.;
  };
  racket = {
    pos = (350., 50.);
    dim = (100., 10.);
    vel = (0., 0.);
  };
  score = 0;
  lives = 3;
}

let update_ball_position dt ball = 
  let (x, y) = ball.pos in
  let (vx, vy) = ball.vel in
  let gravity = 0.1 in  (* Légère gravité *)
  let new_vy = vy -. (gravity *. dt) in
  {ball with 
    pos = (x +. vx *. dt, y +. new_vy *. dt);
    vel = (vx, new_vy)
  }

let update_racket_position dt mouse_x racket =
  let (_, y) = racket.pos in
  let target_x = mouse_x -. (fst racket.dim) /. 2. in
  let current_x = fst racket.pos in
  let dx = target_x -. current_x in
  let new_vx = dx *. 5. in
  {racket with 
    pos = (current_x +. new_vx *. dt, y);
    vel = (new_vx, 0.)
  }

let check_wall_collision ball box =
  let (x, y) = ball.pos in
  let (vx, vy) = ball.vel in
  let bounce_factor = 1.01 in  (* Légère accélération au rebond *)
  
  let new_vx = 
    if x <= box.infx || x >= box.supx then
      -.vx *. bounce_factor
    else vx in
  
  let new_vy =
    if y <= box.infy || y >= box.supy then
      -.vy *. bounce_factor
    else vy in
  
  {ball with vel = (new_vx, new_vy)}

let update_game_state dt mouse_input state =
  let mouse_x, _ = mouse_input in
  let new_racket = update_racket_position dt mouse_x state.racket in
  let new_ball = state.balle
    |> update_ball_position dt
    |> check_wall_collision box in
  {state with
    balle = new_ball;
    racket = new_racket;
  }

let game_flux initial_state mouse_flux =
  Flux.unfold
    (fun state ->
      let mouse_input = Flux.head mouse_flux in
      let new_state = update_game_state Init.dt mouse_input state in
      Some (new_state, new_state))
    initial_state

(* newtonoid.ml *)
let draw_state state =
  (* Dessiner la raquette *)
  let (rx, ry) = state.racket.pos in
  let (rw, rh) = state.racket.dim in
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 
    (int_of_float (rx -. rw/.2.)) 
    (int_of_float (ry -. rh/.2.))
    (int_of_float rw) 
    (int_of_float rh);
  
  (* Dessiner la balle *)
  let (bx, by) = state.balle.pos in
  Graphics.set_color Graphics.red;
  Graphics.fill_circle 
    (int_of_float bx) 
    (int_of_float by) 
    (int_of_float state.balle.radius)

let score state = 0  (* À implémenter avec la logique de score *)

let () =
  Random.self_init ();
  let game_state = game_flux initial_state mouse in
  draw game_state