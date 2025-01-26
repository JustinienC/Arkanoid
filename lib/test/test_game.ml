open OUnit2
open GameEngine
open Gamestate
open Ball
open Paddle
open Brick
open Quadtree
open Bonus

let test_initial_state _ =
  let state = initial_state in
  assert_equal state.lives Config.initial_lives;
  assert_equal state.score 0;
  assert_equal state.level 1;
  assert_equal state.status Playing

let test_apply_special_effects _ =
  let state = { initial_state with lives = 1; score = 0 } in
  let bonuses = [Bonus.create (0.0, 0.0) Brick.ExtraLife; Bonus.create (0.0, 0.0) (Brick.ScoreBonus 100)] in
  let new_state = apply_special_effects state bonuses in
  assert_equal new_state.lives 2;
  assert_equal new_state.score 100

let test_update_ball_position _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 1.0 1.0 in
  let new_ball = update_ball_position 1.0 ball in
  assert_equal (Ball.get_position new_ball) (1.0, 1.0)

let test_handle_ball_collisions _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 1.0 1.0 in
  let paddle = Paddle.create (0.0, 0.0) 10.0 1.0 1.0 in
  let bounds = (0.0, 0.0, 100.0, 100.0) in
  let new_ball = handle_ball_collisions ball paddle bounds in
  assert_equal (Ball.get_velocity new_ball) (1.0, 1.0)

let test_handle_brick_collisions _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 1.0 1.0 in
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 in
  let quadtree = Quadtree.build_quadtree [brick] (0.0, 0.0, 100.0, 100.0) in
  let (colliding_bricks, _, _) = handle_brick_collisions ball quadtree in
  assert_equal (List.length colliding_bricks) 1

let test_update_bricks_state _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 1.0 1.0 in
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 in
  let (new_ball, destroyed_bricks, _, score_increment) = update_bricks_state ball [brick] in
  assert_equal (List.length destroyed_bricks) 1;
  assert_equal score_increment (Brick.get_value brick)

let test_handle_bonuses _ =
  let state = initial_state in
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 in
  let new_paddle = Paddle.create (0.0, 0.0) 10.0 1.0 1.0 in
  let (collected_bonuses, remaining_bonus_items) = handle_bonuses 1.0 state [brick] new_paddle in
  assert_equal (List.length collected_bonuses) 0;
  assert_equal (List.length remaining_bonus_items) 0

let test_handle_active_effects _ =
  let state = initial_state in
  let new_paddle = Paddle.create (0.0, 0.0) 10.0 1.0 1.0 in
  let (new_paddle_with_effects, all_active_effects) = handle_active_effects 1.0 state new_paddle [] in
  assert_equal (List.length all_active_effects) 0

let test_create_lost_ball_state _ =
  let state = { initial_state with lives = 3; score = 0 } in
  let new_state = create_lost_ball_state state 100 (Quadtree.build_quadtree [] (0.0, 0.0, 100.0, 100.0)) in
  assert_equal new_state.lives 2;
  assert_equal new_state.score 100

let test_handle_ball_effects _ =
  let state = initial_state in
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 1.0 1.0 in
  let (new_ball, new_ball_effects, _) = handle_ball_effects 1.0 state [] ball in
  assert_equal (Ball.get_velocity new_ball) (1.0, 1.0);
  assert_equal (List.length new_ball_effects) 0

let test_game_loop _ =
  let state = initial_state in
  let new_state = game_loop 1.0 (0.0, false) state in
  assert_equal new_state.status Playing

let test_update_state _ =
  let state = initial_state in
  let new_state = update_state 1.0 (0.0, false) state in
  assert_equal new_state.status Playing

let suite =
  "GameEngine Tests" >::: [
    "test_initial_state" >:: test_initial_state;
    "test_apply_special_effects" >:: test_apply_special_effects;
    "test_update_ball_position" >:: test_update_ball_position;
    "test_handle_ball_collisions" >:: test_handle_ball_collisions;
    "test_handle_brick_collisions" >:: test_handle_brick_collisions;
    "test_update_bricks_state" >:: test_update_bricks_state;
    "test_handle_bonuses" >:: test_handle_bonuses;
    "test_handle_active_effects" >:: test_handle_active_effects;
    "test_create_lost_ball_state" >:: test_create_lost_ball_state;
    "test_handle_ball_effects" >:: test_handle_ball_effects;
    "test_game_loop" >:: test_game_loop;
    "test_update_state" >:: test_update_state;
  ]

let () =
  run_test_tt_main suite