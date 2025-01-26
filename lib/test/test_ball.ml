open OUnit2
open Ball

let test_create _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  assert_equal (Ball.get_position ball) (0.0, 0.0);
  assert_equal (Ball.get_velocity ball) (1.0, 1.0);
  assert_equal (Ball.get_radius ball) 5.0;
  assert_equal (Ball.get_mass ball) 1.0

let test_update _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  let updated_ball = Ball.update 9.8 1.0 ball in
  assert_equal (Ball.get_position updated_ball) (1.0, -3.9);
  assert_equal (Ball.get_velocity updated_ball) (1.0, -8.8)

let test_bounce _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  let bounced_ball = Ball.bounce ((-10.0, 10.0), (-10.0, 10.0)) ((-5.0, 5.0), (-5.0, 5.0)) ball in
  assert_equal (Ball.get_velocity bounced_ball) (-1.0, 1.0)

let test_apply_effect _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  let speed_up_ball = Ball.apply_effect Brick.SpeedUpBall ball in
  assert_equal (Ball.get_velocity speed_up_ball) (1.2, 1.2);
  let slow_down_ball = Ball.apply_effect Brick.SlowDownBall ball in
  assert_equal (Ball.get_velocity slow_down_ball) (0.8, 0.8)

let test_is_lost _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  assert_equal (Ball.is_lost ((-10.0, 10.0), (-10.0, 10.0)) ball) false;
  let lost_ball = Ball.create (0.0, -15.0) (1.0, 1.0) 5.0 1.0 in
  assert_equal (Ball.is_lost ((-10.0, 10.0), (-10.0, 10.0)) lost_ball) true

let test_set_velocity _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  let new_velocity_ball = Ball.set_velocity (2.0, 2.0) ball in
  assert_equal (Ball.get_velocity new_velocity_ball) (2.0, 2.0)

let test_set_position _ =
  let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
  let new_position_ball = Ball.set_position ball (2.0, 2.0) in
  assert_equal (Ball.get_position new_position_ball) (2.0, 2.0)

let suite =
  "Ball Tests" >::: [
    "test_create" >:: test_create;
    "test_update" >:: test_update;
    "test_bounce" >:: test_bounce;
    "test_apply_effect" >:: test_apply_effect;
    "test_is_lost" >:: test_is_lost;
    "test_set_velocity" >:: test_set_velocity;
    "test_set_position" >:: test_set_position;
  ]

let () =
  run_test_tt_main suite


  open OUnit2
  open Ball

  let test_create _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    assert_equal (Ball.get_position ball) (0.0, 0.0);
    assert_equal (Ball.get_velocity ball) (1.0, 1.0);
    assert_equal (Ball.get_radius ball) 5.0;
    assert_equal (Ball.get_mass ball) 1.0

  let test_update _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let updated_ball = Ball.update 9.8 1.0 ball in
    assert_equal (Ball.get_position updated_ball) (1.0, -3.9);
    assert_equal (Ball.get_velocity updated_ball) (1.0, -8.8)

  let test_bounce _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let bounced_ball = Ball.bounce ((-10.0, 10.0), (-10.0, 10.0)) ((-5.0, 5.0), (-5.0, 5.0)) ball in
    assert_equal (Ball.get_velocity bounced_ball) (-1.0, 1.0)

  let test_apply_effect _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let speed_up_ball = Ball.apply_effect Brick.SpeedUpBall ball in
    assert_equal (Ball.get_velocity speed_up_ball) (1.2, 1.2);
    let slow_down_ball = Ball.apply_effect Brick.SlowDownBall ball in
    assert_equal (Ball.get_velocity slow_down_ball) (0.8, 0.8)

  let test_is_lost _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    assert_equal (Ball.is_lost ((-10.0, 10.0), (-10.0, 10.0)) ball) false;
    let lost_ball = Ball.create (0.0, -15.0) (1.0, 1.0) 5.0 1.0 in
    assert_equal (Ball.is_lost ((-10.0, 10.0), (-10.0, 10.0)) lost_ball) true

  let test_set_velocity _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let new_velocity_ball = Ball.set_velocity (2.0, 2.0) ball in
    assert_equal (Ball.get_velocity new_velocity_ball) (2.0, 2.0)

  let test_set_position _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let new_position_ball = Ball.set_position ball (2.0, 2.0) in
    assert_equal (Ball.get_position new_position_ball) (2.0, 2.0)

  let test_handle_wall_collision _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let screen_bounds = ((-10.0, -10.0), (10.0, 10.0)) in
    let collided_ball = Ball.handle_wall_collision ball screen_bounds in
    assert_bool "Ball should bounce off the wall" 
      (Ball.get_velocity collided_ball <> Ball.get_velocity ball)

  let test_handle_paddle_collision _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let paddle_position = (0.0, -5.0) in
    let paddle_size = (10.0, 1.0) in
    let screen_bounds = ((-10.0, -10.0), (10.0, 10.0)) in
    let collided_ball = Ball.handle_paddle_collision ball paddle_position paddle_size screen_bounds in
    assert_bool "Ball should bounce off the paddle" 
      (Ball.get_velocity collided_ball <> Ball.get_velocity ball)

  let test_handle_brick_collision _ =
    let ball = Ball.create (0.0, 0.0) (1.0, 1.0) 5.0 1.0 in
    let brick_position = (0.0, 0.0) in
    let collided_ball = Ball.handle_brick_collision ball brick_position in
    assert_bool "Ball should bounce off the brick" 
      (Ball.get_velocity collided_ball <> Ball.get_velocity ball)

  let suite =
    "Ball Tests" >::: [
      "test_create" >:: test_create;
      "test_update" >:: test_update;
      "test_bounce" >:: test_bounce;
      "test_apply_effect" >:: test_apply_effect;
      "test_is_lost" >:: test_is_lost;
      "test_set_velocity" >:: test_set_velocity;
      "test_set_position" >:: test_set_position;
      "test_handle_wall_collision" >:: test_handle_wall_collision;
      "test_handle_paddle_collision" >:: test_handle_paddle_collision;
      "test_handle_brick_collision" >:: test_handle_brick_collision;
    ]

  let () =
    run_test_tt_main suite
