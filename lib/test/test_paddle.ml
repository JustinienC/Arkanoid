open OUnit2
open Paddle

let test_create _ =
  let paddle = Paddle.create (0., 0.) 100. 20. 5. in
  assert_equal (Paddle.get_position paddle) (0., 0.);
  assert_equal (Paddle.get_width paddle) 100.;
  assert_equal (Paddle.get_dimensions paddle) (100., 20.)

let test_move _ =
  let paddle = Paddle.create (0., 0.) 100. 20. 5. in
  let screen_bounds = ((0., 0.), (800., 600.)) in
  let moved_paddle = Paddle.move 10. screen_bounds 400. paddle in
  assert_equal (fst (Paddle.get_position moved_paddle)) 400.

let test_apply_effect _ =
  let paddle = Paddle.create (0., 0.) 100. 20. 5. in
  let stretched_paddle = Paddle.apply_effect Brick.StretchPaddle paddle in
  assert_equal (Paddle.get_width stretched_paddle) (100. *. Config.paddle_stretch_factor);
  let shrunk_paddle = Paddle.apply_effect Brick.ShrinkPaddle paddle in
  assert_equal (Paddle.get_width shrunk_paddle) (100. *. Config.paddle_shrink_factor)

let test_remove_effect _ =
  let paddle = Paddle.create (0., 0.) 100. 20. 5. in
  let stretched_paddle = Paddle.apply_effect Brick.StretchPaddle paddle in
  let normal_paddle = Paddle.remove_effect Brick.StretchPaddle stretched_paddle in
  assert_equal (Paddle.get_width normal_paddle) 100.;
  let shrunk_paddle = Paddle.apply_effect Brick.ShrinkPaddle paddle in
  let normal_paddle = Paddle.remove_effect Brick.ShrinkPaddle shrunk_paddle in
  assert_equal (Paddle.get_width normal_paddle) 100.

let suite =
  "Paddle Tests" >::: [
    "test_create" >:: test_create;
    "test_move" >:: test_move;
    "test_apply_effect" >:: test_apply_effect;
    "test_remove_effect" >:: test_remove_effect;
  ]

let () =
  run_test_tt_main suite
  open OUnit2
  open Paddle

  let test_create _ =
    let paddle = Paddle.create (0., 0.) 100. 20. 5. in
    assert_equal (Paddle.get_position paddle) (0., 0.);
    assert_equal (Paddle.get_width paddle) 100.;
    assert_equal (Paddle.get_dimensions paddle) (100., 20.)

  let test_move _ =
    let paddle = Paddle.create (0., 0.) 100. 20. 5. in
    let screen_bounds = ((0., 0.), (800., 600.)) in
    let moved_paddle = Paddle.move 10. screen_bounds 400. paddle in
    assert_equal (fst (Paddle.get_position moved_paddle)) 400.

  let test_apply_effect _ =
    let paddle = Paddle.create (0., 0.) 100. 20. 5. in
    let stretched_paddle = Paddle.apply_effect Brick.StretchPaddle paddle in
    assert_equal (Paddle.get_width stretched_paddle) (100. *. Config.paddle_stretch_factor);
    let shrunk_paddle = Paddle.apply_effect Brick.ShrinkPaddle paddle in
    assert_equal (Paddle.get_width shrunk_paddle) (100. *. Config.paddle_shrink_factor)

  let test_remove_effect _ =
    let paddle = Paddle.create (0., 0.) 100. 20. 5. in
    let stretched_paddle = Paddle.apply_effect Brick.StretchPaddle paddle in
    let normal_paddle = Paddle.remove_effect Brick.StretchPaddle stretched_paddle in
    assert_equal (Paddle.get_width normal_paddle) 100.;
    let shrunk_paddle = Paddle.apply_effect Brick.ShrinkPaddle paddle in
    let normal_paddle = Paddle.remove_effect Brick.ShrinkPaddle shrunk_paddle in
    assert_equal (Paddle.get_width normal_paddle) 100.

  let suite =
    "Paddle Tests" >::: [
      "test_create" >:: test_create;
      "test_move" >:: test_move;
      "test_apply_effect" >:: test_apply_effect;
      "test_remove_effect" >:: test_remove_effect;
    ]

  let () =
    run_test_tt_main suite
