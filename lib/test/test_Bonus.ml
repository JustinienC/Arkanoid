open OUnit2
open Bonus
open Brick
open Paddle

let test_create _ =
  let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
  assert_equal (0., 0.) (Bonus.get_position bonus);
  assert_equal Brick.StretchPaddle (Bonus.get_effect bonus);
  assert_equal Config.bonus_velocity bonus.velocity;
  assert_equal Config.bonus_lifetime bonus.lifetime;
  assert_equal false (Bonus.is_effect_active bonus)

let test_update _ =
  let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
  let updated_bonus = Bonus.update 1.0 bonus in
  assert_equal (0., -.Config.bonus_velocity) (Bonus.get_position updated_bonus);
  assert_equal Config.bonus_lifetime updated_bonus.lifetime;
  assert_equal false (Bonus.is_effect_active updated_bonus)

let test_is_expired _ =
  let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
  let expired_bonus = Bonus.update Config.bonus_lifetime bonus in
  assert_equal true (Bonus.is_expired expired_bonus)

let test_check_collision _ =
  let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
  let paddle = Paddle.create (0., 0.) in
  assert_equal true (Bonus.check_collision bonus paddle)

let test_apply_effect _ =
  let paddle = Paddle.create (0., 0.) in
  let stretched_paddle = Bonus.apply_effect Brick.StretchPaddle paddle in
  assert_equal true (Paddle.is_stretched stretched_paddle)

let test_set_is_active _ =
  let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
  let active_bonus = Bonus.set_is_active true bonus in
  assert_equal true (Bonus.is_effect_active active_bonus)

let suite =
  "Bonus Tests" >::: [
    "test_create" >:: test_create;
    "test_update" >:: test_update;
    "test_is_expired" >:: test_is_expired;
    "test_check_collision" >:: test_check_collision;
    "test_apply_effect" >:: test_apply_effect;
    "test_set_is_active" >:: test_set_is_active;
  ]

let () =
  run_test_tt_main suite
  open OUnit2
  open Bonus
  open Brick
  open Paddle

  let test_create _ =
    let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
    assert_equal (0., 0.) (Bonus.get_position bonus);
    assert_equal Brick.StretchPaddle (Bonus.get_effect bonus);
    assert_equal Config.bonus_velocity bonus.velocity;
    assert_equal Config.bonus_lifetime bonus.lifetime;
    assert_equal false (Bonus.is_effect_active bonus)

  let test_update _ =
    let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
    let updated_bonus = Bonus.update 1.0 bonus in
    assert_equal (0., -.Config.bonus_velocity) (Bonus.get_position updated_bonus);
    assert_equal Config.bonus_lifetime updated_bonus.lifetime;
    assert_equal false (Bonus.is_effect_active updated_bonus)

  let test_is_expired _ =
    let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
    let expired_bonus = Bonus.update Config.bonus_lifetime bonus in
    assert_equal true (Bonus.is_expired expired_bonus)

  let test_check_collision _ =
    let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
    let paddle = Paddle.create (0., 0.) in
    assert_equal true (Bonus.check_collision bonus paddle)

  let test_apply_effect _ =
    let paddle = Paddle.create (0., 0.) in
    let stretched_paddle = Bonus.apply_effect Brick.StretchPaddle paddle in
    assert_equal true (Paddle.is_stretched stretched_paddle)

  let test_set_is_active _ =
    let bonus = Bonus.create (0., 0.) Brick.StretchPaddle in
    let active_bonus = Bonus.set_is_active true bonus in
    assert_equal true (Bonus.is_effect_active active_bonus)

  let suite =
    "Bonus Tests" >::: [
      "test_create" >:: test_create;
      "test_update" >:: test_update;
      "test_is_expired" >:: test_is_expired;
      "test_check_collision" >:: test_check_collision;
      "test_apply_effect" >:: test_apply_effect;
      "test_set_is_active" >:: test_set_is_active;
    ]

  let () =
    run_test_tt_main suite
