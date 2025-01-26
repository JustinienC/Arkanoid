open OUnit2
open Brick

let test_create _ =
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Classic in
  assert_equal (Brick.get_position brick) (0.0, 0.0);
  assert_equal (Brick.get_dimensions brick) (10.0, 5.0);
  assert_equal (Brick.get_value brick) 100;
  assert_equal (Brick.get_type brick) Classic

let test_hit _ =
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 2 100 (Reinforced 2) in
  let hit_brick = match Brick.hit brick with Some b -> b | None -> brick in
  assert_equal (Brick.get_value hit_brick) 0;
  assert_equal (Brick.is_destroyed hit_brick) false;
  let hit_brick = match Brick.hit hit_brick with Some b -> b | None -> brick in
  assert_equal (Brick.is_destroyed hit_brick) true

let test_is_destroyed _ =
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Classic in
  assert_equal (Brick.is_destroyed brick) false;
  let hit_brick = match Brick.hit brick with Some b -> b | None -> brick in
  assert_equal (Brick.is_destroyed hit_brick) true

let test_check_collision _ =
  let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Classic in
  assert_equal (Brick.check_collision brick (5.0, 2.5) 1.0) true;
  assert_equal (Brick.check_collision brick (15.0, 2.5) 1.0) false

let suite =
  "Brick Tests" >::: [
    "test_create" >:: test_create;
    "test_hit" >:: test_hit;
    "test_is_destroyed" >:: test_is_destroyed;
    "test_check_collision" >:: test_check_collision;
  ]

let () =
  run_test_tt_main suite
  open OUnit2
  open Brick

  let test_create _ =
    let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Classic in
    assert_equal (Brick.get_position brick) (0.0, 0.0);
    assert_equal (Brick.get_dimensions brick) (10.0, 5.0);
    assert_equal (Brick.get_value brick) 100;
    assert_equal (Brick.get_type brick) Classic

  let test_hit _ =
    let brick = Brick.create (0.0, 0.0) 10.0 5.0 2 100 (Reinforced 2) in
    let hit_brick = match Brick.hit brick with Some b -> b | None -> brick in
    assert_equal (Brick.get_value hit_brick) 0;
    assert_equal (Brick.is_destroyed hit_brick) false;
    let hit_brick = match Brick.hit hit_brick with Some b -> b | None -> brick in
    assert_equal (Brick.is_destroyed hit_brick) true

  let test_is_destroyed _ =
    let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Classic in
    assert_equal (Brick.is_destroyed brick) false;
    let hit_brick = match Brick.hit brick with Some b -> b | None -> brick in
    assert_equal (Brick.is_destroyed hit_brick) true

  let test_check_collision _ =
    let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Classic in
    assert_equal (Brick.check_collision brick (5.0, 2.5) 1.0) true;
    assert_equal (Brick.check_collision brick (15.0, 2.5) 1.0) false

  let test_indestructible _ =
    let brick = Brick.create (0.0, 0.0) 10.0 5.0 1 100 Indestructible in
    assert_equal (Brick.is_destroyed brick) false;
    let hit_brick = match Brick.hit brick with Some b -> b | None -> brick in
    assert_equal (Brick.is_destroyed hit_brick) false;
    assert_equal (Brick.get_value hit_brick) 0

  let suite =
    "Brick Tests" >::: [
      "test_create" >:: test_create;
      "test_hit" >:: test_hit;
      "test_is_destroyed" >:: test_is_destroyed;
      "test_check_collision" >:: test_check_collision;
      "test_indestructible" >:: test_indestructible;
    ]

  let () =
    run_test_tt_main suite
