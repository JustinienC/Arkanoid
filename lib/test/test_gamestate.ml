(* lib/test_gamestate.ml *)

open OUnit2
open Gamestate
open Config

let test_calculate_brick_layout _ =
  let layout = Gamestate.calculate_brick_layout brick_width brick_height screen_bounds brick_spacing in
  assert_equal layout.cols 10;
  assert_equal layout.rows 5;
  assert_equal layout.brick_count (10 * 5);
  assert_equal layout.spacing (brick_spacing, brick_spacing)

let test_create_brick_grid _ =
  let layout = Gamestate.calculate_brick_layout brick_width brick_height screen_bounds brick_spacing in
  let bricks = Gamestate.create_brick_grid layout.rows layout.cols brick_width brick_height brick_spacing initial_level in
  assert_equal (List.length bricks) layout.brick_count

let test_create_gamestate _ =
  let state = Gamestate.create () in
  assert_equal state.score initial_score;
  assert_equal state.lives initial_lives;
  assert_equal state.level initial_level;
  assert_equal state.status Gamestate.Starting;
  assert_equal (List.length state.bricks) (Gamestate.calculate_brick_layout brick_width brick_height screen_bounds brick_spacing).brick_count

let suite =
  "Gamestate Tests" >::: [
    "test_calculate_brick_layout" >:: test_calculate_brick_layout;
    "test_create_brick_grid" >:: test_create_brick_grid;
    "test_create_gamestate" >:: test_create_gamestate;
  ]

let () =
  run_test_tt_main suite
  (* lib/test_gamestate.ml *)

  open OUnit2
  open Gamestate
  open Config

  let test_calculate_brick_layout _ =
    let layout = Gamestate.calculate_brick_layout brick_width brick_height screen_bounds brick_spacing in
    assert_equal layout.cols 10;
    assert_equal layout.rows 5;
    assert_equal layout.brick_count (10 * 5);
    assert_equal layout.spacing (brick_spacing, brick_spacing)

  let test_create_brick_grid _ =
    let layout = Gamestate.calculate_brick_layout brick_width brick_height screen_bounds brick_spacing in
    let bricks = Gamestate.create_brick_grid layout.rows layout.cols brick_width brick_height brick_spacing initial_level in
    assert_equal (List.length bricks) layout.brick_count

  let test_create_gamestate _ =
    let state = Gamestate.create () in
    assert_equal state.score initial_score;
    assert_equal state.lives initial_lives;
    assert_equal state.level initial_level;
    assert_equal state.status Gamestate.Starting;
    assert_equal (List.length state.bricks) (Gamestate.calculate_brick_layout brick_width brick_height screen_bounds brick_spacing).brick_count

  let suite =
    "Gamestate Tests" >::: [
      "test_calculate_brick_layout" >:: test_calculate_brick_layout;
      "test_create_brick_grid" >:: test_create_brick_grid;
      "test_create_gamestate" >:: test_create_gamestate;
    ]

  let () =
    run_test_tt_main suite
