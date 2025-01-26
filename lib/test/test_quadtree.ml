open OUnit2
open Quadtree

let test_create_bounds _ =
  let bounds = Quadtree.create_bounds 0. 0. 100. 100. in
  assert_equal bounds.x 0.;
  assert_equal bounds.y 0.;
  assert_equal bounds.width 100.;
  assert_equal bounds.height 100.

let test_inside_bounds _ =
  let bounds = Quadtree.create_bounds 0. 0. 100. 100. in
  let obj = Brick.create 50. 50. 10. 10. in
  assert_bool "Object should be inside bounds" (Quadtree.inside_bounds obj bounds);
  let obj_outside = Brick.create 150. 150. 10. 10. in
  assert_bool "Object should be outside bounds" (not (Quadtree.inside_bounds obj_outside bounds))

let test_insert _ =
  let bounds = Quadtree.create_bounds 0. 0. 100. 100. in
  let tree = Quadtree.Branch { nw = Quadtree.Empty; ne = Quadtree.Empty; sw = Quadtree.Empty; se = Quadtree.Empty; bounds } in
  let obj = Brick.create 50. 50. 10. 10. in
  let tree = Quadtree.insert tree obj in
  match tree with
  | Quadtree.Branch { nw = Quadtree.Leaf objs; _ } -> assert_equal (List.length objs) 1
  | _ -> assert_failure "Object was not inserted correctly"

let test_build_quadtree _ =
  let bricks = [Brick.create 10. 10. 10. 10.; Brick.create 90. 90. 10. 10.] in
  let screen_bounds = ((0., 0.), (100., 100.)) in
  let tree = Quadtree.build_quadtree bricks screen_bounds in
  match tree with
  | Quadtree.Branch { nw = Quadtree.Leaf nw_objs; se = Quadtree.Leaf se_objs; _ } ->
      assert_equal (List.length nw_objs) 1;
      assert_equal (List.length se_objs) 1
  | _ -> assert_failure "Quadtree was not built correctly"

let test_query _ =
  let bricks = [Brick.create 10. 10. 10. 10.; Brick.create 90. 90. 10. 10.] in
  let screen_bounds = ((0., 0.), (100., 100.)) in
  let tree = Quadtree.build_quadtree bricks screen_bounds in
  let ball = Ball.create 10. 10. 5. in
  let (candidates, _) = Quadtree.query tree ball in
  assert_equal (List.length candidates) 1

let suite =
  "Quadtree Tests" >::: [
    "test_create_bounds" >:: test_create_bounds;
    "test_inside_bounds" >:: test_inside_bounds;
    "test_insert" >:: test_insert;
    "test_build_quadtree" >:: test_build_quadtree;
    "test_query" >:: test_query;
  ]

let () =
  run_test_tt_main suite
  open OUnit2
  open Quadtree

  let test_create_bounds _ =
    let bounds = Quadtree.create_bounds 0. 0. 100. 100. in
    assert_equal bounds.x 0.;
    assert_equal bounds.y 0.;
    assert_equal bounds.width 100.;
    assert_equal bounds.height 100.

  let test_inside_bounds _ =
    let bounds = Quadtree.create_bounds 0. 0. 100. 100. in
    let obj = Brick.create 50. 50. 10. 10. in
    assert_bool "Object should be inside bounds" (Quadtree.inside_bounds obj bounds);
    let obj_outside = Brick.create 150. 150. 10. 10. in
    assert_bool "Object should be outside bounds" (not (Quadtree.inside_bounds obj_outside bounds))

  let test_insert _ =
    let bounds = Quadtree.create_bounds 0. 0. 100. 100. in
    let tree = Quadtree.Branch { nw = Quadtree.Empty; ne = Quadtree.Empty; sw = Quadtree.Empty; se = Quadtree.Empty; bounds } in
    let obj = Brick.create 50. 50. 10. 10. in
    let tree = Quadtree.insert tree obj in
    match tree with
    | Quadtree.Branch { nw = Quadtree.Leaf objs; _ } -> assert_equal (List.length objs) 1
    | _ -> assert_failure "Object was not inserted correctly"

  let test_build_quadtree _ =
    let bricks = [Brick.create 10. 10. 10. 10.; Brick.create 90. 90. 10. 10.] in
    let screen_bounds = ((0., 0.), (100., 100.)) in
    let tree = Quadtree.build_quadtree bricks screen_bounds in
    match tree with
    | Quadtree.Branch { nw = Quadtree.Leaf nw_objs; se = Quadtree.Leaf se_objs; _ } ->
        assert_equal (List.length nw_objs) 1;
        assert_equal (List.length se_objs) 1
    | _ -> assert_failure "Quadtree was not built correctly"

  let test_query _ =
    let bricks = [Brick.create 10. 10. 10. 10.; Brick.create 90. 90. 10. 10.] in
    let screen_bounds = ((0., 0.), (100., 100.)) in
    let tree = Quadtree.build_quadtree bricks screen_bounds in
    let ball = Ball.create 10. 10. 5. in
    let (candidates, _) = Quadtree.query tree ball in
    assert_equal (List.length candidates) 1

  let suite =
    "Quadtree Tests" >::: [
      "test_create_bounds" >:: test_create_bounds;
      "test_inside_bounds" >:: test_inside_bounds;
      "test_insert" >:: test_insert;
      "test_build_quadtree" >:: test_build_quadtree;
      "test_query" >:: test_query;
    ]

  let () =
    run_test_tt_main suite
