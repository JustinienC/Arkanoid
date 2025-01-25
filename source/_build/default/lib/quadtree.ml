open Brick
open Ball

module Quadtree = struct
  type bounds = {
    x: float;
    y: float;
    width: float;
    height: float;
  }

  type 'a node = 
    | Empty
    | Leaf of 'a list
    | Branch of {
        nw: 'a node;
        ne: 'a node;
        sw: 'a node;
        se: 'a node;
        bounds: bounds;
      }

  let create_bounds x y width height = 
    { x; y; width; height }

  let inside_bounds obj bounds =
    let (obj_x, obj_y) = Brick.get_position obj in
    obj_x >= bounds.x && 
    obj_x < bounds.x +. bounds.width &&
    obj_y >= bounds.y && 
    obj_y < bounds.y +. bounds.height

  let rec insert tree obj =
    match tree with
    | Empty -> Leaf [obj]
    | Leaf objs -> Leaf (obj :: objs)
    | Branch { nw; ne; sw; se; bounds } ->
        let nw_bounds = create_bounds bounds.x bounds.y (bounds.width /. 2.) (bounds.height /. 2.) in
        let ne_bounds = create_bounds (bounds.x +. bounds.width /. 2.) bounds.y (bounds.width /. 2.) (bounds.height /. 2.) in
        let sw_bounds = create_bounds bounds.x (bounds.y +. bounds.height /. 2.) (bounds.width /. 2.) (bounds.height /. 2.) in
        let se_bounds = create_bounds (bounds.x +. bounds.width /. 2.) (bounds.y +. bounds.height /. 2.) (bounds.width /. 2.) (bounds.height /. 2.) in

        if inside_bounds obj nw_bounds then 
          Branch { nw = insert nw obj; ne; sw; se; bounds }
        else if inside_bounds obj ne_bounds then
          Branch { nw; ne = insert ne obj; sw; se; bounds }
        else if inside_bounds obj sw_bounds then
          Branch { nw; ne; sw = insert sw obj; se; bounds }
        else if inside_bounds obj se_bounds then
          Branch { nw; ne; sw; se = insert se obj; bounds }
        else tree

  let rec build_quadtree bricks screen_bounds =
    let ((x_min, y_min), (x_max, y_max)) = screen_bounds in
    let tree_bounds = create_bounds x_min y_min (x_max -. x_min) (y_max -. y_min) in
    let initial_branch = Branch { 
      nw = Empty; 
      ne = Empty; 
      sw = Empty; 
      se = Empty; 
      bounds = tree_bounds 
    } in
    List.fold_left insert initial_branch bricks

  (* let rec query tree ball =
    let (ball_x, ball_y) = Ball.get_position ball in
    let ball_radius = Ball.get_radius ball in
    
    let rec search_node node =
      match node with
      | Empty -> []
      | Leaf objs -> 
          List.filter (fun brick -> 
            let bounds = Brick.get_bounds brick in
            let ((bx1, by1), (bx2, by2)) = bounds in
            
            let x_overlap = 
              ball_x +. ball_radius >= bx1 && 
              ball_x -. ball_radius <= bx2 
            in
            let y_overlap = 
              ball_y +. ball_radius >= by1 && 
              ball_y -. ball_radius <= by2 
            in

            x_overlap && y_overlap
          ) objs
      | Branch { nw; ne; sw; se; bounds } ->
          if ball_x +. ball_radius >= bounds.x && 
             ball_x -. ball_radius <= bounds.x +. bounds.width &&
             ball_y +. ball_radius >= bounds.y && 
             ball_y -. ball_radius <= bounds.y +. bounds.height then
            List.concat_map search_node [nw; ne; sw; se]
          else
            []
    in
    search_node tree *)

    let rec query tree ball =
      let (ball_x, ball_y) = Ball.get_position ball in
      let ball_radius = Ball.get_radius ball in
      
      let rec search_node node =
        match node with
        | Empty -> ([], [])
        | Leaf objs -> 
            List.partition (fun brick -> 
              let bounds = Brick.get_bounds brick in
              let ((bx1, by1), (bx2, by2)) = bounds in
              let x_overlap = ball_x +. ball_radius >= bx1 && ball_x -. ball_radius <= bx2 in
              let y_overlap = ball_y +. ball_radius >= by1 && ball_y -. ball_radius <= by2 in
              x_overlap && y_overlap
            ) objs
        | Branch { nw; ne; sw; se; bounds } ->
            if ball_x +. ball_radius >= bounds.x && 
               ball_x -. ball_radius <= bounds.x +. bounds.width &&
               ball_y +. ball_radius >= bounds.y && 
               ball_y -. ball_radius <= bounds.y +. bounds.height then
              let nw_candidates, nw_non = search_node nw in
              let ne_candidates, ne_non = search_node ne in
              let sw_candidates, sw_non = search_node sw in
              let se_candidates, se_non = search_node se in
              (List.concat [nw_candidates; ne_candidates; sw_candidates; se_candidates],
               List.concat [nw_non; ne_non; sw_non; se_non])
            else
              ([], [])
      in
      search_node tree
end