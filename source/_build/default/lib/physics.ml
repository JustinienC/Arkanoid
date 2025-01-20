module Physics = struct
  let check_collision_aabb point_pos point_size box_pos box_size =
    let px, py = point_pos in
    let bx, by = box_pos in
    let pw, ph = point_size in
    let bw, bh = box_size in
    
    px < bx +. bw &&
    px +. pw > bx &&
    py < by +. bh &&
    py +. ph > by
end