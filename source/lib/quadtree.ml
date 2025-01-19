(* quadtree.ml *)
module Quadtree = struct
  type bounds = {
    x: float;
    y: float;
    width: float;
    height: float;
  }

  type 'a quadtree = {
    bounds: bounds;
    objects: 'a list;
    nodes: 'a quadtree array option;
    max_objects: int;
    max_levels: int;
    level: int;
  }

  let create bounds max_objects max_levels = {
    bounds;
    objects = [];
    nodes = None;
    max_objects;
    max_levels;
    level = 0;
  }

  let split qt =
    let subWidth = qt.bounds.width /. 2. in
    let subHeight = qt.bounds.height /. 2. in
    let x = qt.bounds.x in
    let y = qt.bounds.y in
    
    [|
      (* Top right *)
      {qt with 
        bounds = {x = x +. subWidth; y = y +. subHeight; 
                 width = subWidth; height = subHeight};
        level = qt.level + 1;
        objects = [];
        nodes = None};
      (* Top left *)
      {qt with 
        bounds = {x; y = y +. subHeight; 
                 width = subWidth; height = subHeight};
        level = qt.level + 1;
        objects = [];
        nodes = None};
      (* Bottom left *)
      {qt with 
        bounds = {x; y; 
                 width = subWidth; height = subHeight};
        level = qt.level + 1;
        objects = [];
        nodes = None};
      (* Bottom right *)
      {qt with 
        bounds = {x = x +. subWidth; y; 
                 width = subWidth; height = subHeight};
        level = qt.level + 1;
        objects = [];
        nodes = None}
    |]

  let get_index bounds object_bounds =
    let vertical_midpoint = bounds.x +. (bounds.width /. 2.) in
    let horizontal_midpoint = bounds.y +. (bounds.height /. 2.) in
    
    let top = object_bounds.y +. object_bounds.height > horizontal_midpoint in
    let bottom = object_bounds.y < horizontal_midpoint in
    let left = object_bounds.x < vertical_midpoint in
    let right = object_bounds.x +. object_bounds.width > vertical_midpoint in
    
    match (top, bottom, left, right) with
    | (true, false, false, true) -> Some 0  (* Top right *)
    | (true, false, true, false) -> Some 1  (* Top left *)
    | (false, true, true, false) -> Some 2  (* Bottom left *)
    | (false, true, false, true) -> Some 3  (* Bottom right *)
    | _ -> None  (* Object spans multiple quadrants *)

  let rec insert obj get_bounds qt =
    match qt.nodes with
    | Some nodes ->
        (* Si on a des sous-nœuds, essayer d'insérer dans l'un d'eux *)
        let obj_bounds = get_bounds obj in
        (match get_index qt.bounds obj_bounds with
         | Some i -> 
             let new_nodes = Array.copy nodes in
             new_nodes.(i) <- insert obj get_bounds nodes.(i);
             {qt with nodes = Some new_nodes}
         | None -> 
             {qt with objects = obj :: qt.objects})
    | None ->
        (* Pas de sous-nœuds *)
        if List.length qt.objects < qt.max_objects || qt.level >= qt.max_levels then
          {qt with objects = obj :: qt.objects}
        else begin
          (* Créer des sous-nœuds et redistribuer les objets *)
          let nodes = split qt in
          let qt' = {qt with nodes = Some nodes; objects = []} in
          let qt'' = List.fold_left 
            (fun acc obj -> insert obj get_bounds acc)
            qt'
            (obj :: qt.objects) in
          qt''
        end

  let rec retrieve target_bounds qt =
    let result = ref qt.objects in
    
    (match qt.nodes with
     | Some nodes ->
         (match get_index qt.bounds target_bounds with
          | Some i -> result := List.append !result (retrieve target_bounds nodes.(i))
          | None -> 
              Array.iter 
                (fun node -> result := List.append !result (retrieve target_bounds node))
                nodes)
     | None -> ());
    
    !result
end