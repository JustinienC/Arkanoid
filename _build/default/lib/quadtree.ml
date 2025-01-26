open Brick
open Ball

module Quadtree = struct

  (*
   type bounds
   Représente les limites d'un quadrant 
   Champs :
   - x : float, coordonnée x du coin supérieur gauche
   - y : float, coordonnée y du coin supérieur gauche
   - width : float, largeur du quadrant (> 0)
   - height : float, hauteur du quadrant (> 0)
*)

  type bounds = {
    x: float;
    y: float;
    width: float;
    height: float;
  }

  (*
   type 'a node
   Représente un nœud de l'arbre quaternaire
   Constructeurs :
   - Empty : nœud vide
   - Leaf : contient une liste d'objets
   - Branch : nœud interne avec 4 sous-arbres (nw,ne,sw,se) et leurs limites
*)
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


  (*
   create_bounds : float -> float -> float -> float -> bounds
   crée un nouveau rectangle de limite
   Paramètre x : float, coordonnée x du coin supérieur gauche
   Paramètre y : float, coordonnée y du coin supérieur gauche
   Paramètre width : float, largeur du rectangle (> 0)
   Paramètre height : float, hauteur du rectangle (> 0)
   Résultat : nouvelle instance de bounds
*)

  let create_bounds x y width height = 
    { x; y; width; height }


  (*
   inside_bounds : Brick.t -> bounds -> bool
   vérifie si une brique est dans les limites d'un quadrant
   Paramètre obj : Brick.t, la brique à tester
   Paramètre bounds : bounds, les limites du quadrant
   Résultat : true si la brique est dans les limites, false sinon
  *)

  let inside_bounds obj bounds =
    let (obj_x, obj_y) = Brick.get_position obj in
    obj_x >= bounds.x && 
    obj_x < bounds.x +. bounds.width &&
    obj_y >= bounds.y && 
    obj_y < bounds.y +. bounds.height

    (*
   insert : 'a node -> 'a -> 'a node
   insère un objet dans l'arbre quaternaire
   Paramètre tree : 'a node, l'arbre dans lequel insérer
   Paramètre obj : 'a, l'objet à insérer
   Résultat : nouvel arbre contenant l'objet
   Comportement :
   - Si nœud Empty : crée une feuille avec l'objet
   - Si Leaf : ajoute l'objet à la liste
   - Si Branch : 
     * Détermine le quadrant approprié (nw,ne,sw,se)
     * Insère récursivement dans ce quadrant
     * Si aucun quadrant ne convient, laisse l'arbre inchangé
   Invariant : maintient la structure spatiale de l'arbre quaternaire
*)
  let rec insert tree obj =
    match tree with
     (* Cas 1: Arbre vide - crée une nouvelle feuille *)
    | Empty -> Leaf [obj]
    (* Cas 2: Feuille - ajoute l'objet à la liste existante *)
    | Leaf objs -> Leaf (obj :: objs)
    (* Cas 3: Nœud interne - détermine le quadrant approprié *)
    | Branch { nw; ne; sw; se; bounds } ->
        (* Calcul des limites des quatre quadrants *)
        let nw_bounds = create_bounds bounds.x bounds.y (bounds.width /. 2.) (bounds.height /. 2.) in
        let ne_bounds = create_bounds (bounds.x +. bounds.width /. 2.) bounds.y (bounds.width /. 2.) (bounds.height /. 2.) in
        let sw_bounds = create_bounds bounds.x (bounds.y +. bounds.height /. 2.) (bounds.width /. 2.) (bounds.height /. 2.) in
        let se_bounds = create_bounds (bounds.x +. bounds.width /. 2.) (bounds.y +. bounds.height /. 2.) (bounds.width /. 2.) (bounds.height /. 2.) in

        (* Insertion récursive dans le quadrant approprié *)
        if inside_bounds obj nw_bounds then 
          Branch { nw = insert nw obj; ne; sw; se; bounds }
        else if inside_bounds obj ne_bounds then
          Branch { nw; ne = insert ne obj; sw; se; bounds }
        else if inside_bounds obj sw_bounds then
          Branch { nw; ne; sw = insert sw obj; se; bounds }
        else if inside_bounds obj se_bounds then
          Branch { nw; ne; sw; se = insert se obj; bounds }
        (* Si l'objet n'appartient à aucun quadrant, retourne l'arbre inchangé *)
        else tree

  (*
   build_quadtree : Brick.t list -> (float*float)*(float*float) -> Brick.t node
   construit un arbre quaternaire à partir d'une liste de briques
   Paramètre bricks : Brick.t list, liste des briques à insérer
   Paramètre screen_bounds : limites de l'écran ((x_min,y_min),(x_max,y_max))
   Résultat : arbre quaternaire contenant toutes les briques
*)
  let rec build_quadtree bricks screen_bounds =
     (* Extraction des limites de l'écran *)
    let ((x_min, y_min), (x_max, y_max)) = screen_bounds in
     (* Création des limites initiales de l'arbre *)
    let tree_bounds = create_bounds x_min y_min (x_max -. x_min) (y_max -. y_min) in
     (* Création d'un nœud racine vide *)
    let initial_branch = Branch { 
      nw = Empty; 
      ne = Empty; 
      sw = Empty; 
      se = Empty; 
      bounds = tree_bounds 
    } in
    (* Insertion successive de chaque brique *)
    List.fold_left insert initial_branch bricks


    (*
   query : 'a node -> Ball.t -> 'a list * 'a list
   recherche les objets en collision avec une balle
   Paramètre tree : 'a node, l'arbre quaternaire à interroger
   Paramètre ball : Ball.t, la balle dont on cherche les collisions
   Résultat : (colliding,non_colliding), partition des objets selon leur collision
   avec la balle
*)
  let rec query tree ball =
      let (ball_x, ball_y) = Ball.get_position ball in
      let ball_radius = Ball.get_radius ball in
      
      let rec search_node node =
        match node with
        | Empty -> ([], [])
        | Leaf objs -> 
            (* Partition des objets selon leur collision avec la balle *)
            List.partition (fun brick -> 
              let bounds = Brick.get_bounds brick in
              let ((bx1, by1), (bx2, by2)) = bounds in
              (* Test de chevauchement hum d'entrevement sur les axes x et y *)
              let x_overlap = ball_x +. ball_radius >= bx1 && ball_x -. ball_radius <= bx2 in
              let y_overlap = ball_y +. ball_radius >= by1 && ball_y -. ball_radius <= by2 in
              x_overlap && y_overlap
            ) objs
        | Branch { nw; ne; sw; se; bounds } ->
            (* Test si la balle intersecte ce nœud *)
            if ball_x +. ball_radius >= bounds.x && 
               ball_x -. ball_radius <= bounds.x +. bounds.width &&
               ball_y +. ball_radius >= bounds.y && 
               ball_y -. ball_radius <= bounds.y +. bounds.height then
              (* Recherche récursive dans chaque quadrant *)
              let nw_candidates, nw_non = search_node nw in
              let ne_candidates, ne_non = search_node ne in
              let sw_candidates, sw_non = search_node sw in
              let se_candidates, se_non = search_node se in
               (* Concaténation des résultats *)
              (List.concat [nw_candidates; ne_candidates; sw_candidates; se_candidates],
               List.concat [nw_non; ne_non; sw_non; se_non])
            else
              ([], [])
      in
      search_node tree
end