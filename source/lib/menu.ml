open Graphics

let show_victory () =
  close_graph ();
  open_graph " 300x150"; (* Ouvre une fenêtre de 300x150 pixels *)
  clear_graph ();
  set_window_title "Victoire !";
  moveto 80 75; (* Position du texte *)
  set_color blue;
  draw_string "Félicitations, vous avez gagné !";;

let show_defeat () =
  close_graph ();
  open_graph " 300x150"; (* Ouvre une fenêtre de 300x150 pixels *)
  clear_graph();
  set_window_title "Perdue !";
  moveto 80 75; (* Position du texte *)
  set_color blue;
  draw_string "Dommage, vous avez perdu !";;