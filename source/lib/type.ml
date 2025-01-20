

(* type.ml *)
type position = float * float
type velocity = float * float
type dimension = float * float

type etat_racket = {
  pos: position;
  dim: dimension;
  vel: velocity;
}

type etat_balle = {
  pos: position;
  vel: velocity;
  radius: float;
}

