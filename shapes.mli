(** Basic shapes and stroking lines *)

(** The factor by which the radius of a circle is multiplied to find the length
of the bezier control lines when approximating quarter arcs to make circles. *)
val kappa : float

(** Calling [restrict_angle s a] restricts an angle [a] to one of those at [s,
2s, 3s...] returning the chosen one. *)
val restrict_angle : float -> float -> float

(** Calling [circle x y r] builds a path representing a circle at [(x, y)] with
radius [r]. *)
val circle : float -> float -> float -> Pdfgraphics.path

(** Calling [rectangle x y w h] builds a path representing a rectangle with top
left [(x, y)], width [w] and height [h]. *)
val rectangle : float -> float -> float -> float -> Pdfgraphics.path

(** Standard PDF line caps. *)
type cap = ButtCap | RoundCap | ProjectingCap

(** Standard PDF line joins. (mitre limit?) *)
type join = RoundJoin | MitredJoin | BevelJoin

(** A stroke specification has a start cap, join style, end cap, mitre limit and width. *)
type strokespec =
  {startcap : cap;
   join : join;
   endcap : cap;
   mitrelimit : float;
   linewidth : float}

(** [rotation c p p'] calculates the rotation from [p] to [p'] about [c] with the
shorter arc-length. When arc-lengths are equal, the result may be either. *)
val rotation : (float * float) -> (float * float) -> (float * float) -> float

(** Stroke a path, yielding an edge list. *)
val strokepath : strokespec -> Pdfgraphics.path -> Polygon.edge list

(** Find the xmin, xmax, ymin, ymax bounds of a path, given a stroke specification. *) 
val bounds_stroke : Pdfgraphics.path -> strokespec -> int * int * int * int

