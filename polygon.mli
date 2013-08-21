(** Polygons and bezier curves *)
open Pdfgraphics

val curve_accuracy : float

(** {2 Utilities} *)

(** Transform a given path by a given [Transform.transform] *)
val transform_path : Pdftransform.transform -> path -> path

(** Given a seperation and path, return a list of points of that separation on
the path. *)
val points_on_path : float -> path -> (float * float) list

val bezier_epsilon : float -> fpoint -> fpoint -> fpoint -> fpoint -> bool

(** Calling [bezier_subdivide ~epsilon:e p1 p2 p3 p4] subdivides a bezier curve into list
list of [(fpoint * fpoint)] edges using given flatness [e]. *)
val bezier_subdivide :
  (fpoint -> fpoint -> fpoint -> fpoint -> bool) ->
    fpoint -> fpoint -> fpoint -> fpoint -> (fpoint * fpoint) list

(** Split a bezier segment into two in proportions [t] and [1-t] where [t] is
the given float. *)
val bezier_split : float -> segment -> segment * segment

(** Find the floating-point boundary [(xmin, xmax, ymin, ymax)] of a path. *)
val path_proper_bounds : path -> float * float * float * float

(** Ditto for integer bounds. *) 
val bounds_polygon : path -> int * int * int * int

(** {2 Edge lists } *)
type edge = {x0: int; x1 : int; y0 : int; y1: int}

(** Build an edge list from a path *)
val edgelist_of_path : path -> edge list

val sort_edgelist_maxy_rev : edge list -> edge list

(** {2 Shapes of polygons} *)

(** Calculate shape and minshape of a polygon at the same time. *)
val shapeminshape_polygon : path -> Sprite.shape * Sprite.shape

(** Calculate shape and minshape of an unsorted edge list. *)
val shapeminshape_of_unsorted_edgelist : edge list -> winding_rule -> Sprite.shape * Sprite.shape

(** Calculate the shape or minshape of an unsorted edge list given a winding
rule. *)

(** {2 Sprites of polygons} *)

(** Given a fill, the shape in which to render and a path, render a path. *)
val polygon_sprite : Fill.fill -> Sprite.shape -> path -> Sprite.sprite

(** Same, but give an edge list and winding rule instead of a path. *)
val polygon_sprite_edgelist :
  Fill.fill -> Sprite.shape -> edge list -> winding_rule -> Sprite.sprite

