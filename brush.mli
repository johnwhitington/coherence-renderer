(** Brushes and brushstrokes *)

(** The type of brushes. *)
type brush

(** A stroke of a brush along a path. *)
type brushstroke = brush * Pdfgraphics.path

(** The width and height of a brush. *)
val sizeof_brush : brush -> (int * int)

(** Make a round gaussian brush of a given radius (+ve) and opacity (0..1) *)
val mkround : float -> float -> brush

(** Make a dummy copy of a brushstroke. A dummy has dimensions but renders
nothing. *)
val mkdummy : brushstroke -> brushstroke

(** Find the shape and minshape of a brushstroke. *)
val shape_of_brushstroke : brushstroke -> Sprite.shape * Sprite.shape

(** Find the sprite of a brushstoke filled with a given fill rendered in a given
shape. *)
val sprite_of_brushstroke : brushstroke -> Fill.fill -> Sprite.shape -> Sprite.sprite

(** Smear a sprite using a given brush. *) 
val smear : Sprite.sprite -> brushstroke -> Sprite.sprite

(** Transform a brushstroke. *)
val transform_brushstroke : Pdftransform.transform -> brushstroke -> brushstroke

(** Find the minx, maxx, miny, maxy of a brushstroke. *) 
val bounds_brushstroke : brushstroke -> int * int * int * int

