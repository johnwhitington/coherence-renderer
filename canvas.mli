(** Rectangular matrices of colours *)

(** The type of a canvas. It's an array of rows. *)
type canvas = Colour.colour array array 

(** Return the width of a canvas *)
val canvas_width : canvas -> int

(** Return the height of a canvas *)
val canvas_height : canvas -> int

(** Return width and height together *)
val canvas_dimensions : canvas -> (int * int)

(** [newcanvas w h] builds a canvas of width [w] and height [h] (both > 0). The
values in the canvas are undefined. *)
val newcanvas : int -> int -> canvas

(** The same, but each entry is set to [Colour.clear]. *)
val newcanvasclear : int -> int -> canvas

(** Return a new canvas with the same contents as the given one. There is no
memory sharing. *)
val copycanvas : canvas -> canvas

(** [subcopy src dest x y w h] copies from [src] at position [(x, y)] a block of
width [w] and heigh [h] to [dest] at position [(0, 0)]. Fails if there isn't
room. *)
val subcopy : canvas -> canvas -> int -> int -> int -> int -> unit

val string_of_canvas : canvas -> string

val string_of_canvas_alpha : canvas -> string

val bytes_of_canvas : canvas -> Pdfio.bytes

