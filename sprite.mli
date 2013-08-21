(** Sparse representation of rasters *)

(** If this is set, we check every sprite and shape on ingress to and egress from key
routines. Default: false. *)
val debug_spritecheck : bool ref

(** {2 Types} *)

(** Shapes and sprites have a bounding box [(min_x, min_y, max_x, max_y)]. When
one has not yet been calculated, the value [NoBounds] is used instead.  This
should never be seen externally. *)
type bounds =
  | NoBounds
  | Box of int * int * int * int

type subspan = int * Fill.subspancontent

type span = int * int * subspan list

type spanline = span list

type vspan = int * int * spanline list
            
type sprite =
  | NullSprite
  | Sprite of bounds * vspan list

(** The sprite data structure. A subspan is the pair of its length and its
content. A span is a triple of start x-coordintate, length and list of
subspans. A spanline is a list of spans, a vspan a triple of start
y-coordinate, length and spanline list. A sprite is either the null sprite, or
a set of bounds and a list of vspans. None of the lists in the data structure
may be null. *)

type shapespan = int * int

type shapespanline = shapespan list

type shapevspan = int * int * shapespanline list

type shape =
  | NullShape
  | Shape of bounds * shapevspan list

(** Shapes are the same as sprites, but with no span content. *)
val string_of_shape : shape -> string

(** Cardinality of sprite *)
val sprite_card : sprite -> int

(** Cardinatlity of shape *)
val shape_card : shape -> int

(** Check a sprite is well-formed. *)
val spritecheck : sprite -> bool

(** Check a shape is well-formed. *)
val shapecheck : shape -> bool

(** The amount of memory a shape takes up *)
val shapesize : shape -> int

(** The amount of memory a sprite takes up *)
val spritesize : sprite -> int

(** {2 Basic operations} *)

(** Given a function taking a y coordinate and x coordinate and a shape,
evaluate that function once for each pixel in the shape. *)
val shape_iter : (int -> int -> unit) -> shape -> unit

val shapespan_iter : int -> int -> int -> int -> (int -> int -> int -> unit) -> shape -> unit

(** Similar, but the function also takes a colour -- the value of the pixel. *)
val sprite_iter : (int -> int -> Colour.colour -> unit) -> sprite -> unit

(** Map a shape to a sprite, using a function from shape spans to sprite spans.
The function takes the y coordinate of the span. *)
val map_shape : (shapespan -> int -> span) -> shape -> sprite

val map_shape_calling : (int -> unit) -> (shapespan -> int -> span) -> shape -> sprite

val shape_difference : shape -> shape -> shape
val ( --- ) : shape -> shape -> shape
(** The difference of two shapes. *)

val shape_intersection : shape -> shape -> shape
val ( &&& ) : shape -> shape -> shape
(** The intersection of two shapes. *)

val shape_union : shape -> shape -> shape
val ( ||| ) : shape -> shape -> shape
(** The union of two shapes. *)

(** Do two shapes have any pixels in common? *)
val shape_intersects : shape -> shape -> bool

(** Return a sprite representing the portion of the input sprite in the input
shape. Fails if the input shape is not a subset of the input sprite. *)
val portion : sprite -> shape -> sprite

(** Translate a shape in x and y. *)
val translate_shape : int -> int -> shape -> shape

(** Translate a sprite in x and y. *)
val translate_sprite : int -> int -> sprite -> sprite

(** Given a function from colours to colours, map a sprite. *)
val sprite_map : (Colour.colour -> Colour.colour) -> sprite -> sprite

(** Same, but depends upon y and x coordinates too. *)
val map_coords : (int -> int -> Colour.colour -> Colour.colour) -> sprite -> sprite

(** Give the shape with the same coverage as a sprite. *)
val shape_of_sprite : sprite -> shape

(** Set memebership on sprites. *)
val point_in_shape : shape -> (int * int) -> bool

(** [box x y w h] builds a shape containing all the pixels (px, py) with x <= px
 < x + w and y <= py < y + h *)
val box : int -> int -> int -> int -> shape

(** Make a sprite by filling a shape *)
val fillshape : shape -> Fill.fill -> sprite

(** Return a list of all the spans in the shape (x, y, l) *)
val spanlist_of_shape : shape -> (int * int * int) list

(** Calling [bloat x y shape] convolves the shape with a rectangle
2x+1 by 2y+1. *)
val bloat : int -> int -> shape -> shape

(** Calling [bloat x y shape] gives a shape which, when convolved by 'bloat'
would give the input shape. *)
val erode : int -> int -> shape -> shape

(** {2 Sprites and Canvases} *)

(** Flatten a sprite to a canvas, using the base colour and border width given *)
val flatten_sprite : int -> sprite -> Colour.colour -> Canvas.canvas

(** Pick up a span from a canvas at (x, y, l) *)
val pickup_span : int -> int -> int -> Canvas.canvas -> Colour.colour array

(** Pickup in a shape at a given (x, y) offset in a canvas *)
val pickup : shape -> int -> int -> Canvas.canvas -> sprite

(** {2 Debug routines } *)

(* Debug routines. Add a sprite to the current output list *)
val add_debug_sprite : ?dx:int -> ?dy:int -> sprite -> unit

(* Puts in a shape, in red. *)
val add_debug_shape : ?dx:int -> ?dy:int -> shape -> unit

(* Flush the current sprite list to a new page, given a caption. *)
val write_debug_page : string -> unit

(* Send the PDF to disc. *)
val write_debug_pdf : string -> unit

(**/**)

(* These only used by Convolve *)
val depthspanlist_of_shape : shape -> (int * int * int) list

(* These only use by Brush *)
val spans_accumulate : shapespan list -> shapespan list -> shapespan list

val boxshape : shape -> shape

val caf :
  Colour.compositing_operator -> (Colour.colour -> bool) ->
    sprite -> sprite -> (sprite * shape)

(* These only used by Polygon *)
val shape_spanline_union :
  int ref -> int ref -> shapespanline -> shapespanline -> shapespanline

val shape_spanline_difference :
  int ref -> int ref -> shapespanline -> shapespanline -> shapespanline

val vspan_accumulate_spanlines :
  (int * int * 'a list list) list -> int -> 'a list list -> (int * int * 'a list list) list

val vspan_reversespanlines :
  (int * int * 'a list) -> (int * int * 'a list)

type overlap =
  | BthenA
  | AthenB
  | AoverlapB of int * int
  | AabutB
  | BoverlapA of int * int
  | BabutA
  | AinsideB of int * int
  | BinsideA of int * int
  | ABsame
  | ArightB of int * int
  | BrightA of int * int

val overlap : shapespan -> shapespan -> overlap

