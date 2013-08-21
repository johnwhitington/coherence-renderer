(** Filling primitives *)

(** The basic buiding-block of the [sprite] data structure from module [Sprite],
defined here to avoid inter-module recursion. A [Run] represents a horizontal
run of one or more pixels of the same colour. [Samples] is an array of colours
representing a run of the implied length. [Interval] is an array similar to
samples, but the data is stored from some given offset within that array, rather
than from the beginning. *)
type subspancontent  =
  | Run of Colour.colour
  | Samples of Colour.colour array
  | Interval of int * Colour.colour array

(** A [Plain] fill is position-independent (is not altered by translation). A
[Fancy] one may be. *)
type fillkind = Plain | Fancy

(** A fill. [fillsingle] is the function which, when given [x] and [y] coordinates,
  returns the value of that pixel under this fill. [fillspan y x l] calculates,
  starting at a given [x] and [y], a span of pixel values of a given length [l].
  [filltransform]: Return a new fill, the same as the current one, but transformed by 
  the given [Transform.transform]. [fillchangecolour] applies some simple
  colour-changing function to the fill.
*)
type fill =
  {fillkind : fillkind;
  fillsingle : int -> int -> Colour.colour;
  fillspan : int -> int -> int -> subspancontent;
  filltransform : Pdftransform.transform -> fill -> fill;
  fillchangecolour : (Colour.colour -> Colour.colour) -> fill}

(** A dummy plain fill. *)
val dummy : fill

(** A dummy fancy fill. *)
val dummyfancy : fill

(** The plain fill of a given colour. *)
val plain : Colour.colour -> fill

(** Gradient fill. [gradient (x0, y0) (x1, y1) ext_s ext_e cs ce] builds an
axial gradient from (x0, y0) colour cs to (x1, y1) colour ce. If [ext_s] then the
start colour continues before the start. If [ext_e] then the end colour
continues after the end. Otherwise, these portions are transparent. *)
val gradient :
  ?f:(float -> float) -> (float * float) -> (float * float) -> bool -> bool ->
  Colour.colour -> Colour.colour -> fill

(** Radial fill. [radial (cx, cy) (px, py) (px', py') ext_s ext_e cs ce] builds
a radial gradient fill with centre [(cx, cy)], colour [cs] at point [(px, py)]
and colour [ce] at [(px', py')]. [ext_s] and [ext_e] behave as for [gradient].
*)
val radial :
  ?f:(float -> float) -> (float * float) -> (float * float) -> (float * float) -> bool -> bool ->
  Colour.colour -> Colour.colour -> fill

