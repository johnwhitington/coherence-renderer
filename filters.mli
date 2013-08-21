(** Example Filters *)
open Render

val hole : geometry -> geometry

val smear : Brush.brushstroke -> geometry

val blur : geometry -> Convolve.kernel -> geometry
 
val monochrome : geometry -> geometry

val minus : geometry -> geometry

val swapdepth : Id.idset -> Id.idset -> geometry -> geometry

val wireframe : geometry -> Shapes.strokespec -> Fill.fill -> geometry

type colourtrans = (float * float) -> Pdftransform.transform

val rgb : geometry -> (colourtrans * colourtrans * colourtrans) -> filterkind -> geometry 

val affine : Pdftransform.transform -> geometry -> geometry

