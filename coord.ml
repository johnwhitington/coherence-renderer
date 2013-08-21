(* \chaptertitle{Coord}{Cartesian coordinates in 2D} *)

(* This module defines the integer coordinate system used in the polygon
rasterizer and any other place where we wish to use positioning more accurate
than whole numbers, but without resorting to floating point. The model consists
of \emph{pixel bins} and \emph{subpixel bins} defined thus (taking 64 subpixels
to one pixel): *)
(*i \diagram{papers/pixmodel} i*)
(* Notice how subpixel bins never straddle pixel bins. This means that any
subpixel bin belongs unequivocally to a pixel bin. Thus, all boundary decisions
are dealt with on ingress to the coordinate system, avoiding special cases in
higher-level code. *)
open Pdfutil

(* \intf First, we define two type abbreviations for pixel and
subpixel bins for interface readability. *)
type pixbin = int
and subbin = int

(* \intf We define the \emph{interpixel spacing} --- the number of subpixel bins
to a pixel bin. This must be a power of two, since we wish to be able to
subdivide it easily. Minimum sensible value is 2.*)
let ipspacing = 32

(* \intf The value equal to half the interpixel spacing is used in lots of place
s, so for consistency of naming, we define it here. *)
let halfips = ipspacing / 2

(* We define floating point versions of the two preceeding constants. *)
let ipspacing_fl = float ipspacing
and halfips_fl = float halfips

(* \intf The right-hand subpixel bin in a pixel bin. *)
let right_of_pix p = p * ipspacing

(* \intf The left-hand subpixel bin in a pixel bin. *)
let left_of_pix p = right_of_pix p - ipspacing + 1

(* \intf Analogous functions for top and bottom subpixel bins. *)
let top_of_pix = left_of_pix
and bottom_of_pix = right_of_pix

(* \intf The pixel bin in which the subpixel bin [n] lies *)
let pix_of_sub n = (n + ipspacing - 1) / ipspacing

(* \intf The subpixel bin in which the float [f] lies *)
let sub_of_float f = toint (ceil (f *. ipspacing_fl -. halfips_fl))

(* \intf The pixel bin in which the float [f] lies *)
let pix_of_float f = pix_of_sub (sub_of_float f)

(* \intf The floating point number representing the pixel in which a subpixel
lies. *)
let float_of_sub s = float (pix_of_sub s)

