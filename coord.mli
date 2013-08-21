(** Integer coordinates and conversions *)

(** A pixel bin. *)
type pixbin = int

(** A subpixel bin. *)
type subbin = int

(** The interpixel spacing (number of subpixel bins in a pixel bin). *)
val ipspacing : int

(** Half that value *)
val halfips : int

val left_of_pix : pixbin -> subbin
val top_of_pix : pixbin -> subbin
(** The smallest subpixel bin in a pixel bin. *)

val right_of_pix : pixbin -> subbin
val bottom_of_pix : pixbin -> subbin
(** The largest subpixel bin in a pixel bin. *)

(** The pixel bin in which a subpixel lies. *)
val pix_of_sub : subbin -> pixbin

(** The subpixel bin in which a float lies. *)
val sub_of_float : float -> subbin

(** The pixel bin in which a float lies. *)
val pix_of_float : float -> pixbin

(** Convert a subpixel bin to a real value. *)
val float_of_sub : subbin -> float

