(** Representation of colours *)

(** {2 Types and conversions} *)

(** A colour is stored bits 0..30 of a caml integer, regardless of the
bit-depth of the machine. All channel values mentioned below are premultiplied
(associated) alpha, unless otherwise noted. *)
type colour

(** [colour_of_rgba r g b a] builds a colour from the values, which are on
[0..255] *)
val colour_of_rgba : int -> int -> int -> int -> colour

(** Floating point version. Inputs between 0 and 1 *)
val colour_of_rgba_float : float -> float -> float -> float -> colour

(** The same, for tuple input *)
val colour_of_rgba_tuple : int * int * int * int -> colour

(** The same, where all channels have the same value. *)
val colour_of_channel : int -> colour

val red_of_colour : colour -> int

val green_of_colour : colour -> int

val blue_of_colour : colour -> int

val alpha_of_colour : colour -> int

val rgba_of_colour : colour -> int * int * int * int
(** Find the components [r, g, b, a] of a colour *)

val red_channel : colour -> colour

val green_channel : colour -> colour

val blue_channel : colour -> colour
(** Remove all but one colour channel from a colour, retaining the alpha *)

(** Premultiply a colour *)
val premul : int -> int -> int -> int -> (int * int * int * int)

(** Return the red, green and blue components, unpremultiplied. *)
val unpremul_rgb : colour -> int * int * int

val unpremul_components : colour -> int * int * int * int

(** Predicate on opaqueness of a colour. *)
val opaque : colour -> bool

(** Build a monochrome by replacing r, g, b by their average. Alpha unaltered.  *)
val monochrome : colour -> colour

(** {2 Compositing} *)

(** A compositing operator takes two colours and yields a third. *)
type compositing_operator = colour -> colour -> colour

(** Porter/Duff over operator. *)
val over : compositing_operator

(** Same, but just does the alpha channel ignoring the rest of the colour *)
val alpha_over : compositing_operator

(** Porter/Duff plus operator. Assumes inputs already reduced to remove
overflow possibility *)
val pd_plus : colour -> colour -> colour

(** Porter/Duff dissolve operator. *)
val dissolve : colour -> delta:int -> colour

(** Dissolve between [a] and [b] by [alpha]. *)
val dissolve_between : a:colour -> colour -> alpha:int -> colour

(** An exception for [nocover] below. *)
exception Nocover

(** Raises [Nocover] if ever called. *)
val nocover : colour -> colour -> colour

(** {2 Colours} *)

(** The transparent colour. *)
val clear : colour

val aliceblue : colour 
val antiquewhite : colour 
val aqua : colour 
val aquamarine : colour 
val azure : colour 
val beige : colour 
val bisque : colour 
val black : colour 
val blanchedalmond : colour 
val blue : colour 
val blueviolet : colour 
val brown : colour 
val burlywood : colour 
val cadetblue : colour 
val chartreuse : colour 
val chocolate : colour 
val coral : colour 
val cornflowerblue : colour 
val cornsilk : colour 
val crimson : colour 
val cyan : colour 
val darkblue : colour 
val darkcyan : colour 
val darkgoldenrod : colour 
val darkgray : colour 
val darkgreen : colour 
val darkgrey : colour 
val darkkhaki : colour 
val darkmagenta : colour 
val darkolivegreen : colour 
val darkorange : colour 
val darkorchid : colour 
val darkred : colour 
val darksalmon : colour 
val darkseagreen : colour 
val darkslateblue : colour 
val darkslategray : colour 
val darkslategrey : colour 
val darkturquoise : colour 
val darkviolet : colour 
val deeppink : colour 
val deepskyblue : colour 
val dimgray : colour 
val dimgrey : colour 
val dodgerblue : colour 
val firebrick : colour 
val floralwhite : colour 
val forestgreen : colour 
val fuchsia : colour 
val gainsboro : colour 
val ghostwhite : colour 
val gold : colour 
val goldenrod : colour 
val gray : colour 
val grey : colour 
val green : colour 
val greenyellow : colour 
val honeydew : colour 
val hotpink : colour 
val indianred : colour 
val indigo : colour 
val ivory : colour 
val khaki : colour 
val lavender : colour 
val lavenderblush : colour 
val lawngreen : colour 
val lemonchiffon : colour 
val lightblue : colour 
val lightcoral : colour 
val lightcyan : colour 
val lightgoldenrodyellow : colour 
val lightgray : colour 
val lightgreen : colour 
val lightgrey : colour 
val lightpink : colour 
val lightsalmon : colour 
val lightseagreen : colour 
val lightskyblue : colour 
val lightslategray : colour 
val lightslategrey : colour 
val lightsteelblue : colour 
val lightyellow : colour 
val lime : colour 
val limegreen : colour 
val linen : colour 
val magenta : colour 
val maroon : colour 
val mediumaquamarine : colour 
val mediumblue : colour 
val mediumorchid : colour 
val mediumpurple : colour 
val mediumseagreen : colour 
val mediumslateblue : colour 
val mediumspringgreen : colour 
val mediumturquoise : colour 
val mediumvioletred : colour 
val midnightblue : colour 
val mintcream : colour 
val mistyrose : colour 
val moccasin : colour 
val navajowhite : colour 
val navy : colour 
val oldlace : colour 
val olive : colour 
val olivedrab : colour 
val orange : colour 
val orangered : colour 
val orchid : colour 
val palegoldenrod : colour 
val palegreen : colour 
val paleturquoise : colour 
val palevioletred : colour 
val papayawhip : colour 
val peachpuff : colour 
val peru : colour 
val pink : colour 
val plum : colour 
val powderblue : colour 
val purple : colour 
val red : colour 
val rosybrown : colour 
val royalblue : colour 
val saddlebrown : colour 
val salmon : colour 
val sandybrown : colour 
val seagreen : colour 
val seashell : colour 
val sienna : colour 
val silver : colour 
val skyblue : colour 
val slateblue : colour 
val slategray : colour 
val slategrey : colour 
val snow : colour 
val springgreen : colour 
val steelblue : colour 
val tan : colour 
val teal : colour 
val thistle : colour 
val tomato : colour 
val turquoise : colour 
val violet : colour 
val wheat : colour 
val white : colour 
val whitesmoke : colour 
val yellow : colour 
val yellowgreen : colour  

