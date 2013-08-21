(** General convolution on sprites *)

(** The type of a convolution kernel. *)
type kernel

(** Debug printing of a kernel. *)
val print_kernel : kernel -> unit

(** Make a everywhere-unit kernel of a given radius. *)
val mkunit : int -> kernel

(** Make an x-y seperable kernel of a given radius [r] by supplying a generator
function for values [-r] to [r]. *)
val mkxy : int -> (int -> int) -> kernel

(** Similarly, a full kernel of a given radius and a generator function with
domain [-r] to [r] in both x (supplied first) and y. *)
val mkfull : int -> (int -> int -> int) -> kernel

(** Make a gaussian blur of a given radius. *)
val mkgaussian : int -> kernel

(** Project the radius from a kernel. *)
val radius_of_kernel : kernel -> int

(** Calling [convolve canvas canvas' kernel shape] convolves all the points in
 [shape] gathering data from [canvas] and writing to [canvas']. *)
val convolve :
  Canvas.canvas -> Canvas.canvas -> kernel -> Sprite.shape ->
  Canvas.canvas

(** Convolve a sprite by a kernel. The output sprite has the same shape as the
input sprite. *)
val convolve_sprite : kernel -> Sprite.sprite -> Sprite.sprite

(** Calling [convolve_sprite_in_shape kernel sprite shape pickup_shape] flattens
a sprite to a canvas, convolves in just [shape], and returns data in
[pickup_shape]. [shape] must be a subset of the shape of [sprite]. *)
val convolve_sprite_in_shape :
  kernel -> Sprite.sprite -> Sprite.shape -> Sprite.shape -> Sprite.sprite

