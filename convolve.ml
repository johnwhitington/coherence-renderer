(* \chaptertitle{Convolve}{Efficient convolution} *)

(* Algorithmically efficient implementation of convolution, including x-y
separable and everywhere-unit convolutions. Makes no attempt at tiling for
cache coherence. *)
open Pdfutil
open Printf

(* The type of kernels. We have three:
\begin{enumerate}
  \item [UnitValuedKernel]: All kernel values are 1. Perform 1D blur in x and
  y---$O(n^2)$
  \item [XYKernel]: Kernel is separable in x and y (e.g. gaussian). Perform 1D
  blur in x, followed by 1D blur in y---$O(n^3)$
  \item [FullKernel]: Generic square kernel---$O(n^4)$
\end{enumerate}
The width or height of a kernel is $2\times\textit{radius} + 1$. *)

type kernel =
  | FullKernel of (int * int * int array array)  (*r radius, total value, values *)
  | XYKernel of (int * int * int array)  (*r Radius, total value in x / y, x / y values *)
  | UnitKernel of int   (*r Radius *)

(* Debug routine to print a kernel out. *)
let print_kernel = function
  | FullKernel(r, t, v) ->
      printf "Full kernel, radius %i, total %i. Values:\n" r t;
      Array.iter (fun row -> (Array.iter (fun col -> printf "%i, " col) row; printf "\n")) v
  | XYKernel(r, t, v) ->
      printf "XY Separable kernel, radius %i, total %i. Values:\n" r t;
      Array.iter (fun x -> printf "%i, " x) v; printf "\n"
  | UnitKernel(r) ->
      printf "Unit kernel, radius %i\n" r

(* Make a unit kernel of given radius. *)
let mkunit r =
  if r <= 0 then raise (Invalid_argument "Convolve.mkunit") else
    UnitKernel r

(* Make an x-y seperable kernel from a function $f \textrm{\ :\
}\textbf{int}\rightarrow\textbf{int}$ with domain [-r] to [r] *)
let mkxy r f =
  if r <= 0 then raise (Invalid_argument "Convolve.mkxy") else
    let arr = Array.init (2 * r + 1) (fun i -> f (i - r)) in
      XYKernel (r, Array.fold_left ( + ) 0 arr, arr)

(* Make a full kernel from a function $f\textrm{\ :\
}\textbf{int}\rightarrow\textbf{int}\rightarrow\textbf{int}$ with domain [-r]
to [r] in both x and y. *)
let mkfull r f =
  if r <= 0 then raise (Invalid_argument "Convolve.mkfull") else
    let mkrow y = Array.init (2 * r + 1) (fun i -> f (i - r) y) in
      let arr = Array.init (2 * r + 1) (fun i -> mkrow (i - r)) in
        let total = Array.fold_left (Array.fold_left ( + )) 0 arr in
          FullKernel (r, total, arr) 

(* The gaussian in 2D. We introduce a multiplication factor of 4 to reduce
error whilst keeping numbers in a range which will avoid overflow in the
convolution. 4 is arbitrary.*)
let gaussian r x y =
  let sq x = x *. x in
    let g x y = exp ~-.(sq x +. sq y) /. 2. in
      let mul = float (4 * r * r) in
        toint (mul *.  g (float x /. float r) (float y /. float r) +. 0.5)

(* The gaussian in 1D. *)
let gaussian_1d r = fun x -> gaussian r x 0

(* A gaussian kernel for radius r *)
let mkgaussian r = mkxy r (gaussian_1d r)

(* Utility functions for working with colour components. Add, subtract,
add-with-multiply and divide four-tuples. *)
let add4 (w, x, y, z) (w', x', y', z') =
  w := !w + w';
  x := !x + x';
  y := !y + y';
  z := !z + z'

let sub4 (w, x, y, z) (w', x', y', z') =
  w := !w - w';
  x := !x - x';
  y := !y - y';
  z := !z - z'

let add4m (w, x, y, z) (w', x', y', z') m =
  w := !w + w' * m;
  x := !x + x' * m;
  y := !y + y' * m;
  z := !z + z' * m

let div4 (w, x, y, z) d =
  w / d, x / d, y / d, z / d

(* Convolve a point [(x, y)] from [canvas] to [canvas'] with a given kernel. *)
let convolve_point canvas canvas' (radius, total, values) y x =
  let (tr, tg, tb, ta) as totals = ref 0, ref 0, ref 0, ref 0 in
    for y' = y - radius to y + radius do
      for x' = x - radius to x + radius do
        let kx = (x' - (x - radius)) + 1
        and ky = (y' - (y - radius)) + 1 in
          add4m
            totals
            (Colour.rgba_of_colour canvas.(y' - 1).(x' - 1)) values.(kx - 1).(ky - 1)
      done
    done;
    let col = div4 (!tr, !tg, !tb, !ta) total in
      canvas'.(y - 1).(y - 1) <- Colour.colour_of_rgba_tuple col

(* Functions to perform convolution with x-y seperable kernels. First, we
define code common to both horizontal and vertical code. \smallgap *)

(* set [canvas] at [(x, y)] given total value of kernel, and r g b a
accumulators *)
let setcanvas canvas total x y tr tg tb ta =
  let tr = !tr / total and tg = !tg / total
  and tb = !tb / total and ta = !ta / total in
    let tr = min ta tr and tg = min ta tg and tb = min tb tb in
      canvas.(y - 1).(x - 1) <- Colour.colour_of_rgba tr tg tb ta

(* Horizontal 1D convolution, starting at [(x, y)] of length [l]. *)
let xykernel_x canvas canvas' (radius, total, values) (x, y, l) =
  (* For each point in the span *)
  let tr = ref 0 and tg = ref 0 and tb = ref 0 and ta = ref 0 in
    for x' = x to x + l - 1 do
      tr := 0; tg := 0; tb := 0; ta := 0;
      (* For each contributing sample *)
      for x'' = x' - radius to x' + radius do
        let kx = x'' - (x' - radius) + 1 in
          (*i Printf.printf "y - 1 = %i, x'' - 1 = %i, kx - 1 = %i\n" (y - 1) (x'' - 1) (kx - 1); i*)
          let colour = canvas.(y - 1).(x'' - 1)
          and value = values.(kx - 1) in
            tr += Colour.red_of_colour colour * value;
            tg += Colour.green_of_colour colour * value;
            tb += Colour.blue_of_colour colour * value;
            ta += Colour.alpha_of_colour colour * value
      done;
      setcanvas canvas' total x' y tr tg tb ta
    done

(* Vertical convolution, starting at [(x, y)] of length [d]. *)
let xykernel_y canvas canvas' (radius, total, values) (x, y, d) =
  (* For each point in the span *)
  let tr = ref 0 and tg = ref 0 and tb = ref 0 and ta = ref 0 in
    for y' = y to y + d - 1 do
      tr := 0; tg := 0; tb := 0; ta := 0;
      (* For each contributing sample *)
      for y'' = y' - radius to y' + radius do
        let ky = y'' - (y' - radius) + 1 in
          let colour = canvas.(y'' - 1).(x - 1)
          and value = values.(ky - 1) in
            tr += Colour.red_of_colour colour * value;
            tg += Colour.green_of_colour colour * value;
            tb += Colour.blue_of_colour colour * value;
            ta += Colour.alpha_of_colour colour * value
      done;
      setcanvas canvas' total x y' tr tg tb ta
    done

(* Use an incremental technique to do the x-convolution of a unit kernel. *)
let unitkernel_x canvas canvas' radius (x, y, l) =
  (* 1. Calculate the total for the kernel over the first pixel *)
  let (tr, tg, tb, ta) as totals = ref 0, ref 0, ref 0, ref 0 in
    for x' = x - radius to x + radius do
      add4 totals (Colour.rgba_of_colour canvas.(y - 1).(x' - 1))
    done;
    (* 2. Write this pixel out *)
    let col = div4 (!tr, !tg, !tb, !ta) (radius * 2 + 1) in
      canvas'.(y - 1).(x - 1) <-
        Colour.colour_of_rgba_tuple col;
    (* 3. For each other pixel, remove the old left and add the new right *)
    for pixel = x + 1 to x + l - 1 do
      let leftpos = pixel - radius - 1
      and rightpos = pixel + radius in
        let left = Colour.rgba_of_colour canvas.(y - 1).(leftpos - 1)
        and right = Colour.rgba_of_colour canvas.(y - 1).(rightpos - 1) in
          sub4 totals left; add4 totals right;
          let col = div4 (!tr, !tg, !tb, !ta) (radius * 2 + 1) in
            canvas'.(y - 1).(pixel - 1) <-
              Colour.colour_of_rgba_tuple col
    done

(* Use an incremental technique to do the y-convolution of a unit kernel. *)
let unitkernel_y canvas canvas' radius (x, y, d) =
  (* 1. Calculate the total for the kernel over the first pixel *)
  let (tr, tg, tb, ta) as totals = ref 0, ref 0, ref 0, ref 0 in
    for y' = y - radius to y + radius do
      add4 totals (Colour.rgba_of_colour canvas.(y' - 1).(x - 1))
    done;
    (* 2. Write this pixel out *)
    let col = div4 (!tr, !tg, !tb, !ta) (radius * 2 + 1) in
      canvas'.(y - 1).(x - 1) <-
        Colour.colour_of_rgba_tuple col;
    (* 3. For each other pixel, remove the old top and add the new bottom *)
    for pixel = y + 1 to y + d - 1 do
      let toppos = pixel - radius - 1
      and botpos = pixel + radius in
        let top = Colour.rgba_of_colour canvas.(toppos - 1).(x - 1)
        and bot = Colour.rgba_of_colour canvas.(botpos - 1).(x - 1) in
          sub4 totals top; add4 totals bot;
          let col = div4 (!tr, !tg, !tb, !ta) (radius * 2 + 1) in
            canvas'.(pixel - 1).(x - 1) <-
              Colour.colour_of_rgba_tuple col
    done

(* Convolve in [shape] from [canvas] to [canvas'] using [kernel]. *)
let convolve canvas canvas' kernel shape =
  match kernel with
  | FullKernel k ->
      Sprite.shape_iter (convolve_point canvas canvas' k) shape; canvas'
  | XYKernel k ->
      let spans = Sprite.spanlist_of_shape shape
      and spans_vertical = Sprite.depthspanlist_of_shape shape in
        (*i let a, b = Canvas.canvas_dimensions canvas
        and c, d = Canvas.canvas_dimensions canvas' in i*)
        (*i let print_spans ss =
          iter (function (x, y, z) -> Printf.printf "(%i, %i, %i)" x y z) ss
        in
        Printf.printf "XYKernel: Canvas = %i, %i, Canvas' = %i, %i\n" a b c d;
        Printf.printf "Horizontal Spans:\n";
        print_spans spans;
        Printf.printf "Vertical Spans:\n";
        print_spans spans_vertical; i*)
        iter (xykernel_x canvas canvas' k) spans;
        iter (xykernel_y canvas' canvas k) spans_vertical;
        canvas
  | UnitKernel k ->
      let spans = Sprite.spanlist_of_shape shape
      and spans_vertical = Sprite.depthspanlist_of_shape shape in
        iter (unitkernel_x canvas canvas' k) spans;
        iter (unitkernel_y canvas' canvas k) spans_vertical;
        canvas

(* The radius of a given kernel. *)
let radius_of_kernel = function
  FullKernel (r, _, _) | XYKernel (r, _, _) | UnitKernel r -> r
                        
(* Flatten a sprite to a canvas, convolve it, and pick it up again. *)
let convolve_sprite kernel sprite =
  match sprite with
  | Sprite.NullSprite -> Sprite.NullSprite
  | Sprite.Sprite (Sprite.Box (x0, y0, _, _), _) ->
      let radius = radius_of_kernel kernel in
        let in_trans_x = radius * 2 - (x0 - 1)
        and in_trans_y = radius * 2 - (y0 - 1) in
          let out_trans_x = -in_trans_x
          and out_trans_y = -in_trans_y in
            let canvas = Sprite.flatten_sprite (radius * 2) sprite Colour.clear in
              let canvas' = Canvas.copycanvas canvas in
                let shape =
                  Sprite.translate_shape
                    in_trans_x in_trans_y
                    (Sprite.bloat radius radius (Sprite.shape_of_sprite sprite))
                in
                  let out_canvas = convolve canvas canvas' kernel shape in
                    let result = Sprite.pickup shape 1 1 out_canvas in
                      Sprite.translate_sprite out_trans_x out_trans_y result
  | _ -> failwith "convolve_sprite: malformed input"

let ( --- ) = Sprite.shape_difference

(* Flatten a sprite to a canvas, convolve in just the given shape, and pick up
in another given shape. [shape] must be a subset of the shape of [sprite]
bloated, as must [pickup_shape]. *)
let convolve_sprite_in_shape kernel sprite shape pickup_shape =
  (*i flprint "---------------INPUT SHAPE\n";
  flprint (Sprite.string_of_shape shape);
  flprint "---------------SHAPE OF INPUT SPRITE\n";
  flprint (Sprite.string_of_shape (Sprite.shape_of_sprite sprite));
  flprint "---------------PICKUP SHAPE\n";
  flprint (Sprite.string_of_shape pickup_shape); i*)
  let radius = radius_of_kernel kernel in
    (*i assert (radius > 0);
    assert (shape --- (Sprite.bloat (radius * 2 + 1) (radius * 2 + 1) (Sprite.shape_of_sprite sprite)) = Sprite.NullShape);
    assert (pickup_shape --- (Sprite.bloat (radius * 2 + 1) (radius * 2 + 1) (Sprite.shape_of_sprite sprite)) = Sprite.NullShape); i*)
    match sprite with
    | Sprite.NullSprite -> Sprite.NullSprite
    | Sprite.Sprite (Sprite.Box (x0, y0, _, _), _) ->
        (*i Printf.printf "radius is %i\n" radius; i*)
        let in_trans_x = radius * 2 - (x0 - 1)
        and in_trans_y = radius * 2 - (y0 - 1) in
          let out_trans_x = -in_trans_x
          and out_trans_y = -in_trans_y in
            let canvas = Sprite.flatten_sprite (radius * 2) sprite Colour.clear in
              (*i Printf.printf "Canvas width = %i, Canvas height = %i\n" (Canvas.canvas_width canvas) (Canvas.canvas_height canvas); i*)
              let canvas' = Canvas.copycanvas canvas in
                let shape = Sprite.translate_shape in_trans_x in_trans_y shape in
  (*i flprint "---------------TRANSLATED SHAPE\n";
  flprint (Sprite.string_of_shape shape); i*)
                  let out_canvas = convolve canvas canvas' kernel shape in
                    let pickup_shape =
                      Sprite.translate_shape in_trans_x in_trans_y pickup_shape
                    in
                      let result = Sprite.pickup pickup_shape 1 1 out_canvas in
                        Sprite.translate_sprite out_trans_x out_trans_y result
    | _ -> failwith "convolve_sprite: malformed input"
 


