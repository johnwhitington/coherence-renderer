(* \part{Filling Shapes} *)

(* \chaptertitle{Fill}{Simple Fills} *)

(* This module implements plain and graduated fills. Where possible, we ape the
PDF specification. *)

open Printf
open Pdfutil

(* \section{Types} *)

(* \intf A [subspan] is really part of the [sprite] data structure from module
Sprite, but its most basic component [subspancontent] is defined here to avoid
inter-module recursion. A [Run] is a runlength-encoded run of a single colour.
[Samples] is an array of colours.  [Interval] is an array where the real data
starts at some offset, to avoid data copying when taking a portion of a sprite.
*)
type subspancontent  =
  | Run of Colour.colour
  | Samples of Colour.colour array
  | Interval of int * Colour.colour array

(* \intf Fills come in two kinds: A [Plain] one is not altered by translation, a
[Fancy] one is. *)
type fillkind = Plain | Fancy

(* \intf A fill contains:
\begin{itemize}
  \item [fillkind]: Described above.
  \item [fillsingle]: The function which, when given [x] and [y] coordinates,
  returns the value of that pixel under this fill.
  \item [fillspan]: Calculate, starting at a given [x] and [y], a span of
  pixel values.
  \item [filltransform]: Return a new fill, transformed by the given
  operation.
  \item [fillchangecolour]: Apply some simple colour-changing function to
  the fill.
\end{itemize}
*)
type fill =
  {fillkind : fillkind;
  fillsingle : int -> int -> Colour.colour;
  fillspan : int -> int -> int -> subspancontent;
  filltransform : Pdftransform.transform -> fill -> fill;
  fillchangecolour : (Colour.colour -> Colour.colour) -> fill}

(* \intf A dummy plain fill. *)
let rec dummy =
  {fillkind = Plain;
   fillsingle = (fun _ _ -> Colour.clear);
   fillspan = (fun _ _ _ -> Run(Colour.clear));
   filltransform = (fun _ _ -> dummy);
   fillchangecolour = (fun _ -> dummy)}

(* \intf A dummy fancy fill. *)
let rec dummyfancy = {dummy with fillkind = Fancy}

(* \section{Plain fills} *)

(* \intf The plain fill from a given colour. *)
let rec plain colour =
  {fillkind = Plain;
   fillsingle = (fun _ _ -> colour);
   fillspan = (fun _ _ l -> Run colour);
   filltransform = (fun _ fill -> fill);
   fillchangecolour = (fun f -> plain (f colour))}

(* \section{Gradient fills} *)

(* General axial gradient with axis from [(x0, y0)] to [(x1, y1)]. If [ext_s],
the gradient continues beyond the start point, if [ext_e] then beyond the end.
The start and end colour are [cs] and [ce]. If the axis is a point, the fill is
transparent. \smallgap*)

(* \intf Axial gradient. If horizontal, use runs for efficiency. *)
let rec gradient ?(f=ident) (x0, y0) (x1, y1) ext_s ext_e cs ce =
  let lookup x y =
    if x1 = x0 && y1 = y0 then Colour.clear else
      let bottom = sqr (x1 -. x0) +. sqr (y1 -. y0) in
        let x' =
          ((x1 -. x0) *. (x -. x0) +. (y1 -. y0) *. (y -. y0)) /. bottom
        in
          if x' < 0. then
            if ext_s then cs else Colour.clear
          else if x' > 1. then
            if ext_e then ce else Colour.clear
          else
            Colour.dissolve_between ~a:cs ce ~alpha:(255 - toint (x' *. 255.))
  in
    {fillkind = Fancy;
     fillsingle =
       (fun x y -> lookup (float x) (float y));
     fillspan =
       if x0 = x1 then
         fun _ y _ -> Run (lookup 0. (float y))
       else
         (fun x y l ->
           let y = float y in
             Samples
               (Array.init l (fun i -> lookup (float (i + x)) y))); 
     filltransform =
       (fun t _ ->
          let tr = Pdftransform.transform t in
            gradient (tr (x0, y0)) (tr (x1, y1)) ext_s ext_e cs ce);
     fillchangecolour = 
       (fun f -> gradient (x0, y0) (x1, y1) ext_s ext_e (f cs) (f ce))}

(* \intf Radial gradient at [c] with minor radius point at [p], major radius
point [p'], start and end colours [cs] and [ce] and flags [ext_s] and [ext_e],
determinining if points outside the range [p..p'] are coloured.*)
let rec radial ?(f=ident) c p p' ext_s ext_e cs ce =
  let r = distance_between c p
  and r' = distance_between c p' in
    let diff_rr' = r' -. r in
      let lookup x y =
        let d = distance_between c (x, y) in
          if d > r' then
            if ext_e then ce else Colour.clear
          else if d < r then
            if ext_s then cs else Colour.clear
          else
            if diff_rr' = 0.  then cs else (*r d = r = r' *)
              let t = (d -. r) /. diff_rr' in
                Colour.dissolve_between ~a:cs ce ~alpha:(255 - toint (t *. 255.))
  in
    {fillkind = Fancy;
     fillsingle =
       (fun x y -> lookup (float x) (float y));
     fillspan =
       (fun x y l ->
          let y = float y in
            Samples
              (Array.init l (fun i -> lookup (float (i + x)) y)));
     filltransform =
       (fun t _ ->
          let tr = Pdftransform.transform t in
            radial (tr c) (tr p) (tr p') ext_s ext_e cs ce);
     fillchangecolour =
       fun f -> radial c p p' ext_s ext_e (f cs) (f ce)}

