(* \part{Core Renderer} *)

(* \chaptertitle{Sprite}{Sparse representation of rasters} *)

(* These routines implement a spatial data structure for shapes and sprites. *)
open Pdfutil
open Pdfio

(* We open [Fill] here to bring the constructors of [Fill.subspancontent] up to
top level. The coding convention, however, is to use full dotted notation for
other parts of the Fill module. *)
open Fill

(* If this is set, we check every sprite on ingress to and egress from key
routines. *)
let debug_spritecheck = ref true

(* \section{Data structures} *)

(* Shapes and sprites have a bounding box [(min_x, min_y, max_x, max_y)]. When
one has not yet been calculated, the value [NoBounds] is used instead.  This
should never be seen externally. *)
type bounds =
  | NoBounds
  | Box of int * int * int * int
            
(* The sprite data structure. A subspan is the pair of its length and its
content. A span is a triple of start x-coordintate, length and list of
subspans. A spanline is a list of spans, a vspan a triple of start
y-coordinate, length and spanline list. A sprite is either the null sprite, or
a set of bounds and a list of vspans. None of the lists in the data structure
may be null. *)
type subspan = int * Fill.subspancontent

type span = int * int * subspan list

type spanline = span list

type vspan = int * int * spanline list
            
type sprite =
  | NullSprite
  | Sprite of bounds * vspan list

(* Shapes are the same as sprites, but with no span content. *)
type shapespan = int * int

type shapespanline = shapespan list

type shapevspan = int * int * shapespanline list

type shape =
  | NullShape
  | Shape of bounds * shapevspan list

let string_of_shapespan (x, l) =
  Printf.sprintf "(%i, %i)" x l

let string_of_shapespanline spans =
  fold_left ( ^ ) "" (interleave " " (map string_of_shapespan spans))

let string_of_shapevspan (y, l, spanlines) =
  Printf.sprintf
    "vspan: %i, %i: %s"
    y l (fold_left ( ^ ) "\n" (interleave "\n" (map string_of_shapespanline spanlines)))

let string_of_shape = function
  | NullShape -> "NullShape"
  | Shape (Box (minx, miny, maxx, maxy), vspans) ->
      Printf.sprintf "Box: %i, %i, %i, %i\n" minx miny maxx maxy ^
      fold_left ( ^ ) "" (interleave "\n" (map string_of_shapevspan vspans))
  | _ -> "Malformed shape"

let print_shape shp =
  flprint ("\n" ^ string_of_shape shp ^ "\n")

(* When performing operations on shapes and sprites, we often need to know if
and how spans overlap. The type and function [overlap] calculate this. *)
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

let overlap (sa, la) (sb, lb) =
  if la = 0 || lb = 0 then failwith "Malformed spans" else
    let ea = sa + la - 1 and eb = sb + lb - 1 in
      if sa = sb && ea = eb then ABsame else
      if sa > eb + 1 then BthenA else
      if sb > ea + 1 then AthenB else
      if sa = eb + 1 then AabutB else
      if sb = ea + 1 then BabutA else
      if sa > sb && ea = eb then ArightB (sa, la) else
      if sa < sb && ea = eb then BrightA (sb, lb) else
      if sa > sb && ea < eb then AinsideB (sa, la) else
      if sa < sb && ea > eb then BinsideA (sb, lb) else
      if sa >= sb && sa <= eb && ea > eb then AoverlapB (sa, eb - sa + 1)
      else BoverlapA (sb, ea - sb + 1)

(* Map a shape to a sprite, using a function from shape spans to sprite spans. *)
let map_shape_span f y span =
  f span y 

let map_shape_spanline func f y spanline =
  let r = map (map_shape_span f y) spanline in
    func y;
    r

let map_shape_vspan func f (s, l, shapespanlines) =
  let yvals = ilist s (s + l - 1) in
    s, l, map2 (map_shape_spanline func f) yvals shapespanlines

let map_shape_calling func f = function
  | NullShape -> NullSprite
  | Shape (bbox, vspans) ->
      Sprite (bbox, map (map_shape_vspan func f) vspans)

let map_shape =
  map_shape_calling (fun _ -> ())

(* Iterate on the spans of a shape, ordered top to bottom and minorly left to
right. [f] takes y, x, l. FIXME: Move this into polygon, since only used there. *)
let rec iter_shape_spanline minx maxx f y = function
  | [] -> ()
  | (s, l)::more ->
       let e = s + l - 1 in
         let s = max minx s
         and e = min maxx e in
           let l = e - s + 1 in
             if l > 0 then
               f s y l;
             if e > maxx then () else iter_shape_spanline minx maxx f y more

let rec iter_shape_vspan minx maxx miny maxy f shapespanlines s = function
  | 0 -> ()
  | l ->
     if s <= maxy && s >= miny then iter_shape_spanline minx maxx f s (hd shapespanlines);
     if s <= maxy then iter_shape_vspan minx maxx miny maxy f (tl shapespanlines) (s + 1) (l - 1)

let shapespan_iter minx maxx miny maxy f = function
  | NullShape -> ()
  | Shape (_, vspans) ->
      iter
        (function (s, l, shapespanlines) ->
           if s + l - 1 < miny || s > maxy then () else
             iter_shape_vspan minx maxx miny maxy f shapespanlines s l)
        vspans

(* Apply a fill to a shape yielding a sprite. For fancy fills, we use
[map_shape], but for plain ones, we can do it faster ourselves. *)
let fillshape shape fill =
  match fill.Fill.fillkind with
  | Fill.Fancy ->
      map_shape (fun (s, l) y -> s, l, [(l, fill.Fill.fillspan s y l)]) shape
  | Fill.Plain ->
      match shape with
       | NullShape -> NullSprite
       | Shape (bounds, vspans) ->
           let vspans' =
             map
               (fun (s, l, spanlines) ->
                 (s, l, map
                    (map
                       (fun (s, l) -> s, l, [(l, Run (fill.Fill.fillsingle 0 0))]))
                       spanlines))
               vspans
           in
             Sprite (bounds, vspans')

(* Find the corresponding shape to a given sprite. *)
let shape_of_sprite_span (s, l, _) = (s, l)

let shape_of_sprite_spanline =
   map shape_of_sprite_span

let shape_of_sprite_vspan (s, l, spanlines) =
  (s, l, map shape_of_sprite_spanline spanlines)

let shape_of_sprite = function
  | NullSprite -> NullShape
  | Sprite (bounds, vspans) ->
      Shape (bounds, map shape_of_sprite_vspan vspans)

(* \section{Debug routines} *)

(* Check that a sprite or shape is in normal form. *)
let losecontent (s, l, _) = s, l

let overlapok = function
  | AthenB -> true
  (*i | BabutA -> flprint "!"; exit 2; true i*)
  | _ -> false

let spritecheck_span (s, l, subspans) =
  l = fold_left ( + ) 0 (map (function (l, _) -> l) subspans)

let spritecheck_spanline spans =
  match spans with
  | [] -> false
  | _ ->
    let overlaps = couple overlap (map losecontent spans) in
      let overlapsok = List.for_all overlapok overlaps in
        let spansok = List.for_all spritecheck_span spans in
          overlapsok && spansok

let spritecheck_vspan (s, l, spanlines) =
  length spanlines = l && List.for_all spritecheck_spanline spanlines

let spritecheck spr =
  if !debug_spritecheck = false then true else
    match spr with
    | NullSprite -> true
    | Sprite (_, vspans) ->
        match vspans with
        | [] -> (*i print_shape (shape_of_sprite spr); i*) false
        | _ ->
          let overlaps = couple overlap (map losecontent vspans) in
            let overlapsok = List.for_all overlapok overlaps
            and vspansok = List.for_all spritecheck_vspan vspans in
              if overlapsok && vspansok then true
              else
                begin
                  (*i Printf.printf "overlapsok : %b, vspansok: %b\n" overlapsok vspansok;
                  print_shape (shape_of_sprite spr); i*)
                  false
                end

let shapecheck shp =
  if !debug_spritecheck = false then true else
    match spritecheck (fillshape shp (Fill.plain Colour.clear)) with
    | false -> (*i flprint "FAILURE\n"; flprint (string_of_shape shp); flprint "END\n"; i*) false
    | true -> true

(* \section{Iterators and maps} *)

(* General iterator for coordinate-based side effects on shapes. The function
[f] takes [y] as its first argument and [x] as its second. *)
let shape_iter f shape =
  let vspan_iter f (s, l, spanlines) =
    let spanline_iter f spans =
      iter (fun (s, l) -> for x = s to s + l - 1 do f x done) spans
    in
      let spanlines = ref spanlines in
        for y = s to s + l - 1 do
          spanline_iter (f y) (hd !spanlines);
          spanlines := tl !spanlines
        done
  in
    match shape with
    | NullShape -> ()
    | Shape (_, vspans) -> iter (vspan_iter f) vspans

(* General iterator for coordinate-based side effects on sprites. The function
[f] takes [y] then [x] then [a] the colour of the pixel. *)
let sprite_iter f sprite =
  assert (spritecheck sprite);
  let vspan_iter f (s, l, spanlines) =
    (* Here, f has already had the y coordinate applied to it *)
    let spanline_iter f spans =
      let sprite_iter_fun (s, l, subspans) =
        let x = ref s in
          iter
            (fun (l, subspancontent) ->
               begin match subspancontent with
               | Run c ->
                   for px = 0 to l - 1 do f (!x + px) c done
               | Samples a ->
                   for px = 0 to l - 1 do f (!x + px) a.(px) done
               | Interval (o, a) ->
                   for px = 0 to l - 1 do f (!x + px) a.(px + o - 1) done
               end;
               x := !x + l)
            subspans
      in
        iter sprite_iter_fun spans
    in
      let spanlines = ref spanlines in
        for y = s to s + l - 1 do
          spanline_iter (f y) (hd !spanlines);
          spanlines := tl !spanlines
        done
  in 
    match sprite with
    | NullSprite -> ()
    | Sprite (_, vspans) -> iter (vspan_iter f) vspans

let sprite_card spr =
  let size = ref 0 in
    sprite_iter (fun _ _ _ -> incr size) spr;
    !size

let shape_card shp =
  let size = ref 0 in
    shape_iter (fun _ _ -> incr size) shp;
    !size

(* Iterator for coordinate-based mappings on sprites. The function [f] given to
[map_coords] takes [y] then [x] then the colour of the pixel and gives a [a]
new colour. *)
let map_coords_subspan f x (l, subspancontent) =
  match subspancontent with
  | Run c -> l, Run (f x c)
  | Samples a ->
      let a' = Array.copy a in
        for p = 1 to Array.length a' do
          a'.(p - 1) <- f (x + p - 1) a'.(p - 1)
        done;
        (l, Samples a')
  | Interval (o, a) ->
      let a' = Array.sub a (o - 1) l in
        for p = 1 to Array.length a' do
          a'.(p - 1) <- f (x + p - 1) a'.(p - 1)
        done;
        (l, Samples a')

let rec map_coords_span f (s, l, subspans) =
  match subspans with
  | [] -> []
  | ((sub_l, _) as h)::t ->
      (map_coords_subspan f s h)::map_coords_span f (s + sub_l, l - sub_l, t)
    
let rec map_coords_spanline f =
  let f_map_coords_spanline =
    (fun (s, l, subspans) -> (s, l, map_coords_span f (s, l, subspans)))
  in
    map f_map_coords_spanline

let rec map_coords_vspan f (s, l, spanlines) =
  match spanlines with
  | [] -> []
  | h::t -> (map_coords_spanline (f s) h)::map_coords_vspan f (s + 1, l - 1, t)

let map_coords_vspans f =
  let f_map_coords_vspan =
    (fun (s, l, spanlines) -> (s, l, map_coords_vspan f (s, l, spanlines)))
  in
    map f_map_coords_vspan

let map_coords f sprite =
  match sprite with
  | NullSprite -> NullSprite
  | Sprite (bounds, vspans) -> Sprite (bounds, map_coords_vspans f vspans) 

(* Map for sprites.  [f : Colour.colour -> Colour.colour]. *)
let sprite_subspan_map f (l, subspantype) =
  match subspantype with
  | Run c -> (l, Run (f c))
  | Samples a -> (l, Samples (Array.map f a))
  | Interval (o, a) ->
      let arr' = Array.sub a (o - 1) l in
        for i = 1 to l do
          arr'.(i - 1) <- f arr'.(i - 1)
        done;
          (l, Samples arr')

let sprite_span_map f (s, l, subspans) =
  (s, l, map (sprite_subspan_map f) subspans)

let sprite_spanline_map f spanline =
  map (sprite_span_map f) spanline

let sprite_vspan_map f (s, l, spanlines) =
  (s, l, map (sprite_spanline_map f) spanlines)

let sprite_map f = function
  | NullSprite -> NullSprite
  | Sprite (bounds, vspans) -> Sprite (bounds, map (sprite_vspan_map f) vspans)


(* \section{Marshalling shapes and sprites} *)

(* For debug purposes, we should like to be able to marshal sprites and shapes
to file. We can then read them in from an interactive toplevel loop and inspect
them. *)
let marshal_spritetofile spr filename =
  let fh = open_out_bin filename in
    Marshal.to_channel fh spr [];
    close_out fh

let unmarshal_spritefromfile filename =
  let fh = open_in_bin filename in
    let sprite = (Marshal.from_channel fh : sprite) in
      close_in fh; sprite

let marshal_shapetofile shp filename =
  let fh = open_out_bin filename in
    Marshal.to_channel fh shp [];
    close_out fh

let unmarshal_shapefromfile filename =
  let fh = open_in_bin filename in
    let shape = (Marshal.from_channel fh : shape) in
      close_in fh; shape

(* \section{Calculating cache sizes} *)

(* In order for the cache metrics to work, we need to estimate the size of
shape and sprite data structures in memory. This depends upon compiler release,
but since it's only an estimate, that doesn't matter. *)

(* To compensate for 32 or 64-bit systems *)
let mult = Sys.word_size / 32

let shapesize_spanline spanline =
  5 * (length spanline)

let shapesize_vspan (s, l, spanlines) =
  4
+ fold_left ( + ) 0 (map shapesize_spanline spanlines)
+ 3 * length spanlines

let shapesize_vspans vspans =
  3 * length vspans +
  fold_left ( + ) 0 (map shapesize_vspan vspans)

let shapesize shape =
  match shape with
  | NullShape -> 4 * mult
  | Shape (bounds, vspans) ->
     (6 + shapesize_vspans vspans) * 4 * mult

let spritesize_subspan (l, content) =
  3 +
  match content with
  | Run _ -> 2
  | Samples a -> 1 + Array.length a
  | Interval (_, a) -> 3 + Array.length a

let spritesize_span (_, _, subspans) =
  3 +
  2 * length subspans +
  fold_left ( + ) 0 (map spritesize_subspan subspans)

let spritesize_spanline spanline =
  3 * length spanline +
  fold_left ( + ) 0 (map spritesize_span spanline)

let spritesize_vspan (_, l, spanlines) =
  3 * l +
  fold_left ( + ) 0 (map spritesize_spanline spanlines)

let spritesize_vspans vspans =
  3 * length vspans + 
  fold_left ( + ) 0 (map spritesize_vspan vspans)

let spritesize spr =
  match spr with
  | NullSprite -> 4 * mult
  | Sprite (bounds, vspans) ->
     (6 + spritesize_vspans vspans) * 4 * mult

(* Sometimes we require a shape representing a rectangle at raster resolution
--- for instance representing an initial update region in a windowing system
based upon rectangles. *)
let box x y w h =
  if w = 0 && h = 0 then NullShape else
    if w < 0 || h < 0 then failwith "Sprite.box: negative argument." else
      Shape (Box (x, y, x + w - 1, y + h - 1), [(y, h, many [(x, w)] h)])

(* \section{Translating shapes and sprites} *)

(* Translate a shape. *)
let translate_shape_spanline x =
  map (fun (s, l) -> s + x, l)

let translate_shape_vspan x y (s, l, spanlines) =
  s + y, l, map (translate_shape_spanline x) spanlines

let translate_shape x y shp =
  match shp with
  | NullShape -> NullShape
  | Shape (bounds, vspans) ->
      match bounds with
      | NoBounds -> failwith "Sprite.translate_shape: No bounds."
      | Box (a, b, c, d) ->
          Shape (Box (a + x, b + y, c + x, d + y),
            map (translate_shape_vspan x y) vspans)

(* Translate a sprite. *)
let translate_sprite_spanline x =
  map (fun (s, l, subspans) -> s + x, l, subspans)

let translate_sprite_vspan x y (s, l, spanlines) =
  s + y, l, map (translate_sprite_spanline x) spanlines

let translate_sprite x y spr =
  match spr with
  | NullSprite -> NullSprite
  | Sprite (bounds, vspans) ->
      match bounds with
      | NoBounds -> failwith "Sprite.translate_sprite: No bounds."
      | Box (a, b, c, d) ->
          Sprite (Box (a + x, b + y, c + x, d + y),
            map (translate_sprite_vspan x y) vspans)

(* \section{Calculating bounding boxes} *)

(* Given a shape, add a bounding box to it. For speed, do this imperatively. We
do this by calculating the minimum and maximum x coordinate over all the
spanlines, and the minimum and maximum y coordinate of any spanline. *)
let minmax_shape_spanline spanline =
  match spanline with
  | [] -> max_int, min_int
  | _ ->
    let (s, _), (e, l) = extremes spanline in
      (s, e + l - 1)

let minmax_x_shape_spanlines spanlines =
  let rmin = ref max_int
  and rmax = ref min_int
  and spanlines = ref spanlines in
    while !spanlines <> [] do
      let s, e = minmax_shape_spanline (hd !spanlines) in
        if s < !rmin then rmin := s;
        if e > !rmax then rmax := e;
        spanlines := tl !spanlines
    done;
    !rmin, !rmax

let minmax_x_shape_vspan (_, _, spanlines) =
  minmax_x_shape_spanlines spanlines 

let minmax_x_shape_vspans vspans =
  let rmin = ref max_int
  and rmax = ref min_int
  and vspans = ref vspans in
    while !vspans <> [] do
      let s, e = minmax_x_shape_vspan (hd !vspans) in
        if s < !rmin then rmin := s;
        if e > !rmax then rmax := e;
        vspans := tl !vspans
    done;
    !rmin, !rmax

let rec boxshape shape =
  match shape with
  | NullShape -> NullShape
  | Shape (_, vspans) ->
      let (y0, _, _), (y1, l, _) = extremes vspans in
        let ymin, ymax = y0, y1 + l - 1
        and xmin, xmax = minmax_x_shape_vspans vspans in
          Shape (Box (xmin, ymin, xmax, ymax), vspans)
           
(* Similarly, make a bounding box for a sprite. *)
let minmax_sprite_spanline spanline =
  let (s, _, _), (e, l, _) = extremes spanline in
    (s, e + l - 1)

let minmax_x_sprite_vspan (_, _, spanlines) =
  let rmin = ref max_int
  and rmax = ref min_int
  and spanlines = ref spanlines in
    while !spanlines <> [] do
      let s, e = minmax_sprite_spanline (hd !spanlines) in
        if s < !rmin then rmin := s;
        if e > !rmax then rmax := e;
        spanlines := tl !spanlines
    done;
    !rmin, !rmax

let minmax_x_sprite_vspans vspans =
  let rmin = ref max_int
  and rmax = ref min_int
  and vspans = ref vspans in
    while !vspans <> [] do
      let s, e = minmax_x_sprite_vspan (hd !vspans) in
        if s < !rmin then rmin := s;
        if e > !rmax then rmax := e;
        vspans := tl !vspans
    done;
    !rmin, !rmax

let boxsprite sprite =
  let vspanstrip vspan = (match vspan with (s, l, _) -> s, l) in
    match sprite with
    | NullSprite -> NullSprite
    | Sprite (_, vspans) ->
       let a, b = extremes vspans in
         let (y0, _) = vspanstrip a
         and (y1, l) = vspanstrip b in
           let ymin, ymax = y0, y1 + l - 1
           and xmin, xmax = minmax_x_sprite_vspans vspans in
             Sprite (Box (xmin, ymin, xmax, ymax), vspans)

(* \section{Portions: taking a subset of a sprite} *)

(* Cuts a list of subspans into [1..l] and [l+1..n], where [n] is the length
and [l] the cut point, altering subspans correctly when one straddles the cut
point. Notice how we use [Interval] here --- since arrays are never used
mutably, we can share them between objects, knowing nothing can break. *)
let rec slice subspans l =
  if l = 0 then [], subspans else
    match subspans with
    | (sl, sc) as s::ss ->
        if sl > l then
          let left, right =
            match sc with
            | Run c ->
                (l, Run c), (sl - l, Run c)
            | Samples arr ->
                (l, Interval (1, arr)), (sl - l, Interval(l + 1, arr))
            | Interval (o, arr) ->
                (l, Interval (o, arr)), (sl - l, Interval(o + l, arr))
          in
            conspairopt ((Some left, None), (slice (right::ss) (max (l - sl) 0)))
        else
          conspairopt ((Some s, None), (slice ss (l - sl)))
    | _ -> failwith "Bad call to slice"

(* We often need to accumulate together spanlines into maximal non-overlapping
vspans. The spanlines should be presented in order of increasing y coordinate.
Produces lists of vspans which, when complete, require the order of spanlines
to be reversed within each vspan, and the order of vspans reversing. *)
let vspan_accumulate vspans y spanline =
  match vspans with
  | [] -> [(y, 1, [spanline])]
  | (cy, cl, v)::vs ->
      if y = cy + cl
        then (cy, cl + 1, spanline::v)::vs
        else (y, 1, [spanline])::vspans

(* Accumulate a list of (possibly null i.e malformed) spanlines starting at [y]
to a list of vspans. *)
let rec vspan_accumulate_spanlines vspans y = function
  | [] -> vspans
  | []::t -> vspan_accumulate_spanlines vspans (y + 1) t
  | h::t -> vspan_accumulate_spanlines (vspan_accumulate vspans y h) (y + 1) t

(* Reverse the spanlines within a vspan. *)
let vspan_reversespanlines (s, l, spanlines) =
  s, l, rev spanlines

(* Calculate the portion of a sprite corresponding to a given shape. Fails if
[shp] is not a subset of the shape of [spr]. *)
let rec portion_spanline spr shp =
  match spr, shp with
  | [], [] -> []
  | [], _-> failwith "portion_spanline: bad input"
  | _, [] -> []
  | ((sa, la, adata as ah::at) as a), ((sb, lb as bh::bt) as b) ->
      match overlap (sa, la) bh with
      | ABsame -> ah::portion_spanline at bt
      | BthenA | AabutB -> portion_spanline a bt
      | AthenB | BabutA -> portion_spanline at b
      | AinsideB _ -> failwith "portion_spanline/AinsideB: bad input"
      | BinsideA (s, l) ->
          (s, l, fst (slice (snd (slice adata (s - sa))) l))::portion_spanline a bt
      | AoverlapB (s, l) ->
          if sb = sa && lb < la
            then (s, l, fst (slice adata l))::portion_spanline a bt
            else failwith "portion_spanline/AoverlapB: bad input"
      | BoverlapA _ ->
          failwith "portion_spanline/BoverlapA: bad input"
      | ArightB _ ->
          failwith "portion_spanline/ArightB: bad input"
      | BrightA (s, l) ->
          (s, l, snd (slice adata (s - sa)))::portion_spanline at bt

(* Here, [outputvspans] is an accumulating argument. *)
let rec portion_vspans spr shp outputvspans =
  match spr, shp with
  | _, [] -> rev_map vspan_reversespanlines outputvspans
  | [], _::_ -> failwith "portion_vspans: bad input "
  | (((sa, la, aspanlines)::at) as a),
    (((sb, lb, bspanlines)::bt) as b) ->
      match overlap (sa, la) (sb, lb) with
      | ABsame ->
          let spanlines' = map2 portion_spanline aspanlines bspanlines in
            portion_vspans at bt (vspan_accumulate_spanlines outputvspans sa spanlines')
      | BthenA | AabutB -> portion_vspans a bt outputvspans
      | AthenB | BabutA -> portion_vspans at b outputvspans
      | AinsideB _ ->
          failwith "portion_vspans/AinsideB: bad input "
      | BinsideA (s, l) ->
          let aspanlines' = take (drop aspanlines (s - sa)) l in
            let spanlines' = map2 portion_spanline aspanlines' bspanlines in
              let outputvspans' = vspan_accumulate_spanlines outputvspans s spanlines' in
                portion_vspans a bt outputvspans'
      | AoverlapB (s, l) ->
          if sa = sb && lb < la then
            let aspanlines' = take aspanlines l in
              let spanlines' = map2 portion_spanline aspanlines' bspanlines in
                let outputvspans' = vspan_accumulate_spanlines outputvspans s spanlines' in
                  portion_vspans a bt outputvspans'
          else failwith "portion_vspans/AoverlapB: bad input "
      | BoverlapA (s, l) ->
          if sb = sa && la < lb then
            let aspanlines' = drop aspanlines l in
              let spanlines' = map2 portion_spanline aspanlines' bspanlines in
                let outputvspans' = vspan_accumulate_spanlines outputvspans s spanlines' in
                  portion_vspans at b outputvspans'
          else failwith "portion_vspans/BoverlapA: bad input "
      | ArightB _ ->
          failwith "portion_vspans/ArightB: bad input "
      | BrightA (s, l) ->
          let aspanlines' = drop aspanlines (sb - sa) in
            let spanlines' = map2 portion_spanline aspanlines' bspanlines in
              let outputvspans' = vspan_accumulate_spanlines outputvspans s spanlines' in
                portion_vspans at bt outputvspans'

let portion spr shp =
  match spritecheck spr, shapecheck shp with
  | false, false -> failwith "portion: both inputs malformed"
  | false, true -> failwith "portion: first input malformed"
  | true, false -> failwith "portion: second input malformed"
  | _ ->
    let result =
      match spr, shp with
      | _ , NullShape -> NullSprite
      | NullSprite, _ -> failwith "portion: malformed input (sprite null, shape not)"
      | Sprite (_, sprvspans), Shape (_, shpvspans) ->
          boxsprite (Sprite (NoBounds, (portion_vspans sprvspans shpvspans [])))
    in
      if spritecheck result then result else failwith "portion: malformed output"

(* \section{Composing sprites together} *)

(* When we compose two objects together, we require both the sprite of their
composite and a shape representing the set of pixels which are newly opaque i.e
all those in the newer sprite alone or in the overlapping region. \smallgap*)

(* Specialised [array_map2] for colour arrays. *)
let array_map2 f (a : Colour.colour array) (b : Colour.colour array) =
  let la = Array.length a
  and lb = Array.length b in
    if la = lb then
      let c = Array.make la Colour.clear in
        for p = 0 to (la - 1) do c.(p) <- f a.(p) b.(p) done;
        c
    else
      failwith "array_map2: Differing array lengths"

(* Flatten a list of subspans to an array. *)
let caf_flatten subspans length =
  let arr = Array.make length Colour.clear
  and subspans' = ref subspans
  and pos = ref 0 in
    while !subspans' <> [] do
      (match hd !subspans' with (l, content) ->
        (match content with
          | Run c -> Array.fill arr !pos l c
          | Samples c -> Array.blit c 0 arr !pos l
          | Interval (o, c) -> Array.blit c (o - 1) arr !pos l);
        pos := !pos + l);
      subspans' := tl !subspans'
    done;
    arr

(* Compose two lists of subspans which will form a single output span. They
will have totally overlapped, and so we are losing no data sharing by creating
a new array. *)
let caf_compose compop a b length =
  let a_array = caf_flatten a length
  and b_array = caf_flatten b length in
    array_map2 compop a_array b_array

(* When spans overlap, we wish to preserve run-length encoding of plain bits
which overlap, and data sharing in arrays. First we calculate where the runs in
the overlapping part overlap. *)
let rec runs subspans p =
  match subspans with
  | [] -> []
  | (l, Run _)::ss -> (p, l)::runs ss (p + l)
  | (l, _)::ss -> runs ss (p + l)

let rec runsoverlap a b =
  match a, b with
  | [], _ | _, [] -> []
  | ((sa, la) as ah)::at, bh::bt ->
      match overlap ah bh with
      | BthenA | AabutB -> runsoverlap a bt
      | AthenB | BabutA -> runsoverlap at b
      | AoverlapB (s, l) | BinsideA (s, l) ->
          s::(s + l - 1)::runsoverlap a bt
      | BoverlapA (s, l) | AinsideB (s, l) ->
          s::(s + l - 1)::runsoverlap at b
      | ABsame -> sa::(sa + la - 1)::runsoverlap at bt
      | ArightB (s, l) | BrightA (s, l)->
          s::(s + l - 1)::runsoverlap at bt

(* Find the points at which we ought to split the subspans, returning them as a
list of sizes to take from the front of the list --- i.e a difference list. *)
let splits a b length =
  let rec diffs splits n =
    match splits with
    | [] -> []
    | s::ss -> if (s - n) > 0 then (s - n)::(diffs ss s) else diffs ss s
  in
    let aruns = runs a 1
    and bruns = runs b 1 in
      let slices = diffs (runsoverlap aruns bruns) 0 in
        let sum = fold_left ( + ) 0 slices in
          if sum < length then slices @ [length - sum] else slices
  
(* Split a and b at the split points. *)
let rec caf_splitruns a b splitpoints =
  match splitpoints with
  | [] | [_] -> [a, b]
  | split::splits ->
      let aleft, aright = slice a split
      and bleft, bright = slice b split in
        (aleft, bleft)::(caf_splitruns aright bright splits)

(* Calculates the composition of the overlapping section of two spans. *)
let caf_overlap compop length a b =
  let splitlengths = splits a b length in
    let runs = caf_splitruns a b splitlengths in
      map2
      (fun (asubspans, bsubspans) length ->
         match asubspans, bsubspans with
          | [(al, Run ac)], [(bl, Run bc)] -> (al, Run (compop ac bc))
          | _ -> (length, Samples (caf_compose compop asubspans bsubspans length)))
      runs splitlengths

(* Accumulates shape spans presented in increasing order of x coordinate. Works even
if the spans overlap. *)
let span_accumulate spans ((s', l') as span) =
  match spans with
  | [] -> [span]
  | (s, l)::t ->
      if s' < s
        then failwith "span_accumulate"
        else if s' = s + l
          then (s, l + l')::t
          else if s' >= s && s' < s + l
            then let e' = s' + l' - 1 in (s, e' - s + 1)::t
             else span::spans

(* Do many at once. *)
let spans_accumulate = fold_left span_accumulate

(* Given a predicate on colours [filterop], the array ([arr], the start [o] and
length [l], the current subspan offset, calculate a list of spans representing
the parts true under the predicate. This is imperative for speed.  *)
let array_filter filterop arr o l subspan_offset =
  let spans = ref [] and run = ref 0 and start = ref 0 in
    for p = o to o + l - 1 do
      if filterop arr.(p - 1)
        then
          (if !run = 0 then start := p - o + 1; incr run)
        else
          if !run > 0 then (spans := (!start + subspan_offset - 1, !run)::!spans; run := 0)
    done;
    if !run > 0 then spans := (!start + subspan_offset - 1, !run)::!spans;
    rev !spans

(* Filter a whole span. Returns the spans in reverse order *)
let rec filter_span_inner filterop (s, sl, subspans) filterspans =
  match subspans with
  | [] -> filterspans
  | (l, Run c)::rest ->
        if filterop c then
          let filterspans' = span_accumulate filterspans (s, l) in
            filter_span_inner filterop (s + l, sl - l, rest) filterspans'
        else
          filter_span_inner filterop (s + l, sl - l, rest) filterspans
  | (l, Samples c)::rest ->
        let filterspans' =
          spans_accumulate filterspans (array_filter filterop c 1 l s)
        in
          filter_span_inner filterop (s + l, sl - l, rest) filterspans'
  | (l, Interval (o, c))::rest ->
        let filterspans' =
          spans_accumulate filterspans (array_filter filterop c o l s)
        in
          filter_span_inner filterop (s + l, sl - l, rest) filterspans'
      
let filter_span filterop span = filter_span_inner filterop span []

(* The analagous function for sprite spans. We ought to extend this to
(optionally?) merge subspans. This would involve memory copying, but prevent
excess fragmentation. *)
let spritespan_accumulate spans (s', l', c') =
  match spans with
  | (s, l, c)::t when s' = s + l -> (s, l + l', (c'::c))::t
  | (s, l, _)::_ when s' < s + l -> failwith "spritespan_accumulate"
  | _ -> (s', l', [c'])::spans

let spritespans_accumulate = fold_left spritespan_accumulate

(* Compose and filter a spanline. *)
let rec caf_spanline_inner compop filterop a b spans filterspans =
  (* Once all the accumulation is done, things are in the wrong order. *)
  let rev_spanline =
    let f_rev_spanline =
      (fun (s, l, cs) -> s, l, flatten (rev cs))
    in
      rev_map f_rev_spanline
  in
    match a, b with
    | [], [] -> rev_spanline spans, rev filterspans
    | a, [] -> rev_spanline (spritespans_accumulate spans a), rev filterspans
    | [], b -> rev_spanline (spritespans_accumulate spans b),
               let bfun = (fun s -> rev (filter_span filterop s)) in
                 let filter_span_lists = map bfun b in
                   rev (spans_accumulate filterspans (flatten filter_span_lists))
    | ((sa, la, asubspans) as ah)::at, ((sb, lb, bsubspans) as bh)::bt ->
        let addfiltered fspans spn = spans_accumulate fspans (rev (filter_span filterop spn))
        and ea = sa + la - 1 and eb = sb + lb - 1 in
          match overlap (sa, la) (sb, lb) with
          | BthenA ->
              let spans' = spritespan_accumulate spans bh
              and filterspans' = addfiltered filterspans bh in
                caf_spanline_inner compop filterop a bt spans' filterspans'
          | AthenB ->
              let spans' = spritespan_accumulate spans ah in
                caf_spanline_inner compop filterop at b spans' filterspans
          | ABsame ->
              let overlapspan = (sa, la, caf_overlap compop la asubspans bsubspans) in
                let spans' = spritespan_accumulate spans overlapspan
                and filterspans' = addfiltered filterspans overlapspan in
                  caf_spanline_inner compop filterop at bt spans' filterspans'
          | AabutB ->
              let spans' = spritespan_accumulate spans bh
              and filterspans' = addfiltered filterspans bh in
                caf_spanline_inner compop filterop a bt spans' filterspans'
          | BabutA ->
              let spans' = spritespan_accumulate spans ah in
                caf_spanline_inner compop filterop at b spans' filterspans
          | AinsideB _ ->
              let bleft, brest = slice bsubspans (sa - sb) in
                let bmiddle, bright = slice brest la in
                  let overlapspan = (sa, la, caf_overlap compop la asubspans bmiddle)
                  and spans' = spritespan_accumulate spans (sb, sa - sb, bleft) in
                    let spans'' = spritespan_accumulate spans' overlapspan
                    and filterspans' = addfiltered filterspans (sb, sa - sb, bleft) in
                      let filterspans'' = addfiltered filterspans' overlapspan
                      and bh' = (ea + 1, eb - ea, bright) in
                        caf_spanline_inner compop filterop at (bh'::bt) spans'' filterspans''
          | BinsideA _ -> 
              let aleft, arest = slice asubspans (sb - sa) in
                let amiddle, aright = slice arest lb in
                  let overlapspan = (sb, lb, caf_overlap compop lb amiddle bsubspans)
                  and spans' = spritespan_accumulate spans (sa, sb - sa, aleft) in
                    let spans'' = spritespan_accumulate spans' overlapspan
                    and filterspans' = addfiltered filterspans overlapspan
                    and ah' = (eb + 1, ea - eb, aright) in
                      caf_spanline_inner compop filterop (ah'::at) bt spans'' filterspans'
         | ArightB _ -> 
             let bleft, bright = slice bsubspans (sa - sb) in
               let overlapspan = (sa, la, caf_overlap compop la asubspans bright)
               and spans' = spritespan_accumulate spans (sb, sa - sb, bleft) in
                 let spans'' = spritespan_accumulate spans' overlapspan
                 and filterspans' = addfiltered filterspans (sb, sa - sb, bleft) in
                   let filterspans'' = addfiltered filterspans' overlapspan in
                     caf_spanline_inner compop filterop at bt spans'' filterspans''
         | BrightA _ ->
             let aleft, aright = slice asubspans (sb - sa) in
               let overlapspan = (sb, lb, caf_overlap compop lb aright bsubspans)
               and spans' = spritespan_accumulate spans (sa, sb - sa, aleft) in
                 let spans'' = spritespan_accumulate spans' overlapspan
                 and filterspans' = addfiltered filterspans overlapspan in
                   caf_spanline_inner compop filterop at bt spans'' filterspans'
         | AoverlapB (s, l) ->
             let amiddle, aright = slice asubspans l
             and bleft, bmiddle = slice bsubspans (sa - sb) in
               let overlapspan = (s, l, caf_overlap compop l amiddle bmiddle)
               and spans' = spritespan_accumulate spans (sb, sa - sb, bleft) in
                 let spans'' = spritespan_accumulate spans' overlapspan
                 and filterspans' = addfiltered filterspans (sb, sa - sb, bleft) in
                   let filterspans'' = addfiltered filterspans' overlapspan
                   and ah' = (eb + 1, ea - eb, aright) in
                     caf_spanline_inner compop filterop (ah'::at) bt spans'' filterspans''
         | BoverlapA (s, l) ->
             let aleft, amiddle = slice asubspans (sb - sa)
             and bmiddle, bright = slice bsubspans l in
               let overlapspan = (s, l, caf_overlap compop l amiddle bmiddle)
               and spans' = spritespan_accumulate spans (sa, sb - sa, aleft) in
                 let spans'' = spritespan_accumulate spans' overlapspan
                 and filterspans' = addfiltered filterspans overlapspan
                 and bh' = (ea + 1, eb - ea, bright) in
                   caf_spanline_inner compop filterop at (bh'::bt) spans'' filterspans'

let caf_spanline compop filterop a b =
  caf_spanline_inner compop filterop a b [] []

(* Compose and filter vspans. *)
let rec caf_vspans_inner compop filterop a b vspans filtervspans =
  match a, b with
  | [], [] ->
      let v = (rev_map vspan_reversespanlines vspans)
      and f = (rev_map vspan_reversespanlines filtervspans) in
        v, f
  | (s, l, aspanlines)::at, [] ->
      let vspans' = vspan_accumulate_spanlines vspans s aspanlines in 
        caf_vspans_inner compop filterop at b vspans' filtervspans
  | [], (s, l, bspanlines)::bt ->
      let vspans' = vspan_accumulate_spanlines vspans s bspanlines in
        let filteredspanlines =
          map (function x -> (snd (caf_spanline compop filterop [] x))) bspanlines
        in 
          let filtervspans' = vspan_accumulate_spanlines filtervspans s filteredspanlines in
            caf_vspans_inner compop filterop a bt vspans' filtervspans'
  | (sa, la, aspanlines)::at, (sb, lb, bspanlines)::bt ->
      let ea = sa + la - 1 and eb = sb + lb - 1 in
        match overlap (sa, la) (sb, lb) with
        | AthenB | BabutA ->
            let vspans' = vspan_accumulate_spanlines vspans sa aspanlines in
              caf_vspans_inner compop filterop at b vspans' filtervspans
        | BthenA | AabutB ->
            let vspans' = vspan_accumulate_spanlines vspans sb bspanlines in
              let filteredspanlines =
                map (function x -> (snd (caf_spanline compop filterop [] x))) bspanlines
              in
                let filtervspans' =
                  vspan_accumulate_spanlines filtervspans sb filteredspanlines
                in
                  caf_vspans_inner compop filterop a bt vspans' filtervspans'
        | ABsame ->
            let composedspanlines, filteredspanlines =
              split (map2 (caf_spanline compop filterop) aspanlines bspanlines)
            in
              let vspans' = vspan_accumulate_spanlines vspans sa composedspanlines
              and filtervspans' =
                vspan_accumulate_spanlines filtervspans sa filteredspanlines
              in
                caf_vspans_inner compop filterop at bt vspans' filtervspans'
        | BinsideA (s, l) ->
            let aleft, arest = cleave aspanlines (s - sa) in
              let amiddle, aright = cleave arest l in
                let vspans' = vspan_accumulate_spanlines vspans sa aleft in
                  let composedspanlines, filteredspanlines =
                    split (map2 (caf_spanline compop filterop) amiddle bspanlines)
                  in
                    let vspans'' =
                      vspan_accumulate_spanlines vspans' s composedspanlines
                    and filtervspans' =
                      vspan_accumulate_spanlines filtervspans s filteredspanlines
                    and a' = (eb + 1, ea - eb, aright) in
                      caf_vspans_inner compop filterop (a'::at) bt vspans'' filtervspans'
        | AinsideB (s, l) ->
            let bleft, brest = cleave bspanlines (s - sb) in
              let bmiddle, bright = cleave brest l in
                let vspans' = vspan_accumulate_spanlines vspans sb bleft in
                  let filteredbleft =
                    map
                      (function x -> (snd (caf_spanline compop filterop [] x)))
                      bleft
                  in
                    let filtervspans' = vspan_accumulate_spanlines filtervspans sb filteredbleft in
                      let composedspanlines, filteredspanlines =
                        split (map2 (caf_spanline compop filterop) aspanlines bmiddle)
                      in
                        let vspans'' =
                          vspan_accumulate_spanlines vspans' s composedspanlines
                        and filtervspans'' = 
                          vspan_accumulate_spanlines filtervspans' s filteredspanlines
                        and b' = (ea + 1, eb - ea, bright) in
                          caf_vspans_inner compop filterop at (b'::bt) vspans'' filtervspans''
        | ArightB (s, l) ->
            let bleft, bright = cleave bspanlines (s - sb) in
              let vspans' = vspan_accumulate_spanlines vspans sb bleft in
                let filteredbleft =
                  map
                    (function x -> (snd (caf_spanline compop filterop [] x)))
                    bleft
                in
                  let filtervspans' = vspan_accumulate_spanlines filtervspans sb filteredbleft in
                    let composedspanlines, filteredspanlines =
                      split (map2 (caf_spanline compop filterop) aspanlines bright)
                    in
                      let vspans'' =
                        vspan_accumulate_spanlines vspans' s composedspanlines
                      and filtervspans'' =
                        vspan_accumulate_spanlines filtervspans' s filteredspanlines
                      in
                        caf_vspans_inner compop filterop at bt vspans'' filtervspans''
        | BrightA (s, l) ->
            let aleft, aright = cleave aspanlines (s - sa) in
              let vspans' = vspan_accumulate_spanlines vspans sa aleft in
                let composedspanlines, filteredspanlines =
                  split (map2 (caf_spanline compop filterop) aright bspanlines)
                in
                  let vspans'' =
                    vspan_accumulate_spanlines vspans' s composedspanlines
                  and filtervspans' =
                    vspan_accumulate_spanlines filtervspans s filteredspanlines
                  in
                    caf_vspans_inner compop filterop at bt vspans'' filtervspans'
        | BoverlapA (s, l) ->
            let aleft, aright = cleave aspanlines (sb - sa)
            and bleft, bright = cleave bspanlines l in
              let vspans' = vspan_accumulate_spanlines vspans sa aleft in
                let composedspanlines, filteredspanlines =
                  split (map2 (caf_spanline compop filterop) aright bleft)
                in
                  let vspans'' =
                    vspan_accumulate_spanlines vspans' s composedspanlines
                  and filtervspans' =
                    vspan_accumulate_spanlines filtervspans s filteredspanlines
                  and bh' = (ea + 1, eb - ea, bright) in
                    caf_vspans_inner compop filterop at (bh'::bt) vspans'' filtervspans'
        | AoverlapB (s, l) ->
            let aleft, aright = cleave aspanlines l
            and bleft, bright = cleave bspanlines (sa - sb) in
              let filteredbleft =
                map
                  (function x -> (snd (caf_spanline compop filterop [] x)))
                  bleft
                in
                  let vspans' = vspan_accumulate_spanlines vspans sb bleft
                  and filtervspans' = vspan_accumulate_spanlines filtervspans sb filteredbleft in
                    let composedspanlines, filteredspanlines =
                      split (map2 (caf_spanline compop filterop) aleft bright)
                    in
                      let vspans'' =
                        vspan_accumulate_spanlines vspans' s composedspanlines
                      and filtervspans'' =
                        vspan_accumulate_spanlines filtervspans' s filteredspanlines
                      and ah' = (eb + 1, ea - eb, aright) in
                        caf_vspans_inner compop filterop (ah'::at) bt vspans'' filtervspans''
                  
let caf_vspans compop filterop a b = caf_vspans_inner compop filterop a b [] []

(* Compose and filter for sprites. *)
let caf compop filterop a b =
  match spritecheck a, spritecheck b with
  | false, false -> failwith "caf: both inputs malformed"
  | false, true -> failwith "caf: new section malformed"
  | true, false -> failwith "caf: accumulator malformed"
  | _ ->
    let sprite, shape =
      match a, b with
      | NullSprite, NullSprite -> NullSprite, NullShape
      | NullSprite, Sprite (_, bvspans) ->
         (* Accumulator is null *)
         let composedvspans, filteredvspans =
           caf_vspans compop filterop [] bvspans
         in 
          (* filteredvspans may be null *)
          let shape =
            match filteredvspans with
            | [] -> NullShape
            | _ -> (boxshape (Shape (NoBounds, filteredvspans)))
          and sprite =
            match composedvspans with
              | [] -> NullSprite
              | _ -> (boxsprite (Sprite (NoBounds, composedvspans)))
          in
            sprite, shape
      | _, NullSprite -> a, NullShape
      | Sprite (_,avspans), Sprite (_,bvspans) ->
         (* Something to do *)
         let composedvspans, filteredvspans =
           caf_vspans compop filterop avspans bvspans
         in
          (* filteredvspans may be null *)
          let shape =
            match filteredvspans with
            | [] -> NullShape
            | _ -> (boxshape (Shape (NoBounds, filteredvspans)))
          and sprite =
            match composedvspans with
            | [] -> NullSprite
            | _ -> (boxsprite (Sprite (NoBounds, composedvspans)))
          in
            sprite, shape
    in
      match spritecheck sprite, shapecheck shape with
      | false, false -> failwith "caf: both outputs malformed"
      | false, true -> failwith "caf: caf output sprite malformed"
      | true, false -> failwith "caf: caf output shape malformed"
      | _ -> sprite, shape

(* \section{Union of shapes} *)

(* The union of two shapes.\smallgap *)

(* Spanline union. Return also the bounds of the new spanline. On original
call, neither [a] nor [b] is empty. We use an inline comparison function here
for speed: At some point we need to test if this is actually faster, taking
into account the extra garbage collection. *)
let rec shape_spanline_union minx maxx a b =
  match a, b with
  |  [], [] -> []
  | (((sa, la) as ah)::at), [] ->
      minx := if !minx < sa then !minx else sa;
      let e = sa + la - 1 in
        maxx := if e > !maxx then e else !maxx;
        ah::shape_spanline_union minx maxx at b
  | [], (((sb, lb) as bh)::bt) ->
      minx := if !minx < sb then !minx else sb;
      let e = sb + lb - 1 in
        maxx := if e > !maxx then e else !maxx;
        bh::shape_spanline_union minx maxx a bt
  | ((sa, la) as ah)::at, ((sb, lb) as bh)::bt ->
      let ea = sa + la - 1
      and eb = sb + lb - 1 in
        let small_s = if sa < sb then sa else sb in
          minx := if small_s < !minx then small_s else !minx;
        let big_e = if ea > eb then ea else eb in
          maxx := if big_e > !maxx then big_e else !maxx;
        if la = 0 || lb = 0 then
          failwith "Shape_spanline_union: Malformed spans"
        else
          let ea = sa + la - 1 and eb = sb + lb - 1 in
            if sa = sb && ea = eb then ah::shape_spanline_union minx maxx at bt else
            if sa > eb + 1 then bh::shape_spanline_union minx maxx a bt else
            if sb > ea + 1 then ah::shape_spanline_union minx maxx at b else
            if sa = eb + 1 then
              shape_spanline_union minx maxx ((sb, la + lb)::at) bt
            else if sb = ea + 1 then
              shape_spanline_union minx maxx at ((sa, la + lb)::bt)
            else if sa > sb && ea <= eb then shape_spanline_union minx maxx at b else
            if sa < sb && ea >= eb then shape_spanline_union minx maxx a bt else
            if sa >= sb && sa <= eb && ea > eb then
              shape_spanline_union minx maxx ((sb, ea - sb + 1)::at) bt
            else
              shape_spanline_union minx maxx at ((sa, eb - sa + 1)::bt)

let rec shape_vspans_union a b =
  let xmin = ref max_int
  and xmax = ref min_int in
    let shape_spanline_union = shape_spanline_union xmin xmax in
      match a, b with
      | [], [] -> []
      | a, [] -> a
      | [], b -> b
      | ((sa, la, aspanlines) as ah)::at, ((sb, lb, bspanlines) as bh)::bt ->
          match overlap (sa, la) (sb, lb) with
          | BthenA -> bh::shape_vspans_union a bt
          | AthenB -> ah::shape_vspans_union at b
          | AabutB ->
              let ah' = (sb, la + lb, bspanlines @ aspanlines) in
                shape_vspans_union (ah'::at) bt
          | BabutA ->
              let bh' = (sa, la + lb, aspanlines @ bspanlines) in
                shape_vspans_union at (bh'::bt)
          | AoverlapB (s, l) ->
              let btop, bmiddle = cleave bspanlines (lb - l)
              and amiddle, abottom = cleave aspanlines l in
                let spanlines' = map2 shape_spanline_union amiddle bmiddle in
                  let ah' = (sb, lb + la - l, btop @ spanlines' @ abottom) in
                    shape_vspans_union (ah'::at) bt
          | BoverlapA (s, l) ->
              let atop, amiddle = cleave aspanlines (la - l)
              and bmiddle, bbottom = cleave bspanlines l in
                let spanlines' = map2 shape_spanline_union amiddle bmiddle in
                  let bh' = (sa, la + lb - l, atop @ spanlines' @ bbottom) in
                    shape_vspans_union at (bh'::bt)
          | AinsideB (s, l) ->
              let btop, brest = cleave bspanlines (sa - sb) in
                let bmiddle, bbottom = cleave brest l in
                  let spanlines' = map2 shape_spanline_union aspanlines bmiddle in
                    let bh' = (sb, lb, btop @ spanlines' @ bbottom) in
                      shape_vspans_union at (bh'::bt)
          | BinsideA (s, l) ->
              let atop, arest = cleave aspanlines (sb - sa) in
                let amiddle, abottom = cleave arest l in
                  let spanlines' = map2 shape_spanline_union amiddle bspanlines in
                    let ah' = (sa, la, atop @ spanlines' @ abottom) in
                      shape_vspans_union (ah'::at) bt
          | ArightB (s, l) ->
              let btop, brest = cleave bspanlines (sa - sb) in
                let spanlines' = map2 shape_spanline_union aspanlines brest in
                  let vspan = (sb, lb, btop @ spanlines') in
                    vspan::shape_vspans_union at bt
          | BrightA (s, l) ->
              let atop, arest = cleave aspanlines (sb - sa) in
                let spanlines' = map2 shape_spanline_union arest bspanlines in
                  let vspan = (sa, la, atop @ spanlines') in
                    vspan::shape_vspans_union at bt
          | ABsame ->
              let spanlines' = map2 shape_spanline_union aspanlines bspanlines in
                let vspan = (sa, la, spanlines') in
                  vspan::shape_vspans_union at bt

let shape_union a b =
  match shapecheck a, shapecheck b with
  | false, false -> failwith "shape_union: two malformed inputs"
  | false, true -> failwith "shape_union: first input malformed"
  | true, false -> failwith "shape_union: second input malformed"
  | _ ->
    let result =
      match a, b with
      | NullShape, NullShape -> NullShape
      | NullShape, b -> b
      | a, NullShape -> a
      | Shape (_, avspans), Shape (_, bvspans) ->
          boxshape (Shape (NoBounds, shape_vspans_union avspans bvspans))
    in
      if shapecheck result then result else
        failwith "shape_union: malformed output"

(* A synonym for [shape_union]. *)
let ( ||| ) = shape_union

(* \section{The difference of two shapes} *)

(* The difference $a\setminus b$ between two shapes. *)
let rec shape_spanline_difference minx maxx a b =
  let shape_spanline_difference = shape_spanline_difference minx maxx in
    match a, b with
    | [], _ -> []
    | ((sa, la) as ah)::at, [] ->
         minx := min !minx sa;
         maxx := sa + la - 1;
         ah::shape_spanline_difference at []
    | ((sa, la) as ah)::at, ((sb, lb) as bh)::bt ->
        let ea = sa + la - 1
        and eb = sb + lb - 1 in
          match overlap ah bh with
          | BthenA | AabutB -> shape_spanline_difference a bt
          | AthenB | BabutA ->
              minx := min !minx sa; maxx := ea;
              ah::shape_spanline_difference at b
          | AoverlapB (s, l) ->
              if sa = sb then
                shape_spanline_difference ((sa + lb, ea - eb)::at) bt
              else
                shape_spanline_difference ((eb + 1, ea - eb)::at) bt
          | BoverlapA (s, l) ->
              if sa = sb then
                shape_spanline_difference at b
              else
                 (minx := min !minx sa; maxx := (sa + (sb - sa) - 1);
                 (sa, sb - sa)::shape_spanline_difference at b)
          | ABsame -> shape_spanline_difference at bt
          | BinsideA (s, l) ->
              minx := min !minx sa; maxx := sa + (sb - sa) - 1;
              (sa, sb - sa)::shape_spanline_difference ((eb + 1, ea - eb)::at) bt
          | AinsideB (s, l) ->
              shape_spanline_difference at ((ea + 1, eb - ea)::bt)
          | ArightB (s, l) ->
              shape_spanline_difference at bt
          | BrightA (s, l) ->
              minx := min !minx sa; maxx := max !maxx (sb - 1);
              (sa, sb - sa)::shape_spanline_difference at bt

(* Find the minimum and maximum of a set of spanlines, some of which may be
blank. *)
let rec minmax_spanlines ymin ymax s spanlines =
  match spanlines with
  | [] -> ymin, ymax
  | []::t -> minmax_spanlines ymin ymax (s + 1) t
  | _::t -> minmax_spanlines (min ymin s) (max ymax s) (s + 1) t

(* Combine equal length lists of spanlines using a spanline function, returning
the new list of spanlines and the minimum and maximum x pixels encountered. We
use references to avoid list reversal *)
let rec shape_vspans_difference_combine xmin xmax f aspanlines bspanlines =
  match aspanlines, bspanlines with
  | [], [] -> []
  | ah::at, bh::bt ->
      let xmin_r = ref max_int and xmax_r = ref min_int in
        let spanline = f xmin_r xmax_r ah bh in
          xmin := min !xmin !xmin_r;
          xmax := max !xmax !xmax_r;
          spanline::shape_vspans_difference_combine xmin xmax f at bt
  | _ -> failwith "Sprite.shape_vspans_difference_combine: bad input"

(* The difference of vspans. *)
let rec shape_vspans_difference_inner xmin xmax ymin ymax a b vspans =
  let reverse_subspans =
    rev_map (function (s, l, spanlines) -> (s, l, rev spanlines))
  in
    match a, b with
    | [], _ -> (reverse_subspans vspans), xmin, xmax, ymin, ymax
    | ((sa, la, aspanlines)::arest), [] ->
        let xmin_cand, xmax_cand = minmax_x_shape_spanlines aspanlines in
          let xmin' = min xmin xmin_cand and xmax' = max xmax xmax_cand in
            let ymin' = min ymin sa and ymax' = max ymax (sa + la - 1) in
              let vspans' = vspan_accumulate_spanlines vspans sa aspanlines in
                shape_vspans_difference_inner xmin' xmax' ymin' ymax' arest b vspans'
    | (sa, la, aspanlines)::at, (sb, lb, bspanlines)::bt ->
        let ea = sa + la - 1 and eb = sb + lb - 1 in
          match overlap (sa, la) (sb, lb) with
          | BthenA | AabutB ->
              shape_vspans_difference_inner xmin xmax ymin ymax a bt vspans
          | AthenB | BabutA ->
              let ymin' = min ymin sa and ymax' = max ymax ea in
                let xmin_cand, xmax_cand = minmax_x_shape_spanlines aspanlines in
                  let xmin' = min xmin xmin_cand and xmax' = max xmax xmax_cand in
                    let vspans' = vspan_accumulate_spanlines vspans sa aspanlines in
                      shape_vspans_difference_inner xmin' xmax' ymin' ymax' at b vspans'
          | AoverlapB (s, l) ->
              let aspanlines' = take aspanlines l
              and bspanlines' = drop bspanlines (sa - sb) in
                let xmin_cand = ref max_int and xmax_cand = ref min_int in
                  let spanlines' =
                    shape_vspans_difference_combine
                      xmin_cand xmax_cand shape_spanline_difference aspanlines' bspanlines'
                  in
                    let ymin', ymax' = minmax_spanlines ymin ymax s spanlines' in
                      let vspans' = vspan_accumulate_spanlines vspans s spanlines'
                      and a' = (sa + l, la - l, drop aspanlines l) in
                        shape_vspans_difference_inner
                          (min xmin !xmin_cand) (max xmax !xmax_cand)
                          ymin' ymax' (a'::at) b vspans'
          | BoverlapA (s, l) ->
              let aonly = take aspanlines (la - l) in
                let vspans' = vspan_accumulate_spanlines vspans sa aonly in
                  let aspanlines' = drop aspanlines (sb - sa)
                  and bspanlines' = take bspanlines l in
                  let xmin_cand = ref max_int and xmax_cand = ref min_int in
                    let spanlines' =
                      shape_vspans_difference_combine
                        xmin_cand xmax_cand shape_spanline_difference
                        aspanlines' bspanlines'
                    in
                      let ymin', ymax' = minmax_spanlines ymin ymax s spanlines' in
                        let ymin'' = min ymin' sa in
                          let vspans'' = vspan_accumulate_spanlines vspans' s spanlines'
                          and b' = (sb + l, lb - l, drop bspanlines l) in
                            let xmin' = min xmin !xmin_cand
                            and xmax' = max xmax !xmax_cand in
                              let xmin_aonly, xmax_aonly =
                                minmax_x_shape_spanlines aonly
                              in
                                let xmin'' = min xmin' xmin_aonly
                                and xmax'' = max xmax' xmax_aonly in
                                  shape_vspans_difference_inner
                                    xmin'' xmax'' ymin'' ymax' at (b'::bt) vspans''
          | ABsame ->
              let xmin_cand = ref max_int and xmax_cand = ref min_int in
                let spanlines' =
                  shape_vspans_difference_combine
                    xmin_cand xmax_cand shape_spanline_difference aspanlines bspanlines
                in
                  let ymin', ymax' = minmax_spanlines ymin ymax sa spanlines' in
                    let vspans' = vspan_accumulate_spanlines vspans sa spanlines' in
                      shape_vspans_difference_inner
                        (min xmin !xmin_cand) (max xmax !xmax_cand)
                        ymin' ymax' at bt vspans'
          | BinsideA (s, l) ->
              let aonly = take aspanlines (sb - sa) in
                let vspans' = vspan_accumulate_spanlines vspans sa aonly in
                  let aspanlines' = take (drop aspanlines (sb - sa)) l in
                    let xmin_cand = ref max_int and xmax_cand = ref min_int in
                      let spanlines' =
                        shape_vspans_difference_combine
                          xmin_cand xmax_cand shape_spanline_difference
                          aspanlines' bspanlines
                      in
              let xmin' = min xmin !xmin_cand
              and xmax' = max xmax !xmax_cand in
                let xmin_aonly, xmax_aonly = minmax_x_shape_spanlines aonly in
                  let xmin'' = min xmin' xmin_aonly
                  and xmax'' = max xmax' xmax_aonly in
                    let _, ymax' = minmax_spanlines ymin ymax s spanlines' in
                      let vspans'' = vspan_accumulate_spanlines vspans' s spanlines'
                      and a' = (sb + l, ea - eb, drop aspanlines (l + (sb - sa))) in
                        shape_vspans_difference_inner xmin'' xmax''
                          (min ymin sa) ymax' (a'::at) bt vspans'' 
          | AinsideB (s, l) ->
              let bspanlines' = take (drop bspanlines (sa - sb)) l in
                let xmin_cand = ref max_int and xmax_cand = ref min_int in
                  let spanlines' =
                    shape_vspans_difference_combine
                      xmin_cand xmax_cand shape_spanline_difference
                      aspanlines bspanlines'
                  in
              let xmin' = min xmin !xmin_cand and xmax' = max xmax !xmax_cand in
                let ymin', ymax' = minmax_spanlines ymin ymax s spanlines' in
                  let vspans' = vspan_accumulate_spanlines vspans s spanlines'
                  and b' = (sa + l, eb - ea, drop bspanlines (l + (sa - sb))) in
                    shape_vspans_difference_inner xmin' xmax' ymin' ymax'
                      at (b'::bt) vspans'
          | ArightB (s, l) ->
              (* Let ABsame handle it *)
              let b' = (s, l, drop bspanlines (sa - sb)) in
                shape_vspans_difference_inner xmin xmax ymin ymax a (b'::bt) vspans
          | BrightA (s, l) ->
              (* Deal with the unoverlapping portion of A, then pass to ABSame *)
              let aonly = take aspanlines (sb - sa) in
                let vspans' = vspan_accumulate_spanlines vspans sa aonly
                and a' = (s, l, drop aspanlines (sb - sa)) in
                  let ymin' = min ymin sa in
                  let xmin_aonly, xmax_aonly = minmax_x_shape_spanlines aonly in
                    let xmin' = min xmin xmin_aonly
                    and xmax' = max xmax xmax_aonly in
                      shape_vspans_difference_inner xmin' xmax' ymin' ymax
                        (a'::at) b vspans'

(* The difference $a\setminus b$ of two shapes. *)
let shape_difference a b =
  match shapecheck a, shapecheck b with
  | false, false -> failwith "shape_difference: two malformed inputs"
  | false, true -> failwith "shape_difference: first input malformed"
  | true, false -> failwith "shape_difference: second input malformed"
  | _ ->
    let result =
      match a, b with
      | NullShape, _ -> NullShape
      | _, NullShape -> a
      | Shape (abounds, avspans), Shape (bbounds, bvspans) ->
          match abounds, bbounds with
          | Box (ax0, ay0, ax1, ay1), Box (bx0, by0, bx1, by1) ->
              (match box_overlap ax0 ay0 ax1 ay1 bx0 by0 bx1 by1 with
               | Some _ ->
                  let vspans, minx, maxx, miny, maxy =
                    shape_vspans_difference_inner
                      max_int min_int max_int min_int avspans bvspans []
                  in
                    (match vspans with
                     | [] -> NullShape
                     | _ -> boxshape (Shape (Box (minx, miny, maxx, maxy), vspans)))
               | None -> a)
          | _ -> failwith "Sprite.shape_difference: ill-formed bounds"
    in
      if shapecheck result then result else
        failwith "shape_difference: output malformed"

(* Synonym for difference. *)
let ( --- ) = shape_difference

(* \section{Intersection of two shapes} *)

(* Intersection of two shapes [a && b]. \smallgap *)

(* Intersect two shape spanlines. *)
let rec shape_spanline_intersection a b =
  match a, b with
  | [], _ | _, [] -> []
  | ah::at, bh::bt ->
      match overlap ah bh with
      | BthenA | AabutB -> shape_spanline_intersection a bt
      | AthenB | BabutA -> shape_spanline_intersection at b
      | AoverlapB (o, o') | BinsideA (o, o') -> (o, o')::(shape_spanline_intersection a bt)
      | BoverlapA (o, o') | AinsideB (o, o') -> (o, o')::(shape_spanline_intersection at b)
      | BrightA (o, o') -> (o, o')::(shape_spanline_intersection at bt)
      | ArightB (o, o') -> (o, o')::(shape_spanline_intersection at bt)
      | ABsame -> ah::(shape_spanline_intersection at bt)

(* Combine two sets of spanlines. We use this when the operator may produce an
empty list from two non-empty lists. [a, b] of equal lengths *)
let combine_spanlines =
  let rec combine_spanlines_inner vspan s a b f = 
   match a, b with
    | [], [] ->
       (match vspan with
        | None -> []
        | Some (vs, vl, spanlines) -> [(vs, vl, rev spanlines)])
    | [], _ | _, [] -> failwith "combine_spanlines: lists not same length"
    | ah::at, bh::bt ->
        (match f ah bh with
        | [] ->
            (match vspan with
             | None -> combine_spanlines_inner None (s + 1) at bt f
             | Some (vs, vl, spanlines) ->
                 (vs, vl, rev spanlines)::
                 (combine_spanlines_inner None (s + 1) at bt f))
        | spanline ->
                let vspan' =
                  (match vspan with
                   | None -> Some (s, 1, [spanline])
                   | Some (vs, vl, spanlines) ->
                       Some (vs, vl + 1, spanline::spanlines))
                in
                  combine_spanlines_inner vspan' (s + 1) at bt f)
  in
    combine_spanlines_inner None

(* Intersect two shape vspan lists. *)
let rec shape_vspan_intersection a b =
  match a, b with
  | ([], _) | (_, []) -> []
  | (sa, la, spanlines_a)::at, (sb, lb, spanlines_b)::bt ->
      match overlap (sa, la) (sb, lb) with
      | BthenA | AabutB -> shape_vspan_intersection a bt
      | AthenB | BabutA -> shape_vspan_intersection at b
      | AoverlapB (s, l) ->
          let spanlines_a', new_ah_spanlines = cleave spanlines_a l
          and spanlines_b' = drop spanlines_b (s - sb) in
            let ah' = (s + l, la - l, new_ah_spanlines)
            and vspans =
              combine_spanlines s spanlines_a' spanlines_b' shape_spanline_intersection
            in
              vspans @ (shape_vspan_intersection (ah'::at) bt)
      | BinsideA (s, l) ->
          let spanlines_a' = drop spanlines_a (sb - sa) in
            let spanlines_a'', new_ah_spanlines = cleave spanlines_a' l
            and ea = sa + la - 1
            and eb = sb + lb - 1 in
              let ah' = (eb + 1, ea - eb, new_ah_spanlines)
              and vspans =
                combine_spanlines s spanlines_a'' spanlines_b shape_spanline_intersection
              in
                vspans @ (shape_vspan_intersection (ah'::at) bt)
      | BoverlapA (s, l) ->
          let spanlines_b', new_bh_spanlines = cleave spanlines_b l
          and spanlines_a' = drop spanlines_a (s - sa) in
            let bh' = (s + l, lb - l, new_bh_spanlines)
            and vspans =
              combine_spanlines s spanlines_a' spanlines_b' shape_spanline_intersection
            in
              vspans @ (shape_vspan_intersection at (bh'::bt))
      | AinsideB (s, l) ->
          let spanlines_b' = drop spanlines_b (sa - sb) in
            let spanlines_b'', new_bh_spanlines = cleave spanlines_b' l
            and ea = sa + la - 1
            and eb = sb + lb - 1 in
              let bh' = (ea + 1, eb - ea, new_bh_spanlines)
              and vspans =
                combine_spanlines s spanlines_a spanlines_b'' shape_spanline_intersection
              in
                vspans @ (shape_vspan_intersection at (bh'::bt))
      | BrightA (s, l) ->
          let spanlines_a' = drop spanlines_a (s - sa) in
            let vspans =
              combine_spanlines s spanlines_a' spanlines_b shape_spanline_intersection
            in
              vspans @ (shape_vspan_intersection at bt)
      | ArightB (s, l) ->
          let spanlines_b' = drop spanlines_b (s - sb) in
            let vspans =
              combine_spanlines s spanlines_a spanlines_b' shape_spanline_intersection
            in
              vspans @ (shape_vspan_intersection at bt)
      | ABsame ->
          let vspans =
            combine_spanlines sa spanlines_a spanlines_b shape_spanline_intersection
          in
            vspans @ (shape_vspan_intersection at bt)

let shape_intersection a b =
  match shapecheck a, shapecheck b with
  | false, false -> failwith "shape_intersection: two malformed inputs"
  | false, true -> failwith "shape_intersection: first input malformed"
  | true, false -> failwith "shape_intersection: second input malformed"
  | _ ->
    let result =
      match a, b with
      | Shape (Box (ax0, ay0, ax1, ay1), a_vspans),
        Shape (Box (bx0, by0, bx1, by1), b_vspans) ->
          begin
            match box_overlap ax0 ay0 ax1 ay1 bx0 by0 bx1 by1 with
             | None -> NullShape
             | Some _ ->
                 begin
                   match shape_vspan_intersection a_vspans b_vspans with
                   | [] -> NullShape
                   | shape -> boxshape (Shape (NoBounds, shape))
                 end
          end
      | Shape (_, a_vspans), Shape (_, b_vspans) ->
         (* Why is this case here? should never happen *)
          begin
            match shape_vspan_intersection a_vspans b_vspans with
            | [] -> NullShape
            | shape -> boxshape (Shape (NoBounds, shape))
          end
      | _ -> NullShape
    in
      if shapecheck result then result else
        failwith "shape_intersection: malformed output"

(* Synonym for intersection *)
let ( &&& ) = shape_intersection

(* Predicate for the intersection of two shapes. This is rather inefficient,
since it produces output it then throws away --- we can bail out as soon as we
find an intersection. *)
let shape_intersects a b =
  shape_intersection a b <> NullShape

(* \section{Writing to and reading from canvases} *)

(* Flatten a sprite to a [Colour.colour array array], using the smallest
dimensions possible. *)
let span_to_array arr x y subspans =
  let subspans = ref subspans and x = ref x in
    for subspan = 1 to length !subspans do
      (match hd !subspans with
        (l, subspancontent) ->
          (match subspancontent with
          | Run col ->
              for x' = !x to !x + l - 1 do arr.(y - 1).(x' - 1) <- col done
          | Samples cols ->
              for x' = !x to !x + l - 1 do arr.(y - 1).(x' - 1) <- cols.(x' - !x) done
          | Interval (o, cols) ->
              for x' = !x to !x + l - 1 do arr.(y - 1).(x' - 1) <- cols.(x' - !x + o - 1) done);
          x := !x + l);
      subspans := tl !subspans
    done

let spanline_to_array arr xoff y spanline =
  let spans = ref spanline in
    for spannumber = 1 to length !spans do
      let (s, l, subspans) = hd !spans in
        span_to_array arr (s - xoff) y subspans;
        spans := tl !spans
    done

let vspan_to_array arr xoff yoff (s, l, spanlines) =
  let lines = ref spanlines in
    for line = s to s + l - 1 do
      spanline_to_array arr xoff (line - yoff) (hd !lines);
      lines := tl !lines
    done

let flatten_sprite border spr col =
  match spr with
  | NullSprite ->
      failwith "flatten: Don't know what to do with null sprite"
  | Sprite (Box (x0, y0, x1, y1), vspans) ->
     let arr =
       Canvas.newcanvasclear (x1 - x0 + 1 + 2 * border) (y1 - y0 + 1 + 2 * border)
     in
       iter (vspan_to_array arr (x0 - 1 - border) (y0 - 1 - border)) vspans;
       arr
  | _ -> failwith "flatten: Malformed sprite"

(* Pick up a sprite from a 2D bigarray given a shape, and [x, y] position,
where [x, y = 1, 1] means no offset. *)
let pickup_span x y l arr =
  let arr' = Array.make l Colour.clear in
    for x' = x to x + l - 1 do
      arr'.(x' - x) <- arr.(y - 1).(x' - 1)
    done;
    arr'

let pickup_spanline x y spanline arr =
  map
    (fun (s, l) -> s, l, [(l, Samples (pickup_span (x + s - 1) y l arr))])
    spanline

let pickup_vspan x y arr (s, l, spanlines) =
  let sprspanlines = ref [] and shpspanlines = ref spanlines in
    for spanlinenum = s to s + l - 1 do
      let spanline = pickup_spanline x (y + spanlinenum - 1) (hd !shpspanlines) arr in
        sprspanlines := spanline::!sprspanlines;
        shpspanlines := tl !shpspanlines
    done;
    (s, l, rev !sprspanlines)

let pickup shp x y arr =
  match shp with
  | NullShape -> NullSprite
  | Shape (Box (x0, y0, x1, y1), shapevspans) ->
      Sprite (Box (x0, y0, x1, y1), map (pickup_vspan x y arr) shapevspans)
  | _ -> failwith "pickup: Malformed shape"

(* \section{Bloating and eroding shapes} *)

(* We wish to be able to `bloat' a shape e.g to give the shape of a blurred
object. This is done by convolving a shape with a box of width [2m + 1] and
height [2n + 1]. This is done by an x-pass followed by a y-pass. The naive way
is too slow, so we use a new algorithm of `Rolling Unions'.\smallgap *)

(* Bloat a shape spanline by the radius [mrad]. *)
let bloat_spanline mrad shpspanline =
  let bloath mrad (s, l) = (s - mrad, l + 2 * mrad) in
    rev (fold_left span_accumulate [] (map (bloath mrad) shpspanline))

(* Make a list of spanlines from a list of vspans, substituting [[]] for each
spanline not in one of the vspans. *)
let rec spanlines_of_vspans vspans =
  match vspans with
  | [] -> []
  | [(_, _, spanlines)] -> spanlines
  | (s, l, spanlines)::((s', l', spanlines')::_ as rest) ->
      spanlines @ (many [] (s' - s - l)) @ spanlines_of_vspans rest

exception GetElts_NotEnough

(* Get the first n elements from a list, throwing [GetElts_NotEnough] if there
are not enough. Returned in reverse order *)
let rec getelts l n =
  if n < 0 then failwith "getelts: negative argument"
  else if n = 0 then [] else
    match l with
    | [] -> raise GetElts_NotEnough
    | h::t -> h::getelts t (n - 1)

(* Build a tree of unions, so as to speed vertical bloating. The tree is
balanced except possibly for the bottom row, which may not fill to the right *)

(* a has half the number of nodes as b (or half of b - 1, if b is odd) *)
(* Returns a forest of trees *)
let build_layer f =
  let getval = function Lf -> [] | Br(v, _, _) -> v in
    pair_ext
      (fun a b -> Br (f (getval a) (getval b), a, b))
      (fun a -> Br (getval a, a, Lf))

let build_real_layer =
  build_layer (shape_spanline_union (ref max_int) (ref min_int))

let build_fake_layer =
  build_layer (fun _ _ -> [])

let rec fake_until_root trees =
  match trees with
  | [] -> failwith "Sprite.fake_until_root: empty forest"
  | [tree] -> tree
  | _ -> fake_until_root (build_fake_layer trees)

(* We must calculate how many layers to do, and then fake the rest of the tree *)
(* We know length(spanlines) >= nrad * 2 + 1 *)
let build_tree spanlines nrad =
  let l = length spanlines in
    let n = log2of (min (pow2lt l) (pow2lt (nrad * 2 + 1))) in
      let real =
        applyn build_real_layer n (map (fun x -> (Br (x, Lf, Lf))) spanlines)
      in
        fake_until_root real

(* Look up a precomputed union in the tree. [treelength] is the width of
the root (the power of two greater than the length of the original list of
spanlines *)
let rec tree_lookup tree treelength toget_length toget_start =
  match tree with
  | Lf -> []
  | Br (v, l, r) ->
      if toget_length = treelength then v else
        let treelength' = treelength / 2 in
          if toget_start <= treelength' then
            tree_lookup l treelength' toget_length toget_start
          else
            tree_lookup r treelength' toget_length (toget_start - treelength')

(* Find the length to ask for in the tree *)
let get_len start length =
  if start = 1 then pow2lt length else
    min (largest_pow2_divisible (start - 1)) (pow2lt length)

(* Calculate a union by reading partial unions from the tree and unioning them
together *)
let rec get_union tree start length numspanlines =
  if length = 0 then [] else
    let toget = get_len start length in
      let looked_up = tree_lookup tree (pow2gt numspanlines) toget start in
        let rest = get_union tree (start + toget) (length - toget) numspanlines in
          shape_spanline_union (ref max_int) (ref min_int) looked_up rest

let rec bloatv nrad spanlines =
  let numspanlines = length spanlines
  and tree = build_tree spanlines nrad in
    let spanlines' = ref [] in
      for start = 1 to numspanlines do
        spanlines' :=
          (get_union tree (start - nrad) (nrad * 2 + 1) numspanlines) :: !spanlines';
      done;
      rev !spanlines'

let bloat_vspans mrad nrad vspans =
  match vspans with
  | [] -> failwith "bloat_vspans: empty vspan list"
  | (vs, _, _) :: _ ->
      let padding = many [] nrad
      and shpspanlines = spanlines_of_vspans vspans in
        let spanlines =
          padding @ (map (bloat_spanline mrad) shpspanlines) @ padding
        in
          rev_map
            vspan_reversespanlines
            (vspan_accumulate_spanlines [] (vs - nrad) (bloatv nrad spanlines))

(* Bloat a shape by x radius [mrad] and y radius [nrad]. *)
let bloat mrad nrad shp =
  match shp with
  | NullShape -> NullShape
  | Shape (Box (x0, y0, x1, y1), vspans) ->
      Shape (Box (x0 - mrad, y0 - nrad, x1 + mrad, y1 + nrad),
        bloat_vspans mrad nrad vspans)
  | _ -> failwith "Sprite.bloat: malformed shape"

(* Erode based upon bloat. Not the optimal way to do it *)
let erode mrad nrad shp =
  match shp with
  | NullShape -> NullShape
  | Shape (Box (x0, y0, x1, y1), _) ->
      let enclosing =
        box (x0 - mrad) (y0 - nrad) (x1 - x0 + 1 + mrad * 2) (y1 - y0 + 1 + nrad * 2)
      in
        let inverse = shape_difference enclosing shp in
          let bloated = bloat mrad nrad inverse in
            shape_difference shp bloated
  | _ -> failwith "Sprite.erode: malformed shape"

(* \section{Calculating vertical sections} *)

(* The [Convolve] module needs a list of horizontal and vertical spans of
a shape in which it is to convolve. Extracting horizontal spans is easy. The
vertical case is not so simple.\smallgap *)

(* Returns a list of [(x, y, length)] tuples, one for each span in the input
shape. *)
let spanlist_of_spanline y spans =
  map (fun (s, l) -> (s, y, l)) spans

let rec spanlist_of_vspan (y, l, spanlines) =
  match l, spanlines with
  | 0, _ -> []
  | _, [] -> failwith "Sprite.spanlist_of_vspan: internal inconsistency"
  | _, (spanline::rest) ->
    (spanlist_of_spanline y spanline) @ (spanlist_of_vspan (y + 1, l - 1, rest))

let spanlist_of_shape shape =
  match shape with
  | NullShape -> []
  | Shape (_, vspans) -> flatten (map spanlist_of_vspan vspans)

(* Returns a list of [(x, y, depth) tuples], one for each 'individual vertical
span' in the shape. *)
let rec depthspanlist_spanlist_of_vspan (y, l, spanlines) =
  match l, spanlines with
  | 0, _ -> []
  | _, [] ->
    failwith "Sprite.depthspanlist_spanlist_of_vspan: inconsistency"
  | _, (spanline::rest) ->
    (y, spanline)::depthspanlist_spanlist_of_vspan (y + 1, l - 1, rest)

let rec depthspanlist_take_vertspan firstx spanlines =
  let depthspan_accumulate p spans =
    match spans with
    | [] -> [(p, 1)]
    | (s, l)::rest -> if p = s - 1 then (s - 1, l + 1)::rest else (p, 1)::spans
  in
    match spanlines with
    | [] -> [], []
    | ((y, ((s, l)::otherspans)) as spanline)::rest ->
        let restlines, restspansout = depthspanlist_take_vertspan firstx rest in
          if s = firstx then
            let spanlines' =
              if l = 1 then
                match otherspans with
                | [] -> restlines
                | _ -> (y, otherspans)::restlines
              else
                (y, ((s + 1, l - 1)::otherspans))::restlines
            and spanout' = depthspan_accumulate y restspansout in
              spanlines', spanout'
          else
            (spanline::restlines), restspansout
    | _ -> failwith "Sprite.depthspanlist_take_vertspan: Malformed."

let find_first_x lines =
  let rec find_first_x_inner sofar lines =
    match lines with
    | [] -> sofar
    | (_, [])::_ -> failwith "Sprite.find_first_x: malformed spanlines"
    | (y, ((s, _)::_))::rest -> find_first_x_inner (min s sofar) rest
  in
    find_first_x_inner max_int lines

(* lines are [(y, spanline)]. *)
let rec depthspanlist_of_spanlines lines vertlines =
  match lines with
  | [] -> vertlines
  | _ -> let firstx = find_first_x lines in
           let lines', vertline = depthspanlist_take_vertspan firstx lines in
             depthspanlist_of_spanlines lines' ((firstx, vertline)::vertlines)

(* Convert [(x, vertspanlist) list] to [(x, y, d) list]. *)
let rec vertspanlines_to_vertspans = function
  | [] -> []
  | (x, [])::rest -> vertspanlines_to_vertspans rest
  | (x, ((s, d)::more))::rest ->
    (x, s, d)::vertspanlines_to_vertspans ((x, more)::rest)

let depthspanlist_of_shape shape =
  match shape with
  | NullShape -> []
  | Shape (_, vspans) ->
     let spanlines =
       flatten ((map depthspanlist_spanlist_of_vspan) vspans)
     in
       let spanlinesout = depthspanlist_of_spanlines spanlines [] in
         vertspanlines_to_vertspans spanlinesout

(* \section{Point-in-shape calculation} *)

(* Set membership test for shapes. *)
let rec point_in_spanline x (y : int) yspanline spans =
  match spans with
  | [] -> false
  | (s, l)::ss ->
      y = yspanline && x >= s && x <= (s + l - 1) || point_in_spanline x y yspanline ss

let rec point_in_vspan x y s spanlines =
  match spanlines with
  | [] -> false
  | sl::sls -> point_in_spanline x y s sl || point_in_vspan x y (s + 1) sls

let rec point_in_shape shp (x, y) =
  match shp with
  | NullShape -> false
  | Shape (NoBounds, _) -> failwith "Sprite.point_in_shape: malformed shape"
  | Shape (Box (x0, y0, x1, y1), vspans) ->
      match vspans with
      | [] -> false
      | (s, _, spanlines)::vs ->
          (not (x < x0 || x > x1 || y < y0 || y > y1)) &&
          (point_in_vspan x y s spanlines ||
            point_in_shape (Shape (Box (x0, y0, x1, y1), vs)) (x, y))

(* \section{Debug to PDF files} *)
let debug_pages = ref []

let debug_pdf = ref (Pdf.empty ())

let debug_sprites = ref []

let add_debug_sprite ?(dx = 0) ?(dy = 0) s =
  (* Temporarily, composit this against a fully white background to make sure no partial transparencies get though until we've fixed the debug code to do soft masks for PDFs properly - presently they're fine on screen but fail on printing *)
  flprint "************** add_debug_sprite\n";
  debug_sprites =|
     let s = sprite_map (fun c -> Colour.over c Colour.white) s in
       translate_sprite dx dy s
    (*i if s = NullSprite then s else
      let canvas = flatten_sprite 0 (translate_sprite dx dy s) Colour.white in
        let w = Canvas.canvas_width canvas
        and h = Canvas.canvas_height canvas in
        Printf.printf "canvas width = %i, height = %i\n" w h;
        Printf.printf "pickup box x y w h = 0, 0, %i, %i\n" w h;
        pickup (box 1 1 w h) 1 1 canvas i*)

let add_debug_shape ?(dx = 0) ?(dy = 0) s =
  add_debug_sprite ~dx ~dy (fillshape s (Fill.plain Colour.black))

let name_num = ref 0

let newname s =
  incr name_num;
  s ^ string_of_int (pred !name_num)

let xobjects_of_sprite = function
  | NullSprite -> None
  | Sprite (Box (x0, y0, x1, y1), _) as sprite ->
      begin let w = x1 - x0 + 1 and h = y1 - y0 + 1 in
        let stream = mkbytes (w * h * 3)
        and alphastream = mkbytes (w * h) in
          fillbytes 255 stream;
          fillbytes 255 alphastream;
          sprite_iter
            (fun y x c ->
              let r, g, b, a =
                (*i 0, 0, 0, 0 i*)
                Colour.unpremul_components c
              and p =
                (y - y0) * w * 3 + (x - x0) * 3
              in
              bset stream p r;
              bset stream (p + 1) g;
              bset stream (p + 2) b;
              bset alphastream (p / 3) a)
            sprite;
            let alphanum =
              (!debug_pdf.Pdf.objects.Pdf.maxobjnum) + 10
            in
            let xobject =
              (* Dictionary and stream *)
              let dict =
                Pdf.Dictionary
                  [("/Type", Pdf.Name "/XObject");
                   ("/Subtype", Pdf.Name "/Image");
                   ("/Length", Pdf.Integer (bytes_size stream));
                   ("/Width", Pdf.Integer w);
                   ("/Height", Pdf.Integer h);
                   ("/ColorSpace", Pdf.Name "/DeviceRGB");
                   ("/BitsPerComponent", Pdf.Integer 8);
                   (*i ("/SMask", Pdf.Indirect alphanum) i*)]
              in
                let stream = Pdf.Stream (ref (dict, Pdf.Got stream)) in
                  Pdfcodec.encode_pdfstream !debug_pdf Pdfcodec.Flate stream;
                  stream
            and alphaxobject =
              let dict =
                Pdf.Dictionary
                  [("/Type", Pdf.Name "/XObject");
                   ("/Subtype", Pdf.Name "/Image");
                   ("/Length", Pdf.Integer (bytes_size alphastream));
                   ("/Width", Pdf.Integer w);
                   ("/Height", Pdf.Integer h);
                   ("/ColorSpace", Pdf.Name "/DeviceGray");
                   ("/BitsPerComponent", Pdf.Integer 8)]
              in
                let stream = Pdf.Stream (ref (dict, Pdf.Got alphastream)) in
                  Pdfcodec.encode_pdfstream !debug_pdf Pdfcodec.Flate stream;
                  stream
            in
              let name = newname "/Im" and alphaname = newname "/Mask" in
                (* Add the xobject to the pdf, and reference it *)
                let xobjnum = Pdf.addobj !debug_pdf xobject in
                  ignore (Pdf.addobj_given_num !debug_pdf (alphanum, alphaxobject));
              Some (name, alphaname, xobjnum, alphanum, x0, y0, x1, y1)
    end
  | _ -> assert false

let add_page_of_sprites sprites caption =
  let font =
    Pdf.Dictionary
      [("/Type", Pdf.Name "/Font");
       ("/Subtype", Pdf.Name "/Type1");
       ("/BaseFont", Pdf.Name "/Times-Italic")]
  and textops =
    Pdfops.stream_of_ops
      [Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (50., 50.)]);
       Pdfops.Op_BT;
       Pdfops.Op_Tf ("/F0", 48.);
       Pdfops.Op_Tj caption;
       Pdfops.Op_ET]
  in
  let items = option_map xobjects_of_sprite sprites in
    let xobjnames, alphanames, xobjnums, alphanums, x0s, y0s, x1s, y1s = split8 items in
      let xobjects =
        Pdf.Dictionary
          (flatten
            (map4
            (fun name alphaname num alphanum ->
              [name, Pdf.Indirect num; (*i alphaname, Pdf.Indirect alphanum i*)])
            xobjnames alphanames xobjnums alphanums))
      in
      let resources =
        Pdf.Dictionary
          [("/XObject", xobjects);
           ("/Font", Pdf.Dictionary [("/F0", font)])]
      in
         let op_dos =
           map (fun n -> Pdfops.Op_Do n) xobjnames
         and scales =
           map4
             (fun x0 y0 x1 y1 ->
                let w = float x1 -. float x0 +. 1.
                and h = float y1 -. float y0 +. 1. in
                  Pdfops.Op_cm
                    (Pdftransform.matrix_compose
                       (Pdftransform.mktranslate (float x0) (1024. -. float y0 -. h))
                       (Pdftransform.mkscale (0., 0.) w h)))
             x0s y0s x1s y1s
         in
           let ops =
             Pdfops.stream_of_ops
               (flatten
                  (map2
                    (fun d s -> [Pdfops.Op_q; s; d; Pdfops.Op_Q])
                    op_dos scales))
           in
             debug_pages =|
               {(Pdfpage.blankpage (Pdfpaper.make Pdfunits.PdfPoint 2560. 1024.)) with
                   Pdfpage.resources = resources;
                   Pdfpage.content = [ops; textops]}

(* Write the whole debug pdf out. *)
let write_debug_pdf name =
  match !debug_pages with
  | [] -> ()
  | ps ->
      let pdf, pagetree_num = Pdfpage.add_pagetree (rev ps) !debug_pdf in
        let pdf = Pdfpage.add_root pagetree_num [] pdf in
          let pdf = {pdf with Pdf.minor = 4} in
            Pdf.remove_unreferenced pdf;
            Pdfwrite.pdf_to_file pdf name;
            debug_pages := []

(* Write any sprites in [debug_sprites] to a new page *)
let write_debug_page caption =
  if !debug_sprites <> [] then
    begin
      add_page_of_sprites !debug_sprites caption;
      debug_sprites := [];
    end



