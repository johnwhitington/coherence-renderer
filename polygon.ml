(* \chaptertitle{Polygon}{Rasterising polygons} *)

(* \epigraph{This is an ex-parrot.}{\textit{John Cleese, 1939--}} *)

(* This is a module which provides for the rendering of some portion of a
polygon to sparse rasters (as defined in module [Sprite]) and associated utility
functions on polygons and \bez\ curves.

For high quality antialiasing, one needs to use something more sophisticated
than a box filter (i.e the calculation of the area of a square surrounding the
pixel which is covered by a polygon).
*)
open Pdfutil

(* \section{Constants and types} *)

(* The accuracy of de Casteljau subdivision of \bez\ curves. Must be [>0].
Lower is more accurate. *)
let curve_accuracy = 0.2

(* Antialiasing resolution (that is, the scale factor for oversampling). *)
let res = 32

(* Antialiasing softness. [2] for standard antialiasing, [<2] for hard/crisp,
[>2] for soft/blurry. *)
let softness = 2.

(* \section{Paths and \bez\ subdivision} *)
let string_of_segment = function
  | Pdfgraphics.Straight ((ax, ay), (bx, by)) ->
      Printf.sprintf "Line: (%f, %f) --> (%f, %f)\n" ax ay bx by
  | Pdfgraphics.Bezier ((ax, ay), (bx, by), (cx, cy), (dx, dy)) ->
      Printf.sprintf
        "Curve: (%f, %f) (%f, %f) (%f, %f) (%f, %f)\n"
        ax ay bx by cx cy dx dy

let string_of_subpath (h, segments) =
  Printf.sprintf "Hole: %b\n" (h = Pdfgraphics.Hole) ^
  fold_left ( ^ ) "" (map string_of_segment segments)

let string_of_path (windingrule, subpaths) =
  begin
    match windingrule with
    | Pdfgraphics.EvenOdd -> "Even-odd\n"
    | Pdfgraphics.NonZero -> "Non-zero\n"
  end ^
  Printf.sprintf "%i subpaths\n" (length subpaths) ^
  fold_left ( ^ ) "" (map string_of_subpath subpaths)

(* Transform a path by the [Transform.transform] [t]. *)
let transform_segment matrix seg =
  let f = Pdftransform.transform_matrix matrix in
    match seg with
    | Pdfgraphics.Straight (a, b) -> Pdfgraphics.Straight (f a, f b)
    | Pdfgraphics.Bezier (a, b, c, d) -> Pdfgraphics.Bezier (f a, f b, f c, f d)

let transform_path t (windingrule, subpaths) =
  let matrix = Pdftransform.matrix_of_transform t in
    let subpaths' =
      map
        (fun (hole, closed, segs) -> hole, closed, map (transform_segment matrix) segs)
        subpaths
    in
    windingrule, subpaths'

(* Make a path from a list of points, joining them together with straight
segments and linking the last to the first to close the path. *)
let path_of_pointlist windingrule points =
  match points with
  | [] | [_] ->
     failwith "Polygon.path_of_pointlist: not enough points."
  | x::_ ->
     let points' = points @ [x] in
       let pointpairs = pairs points' in
         let makestraight = fun (p1, p2) -> Pdfgraphics.Straight (p1, p2) in
           windingrule, [(Pdfgraphics.Not_hole, map makestraight pointpairs)]

(* An edge consists of two points. *)
type edge = {x0: int; x1 : int; y0 : int; y1: int}

(* Convert a \bez\ curve to an edgelist using deCasteljau subdivision,
See the cruft in [bezier_epsilon] below re floating point issues. *)
let distance_point_from_line (cx, cy) ((ax, ay), (bx, by)) =
  let square x = x *. x in
    let l = sqrt (square (bx -. ax) +. square (by -. ay)) in
      let s =
        ((ay -. cy) *. (bx -. ax) -. (ax -. cx) *. (by -. ay)) /. square l
      in
        abs_float s *. l

(* Point `addition', `division' and `multiplication'. *)
let ( ++ ) (x1, y1) (x2, y2) = x1 +. x2, y1 +. y2

let ( // ) (x1, y1) n = x1 /. n, y1 /. n

let ( ** ) (x1, y1) n = x1 *. n, y1 *. n

(* The point between [a] and [b] which splits the line between them into
two parts in the proportions [t] and $1-t$ *)
let partway t (ax, ay) (bx, by) =
  assert (t >= 0. && t <= 1.);
  let t' = 1. -. t in
    t' *. ax +. t *. bx,
    t' *. ay +. t *. by

(* Is a curve flat enough to no longer require subdivision? *)
let bezier_epsilon epsilon p1 p2 p3 p4 =
  let line = p1, p4 in
    let d1 = distance_point_from_line p2 line 
    and d2 = distance_point_from_line p3 line in
      (* Both [<] epsilon, or either degenerate *)
      match classify_float d1, classify_float d2 with
      | FP_normal, FP_normal -> d1 < epsilon && d2 < epsilon
      | _ -> true

let f_accuracy = bezier_epsilon curve_accuracy

(* Subdivide a curve into an edge lists, returning the lists in order. *)
let rec bezier_subdivide f p1 p2 p3 p4 =
  if f p1 p2 p3 p4 then [p1, p4] else
    let half x = x // 2. in
      let l2 = half (p1 ++ p2) and h = half (p2 ++ p3) in
        let l3 = half (l2 ++ h) and r3 = half (p3 ++ p4) in 
          let r2 = half (h ++ r3) in
           let l4 = half (l3 ++ r2) in
             let r1 = l4 in
               bezier_subdivide f p1 l2 l3 l4 @ bezier_subdivide f r1 r2 r3 p4

(* Split a \bez\ curve into two segments in proportions [t] and [1-t] for
$0\leq t\leq 1$ *)
let bezier_split t = function
  | Pdfgraphics.Bezier (p1, p2, p3, p4) ->
      let divide = partway t in
        let l2 = divide p1 p2 and h = divide p2 p3 in
          let l3 = divide l2 h and r3 = divide p3 p4 in 
            let r2 = divide h r3 in
             let l4 = divide l3 r2 in
               let r1 = l4 in
                 Pdfgraphics.Bezier (p1, l2, l3, l4),
                 Pdfgraphics.Bezier (r1, r2, r3, p4)
  | _ -> failwith "Polygon.bezier_split: Unrecognised segment."

(* This next set of functions find the set of points equally spaced on a path. *)
let straightlength (x1, y1) (x2, y2) =
  let sq x = x *. x in
    sqrt (sq (x2 -. x1) +. sq (y2 -. y1))

let bezierlength p1 p2 p3 p4 =
  let edges = bezier_subdivide f_accuracy p1 p2 p3 p4 in
    let f_bezlength = fun (p1, p2) -> straightlength p1 p2 in
      let lengths = map f_bezlength edges in
        fold_left ( +. ) 0. lengths

(* The \bez\ stuff here is not terribly efficient - it will subdivide the whole
   remaining curve for each point generated $O(n^2)$\smallgap *)

(* Returns the point at position pos on the segment, and the right part. It is
assumed that the position does not lie beyound the end of the segment. If it
lies at the end, no right segment is returned *)
let splitat pos = function
  | Pdfgraphics.Straight (p1, p2) ->
      let length = straightlength p1 p2 in
        assert (length > 0.);
        let proportion = pos /. length in
          let p = (p1 ** (1. -. proportion)) ++ (p2 ** proportion) in
            p, if p = p2 then None else Some (Pdfgraphics.Straight (p, p2))
  | _ -> failwith "splitat called with non-straight segment."

let seglength = function
  | Pdfgraphics.Straight (p1, p2) -> straightlength p1 p2
  | Pdfgraphics.Bezier (p1, p2, p3, p4) -> bezierlength p1 p2 p3 p4
  
let rec takelength sep = function
  | [] -> [], None
  | s::ss ->
      let l = seglength s in
        if sep <= l then
          let point, rightpart = splitat sep s in
            (match rightpart with
             | None -> ss
             | Some seg -> seg::ss),
            Some point
          else
            takelength (sep -. l) ss

(* Since we're returning all points, subdivide \bez s to straight segments
immediately. This removes the $O(n^2)$ bad case above. We don't guarantee any
order in the result. The functions above will still be used for cutting paths
apart *)
let rec points_on_subpath sep points segments =
  let segmentsr = ref [] in
    (iter
      (let f_points_on_subpath =
        (fun segment ->
           match segment with
           | Pdfgraphics.Straight _ -> segmentsr := segment::!segmentsr
           | Pdfgraphics.Bezier (p1, p2, p3, p4) ->
               let edges =
                 map
                   (fun (p1, p2) -> Pdfgraphics.Straight (p1, p2))
                   (bezier_subdivide f_accuracy p1 p2 p3 p4)
               in
                 segmentsr := edges @ !segmentsr) in f_points_on_subpath)
      segments);
    (* Now an imperative version *)
    while !segmentsr <> [] do
      let segments', point = takelength sep !segmentsr in
        (match point with
        | None -> ()
        | Some p -> points := p::!points);
        segmentsr := segments'
    done

(* The main function, finding points on a path at seperation \emph{sep}. *)
let points_on_path sep (windingrule, subpaths) =
  let points = ref [] in
    iter (points_on_subpath sep points) (map (function (_, _, s) -> s) subpaths);
    rev !points

(* Calculate the edge list from a subpath. Here we can ignore whether or not
things are holes *)
let edgelist_from_subpath (_, _, s) =
  let f_edgelist_from_subpath =
    (function
      | Pdfgraphics.Straight (a, b) -> [a, b]
      | Pdfgraphics.Bezier (p1, p2, p3, p4) -> bezier_subdivide f_accuracy p1 p2 p3 p4)
  in
    flatten (map f_edgelist_from_subpath s)

(* \section{Preliminary functions on edge lists} *)

(* Projections on edges. [x0] is the x coordinate corresponing to the minimum
[y]. [x1] corresponds to the maximum. [y0] is the y coordinate corresponding to
the minimum [x]. [y1] corresponds to the maximum. *)
let x0in e = if e.y0 > e.y1 then e.x1 else (if e.y1 > e.y0 then e.x0 else min e.x0 e.x1)
and x1in e = if e.y0 > e.y1 then e.x0 else (if e.y1 > e.y0 then e.x1 else max e.x0 e.x1)
and xminin e = min e.x0 e.x1
and xmaxin e = max e.x0 e.x1
and yminin e = min e.y0 e.y1
and ymaxin e = max e.y0 e.y1

(* Reverse Sort an edge list by maximum y coordinate. *)
let sort_edgelist_maxy_rev =
  sort (fun a b -> compare_i (ymaxin b) (ymaxin a))

(* Sort an edge list by minimum x coordinate. *)
let sort_edgelist_minx =
  sort (fun a b -> compare_i (xminin a) (xminin b))

(* Debug printing routines. *)
let print_edge e =
  Printf.printf "(%i, %i), (%i, %i)\n" e.x0 e.y0 e.x1 e.y1

let print_edgelist = iter print_edge

(* Convert a floating-point-defined polygon to subbins *)
let polygon_subbin_of_float =
  let f_polygon_subbin_of_float =
    fun (a, b) -> Coord.sub_of_float a, Coord.sub_of_float b
  in
    map f_polygon_subbin_of_float

(* The same for an edge list. *)
let edgelist_subbin_of_float =
  let f_edgelist_subbin_of_float =
    fun ((a, b), (c, d)) ->
      (Coord.sub_of_float a, Coord.sub_of_float b),
      (Coord.sub_of_float c, Coord.sub_of_float d)
  in
    map f_edgelist_subbin_of_float

let redgelist_subbin_of_float =
  let f_redgelist_subbin_of_float =
    fun ((x0, y0), (x1, y1)) ->
      {x0 = Coord.sub_of_float x0;
       y0 = Coord.sub_of_float y0;
       x1 = Coord.sub_of_float x1;
       y1 = Coord.sub_of_float y1}
  in
    map f_redgelist_subbin_of_float

(* Take all the subpaths, and combines them into one edgelist, which is then
sorted as usual *)
let edgelist_of_path (_, subpaths) =
  let edgelists_float = map edgelist_from_subpath subpaths in
    let edgelists_subbin = map redgelist_subbin_of_float edgelists_float in
      flatten edgelists_subbin

(* Find the floating-point bounding rectangle of a path *)
let path_proper_bounds (_, subpaths) =
  let edges = flatten (map edgelist_from_subpath subpaths) in
    match edges with
    | [] -> failwith "Polygon2.path_proper_bounds: zero edges"
    | ((x0, y0), (x1, y1))::t ->
        let xmin, xmax = ref (fmin x0 x1), ref (fmax x0 x1)
        and ymin, ymax = ref (fmin y0 y1), ref (fmax y0 y1) in
          iter
          (fun ((x0, y0), (x1, y1)) ->
             xmin := fmin !xmin x0; xmin := fmin !xmin x1;
             xmax := fmax !xmax x0; xmax := fmax !xmax x1;
             ymin := fmin !ymin y0; ymin := fmin !ymin y1;
             ymax := fmax !ymax y0; ymax := fmax !ymax y1)
          t;
          !xmin, !xmax, !ymin, !ymax
    
(* \section{Splitting edges for subdivision} *)

(* New type for crossing lists, to include information for non-zero winding
rule. [dir] is [1] for anticlockwise, [-1] for clockwise. *)
type wise = A | C

type crossing = {pos : int; dir : wise}

let val_of_dir = function
  | A -> 1
  | C -> ~-1

let string_of_dir = function
  | A -> "Anticlockwise"
  | C -> "Clockwise"

let print_crossing {pos = pos; dir = dir} =
  Printf.printf "pos = %i, dir = %s\n" pos (string_of_dir dir)

(* Calculate the horizontal crossing of a line passing through a vertical split *)
let crossing_of_line e n =
  {pos = n;
   dir = if e.y1 > e.y0 then A else C}

(* Clip some edges to a maximum and minimum [y] coordinate, returning the
clipped edges, and the [x] coordinates at which they cross the top and bottom *)
let rec clip_yrange2_points top bot edges tops middles bots =
  match edges with
  | [] -> tops, middles, bots
  | (g, edge)::t ->
      let x0 = x0in edge and x1 = x1in edge
      and ymin = yminin edge and ymax = ymaxin edge in
        if ymin > bot || ymax < top then (*r discard *)
          clip_yrange2_points top bot t tops middles bots
        else if ymin = ymax then (*r horizontal *) 
          clip_yrange2_points top bot t tops (edge::middles) bots
        else if ymin >= top && ymax <= bot then (*r nothing to do *)
          clip_yrange2_points top bot t tops (edge::middles) bots
        else
          if ymin >= top then (*r Just bottom clipping *)
            let y = bot in
              let xy =
                toint (float x0 +. g *. (float (y - ymin) +. 0.25) +. 0.5)
              in
                let answer = {x0 = x0; y0 = ymin; x1 = xy; y1 = y} in
                  clip_yrange2_points
                    top bot t tops (answer::middles)
                    (crossing_of_line edge xy::bots)
          else
            if ymax <= bot then (*r Just top clipping *)
              let y = top - 1 in
                let xy =
                  toint (float x0 +. g *. (float (y - ymin) +. 0.25) +. 0.5)
                in
                  let answer = {x0 = xy; y0 = y + 1; x1 = x1; y1 = ymax} in
                    clip_yrange2_points
                      top bot t (crossing_of_line edge xy::tops)
                      (answer::middles) bots
            else
              let topcrossing, edge' = (*r Clip both *)
                let y = top - 1 in
                  let xy =
                    toint (float x0 +. g *. (float (y - ymin) +. 0.25) +. 0.5)
                  in
                    xy, {x0 = xy; y0 = y + 1; x1 = x1; y1 = ymax}
              in
                let middle, botcrossing =
                  let y = bot in
                    let x0 = x0in edge' and ymin = yminin edge' in
                      let xy =
                        toint
                          (float x0 +. g *. (float (y - ymin) +. 0.25) +. 0.5)
                      in
                        {x0 = x0; y0 = ymin; x1 = xy; y1 = y}, xy
                in
                  clip_yrange2_points
                    top bot t
                    (crossing_of_line edge topcrossing::tops)
                    (middle::middles)
                    (crossing_of_line edge botcrossing::bots)

let clip_yrange_points t b e =
  clip_yrange2_points t b e [] [] []

(* \section{Shape, minshape and maxshape calculation} *)

(* Edge list sorted by minimum x coordinate on input. Accumulate a list of
spans (s, l) from some (s, e) pairs presented in increasing order of s. *)
let spanacc spans (s, e) =
  match spans with
  | [] -> [(s, e - s + 1)]
  | (start, l)::rest ->
      let olde = start + l - 1 in
        if s > olde + 1 then
          (s, e - s + 1)::spans
        else
          if e <= olde then spans else (start, e - start + 1)::rest

(* Find the integer bounding box of a polygon given as a path. *)
let rec bounds_polygon (_, subpaths) =
  match subpaths with
  | [] -> failwith "Polygon2.bounds_polygon: Malformed (empty) path"
  | _ ->
    let minx = ref max_int and maxx = ref min_int
    and miny = ref max_int and maxy = ref min_int in
    let updatex n = minx := min !minx n; maxx := max !maxx n
    and updatey n = miny := min !miny n; maxy := max !maxy n in
    let updatex_bez n n' = minx := min !minx n; maxx := max !maxx n'
    and updatey_bez n n' = miny := min !miny n; maxy := max !maxy n'
    and f = Coord.pix_of_float in
      let eachsegment = function
        | Pdfgraphics.Straight ((x0, y0), (x1, y1)) ->
            let x0 = f x0 and x1 = f x1 and y0 = f y0 and y1 = f y1 in
              updatex x0; updatex x1; updatey y0; updatey y1
        | Pdfgraphics.Bezier (p1, p2, p3, p4) ->
            (* Here, we subdivide into straight lines with a flatness of
            1, and add expand the resultant boundary by 1 to account
            for the remaining possible deviation from a straight line in
            each segment *)
            let segments =
               map
                 (fun (x, y) -> Pdfgraphics.Straight (x, y))
                 (bezier_subdivide (bezier_epsilon 1.) p1 p2 p3 p4)
            in
              let bez_minx, bez_maxx, bez_miny, bez_maxy =
                bounds_polygon
                  (Pdfgraphics.NonZero,
                    [(Pdfgraphics.Not_hole, Pdfgraphics.Closed, segments)])
              in
                updatex_bez (pred bez_minx) (succ bez_maxx);
                updatey_bez (pred bez_miny) (succ bez_maxy)
      in
        (* Throw away holes, closures *) 
        iter (iter eachsegment) (map (fun (_, _, s) -> s) subpaths);
        !minx, !maxx, !miny, !maxy

(* Find list of spans representing the pixels whose enclosing square is
intersected by an edge. Return also the minimum and maximum coordinates. *)
let rec coverage_inner spans = function
  | [] -> rev spans
  | e::edges ->
      let xmin = xminin e and xmax = xmaxin e in
        let l = Coord.pix_of_sub (xmin - Coord.halfips)
        and r = Coord.pix_of_sub (xmax + Coord.halfips) in
          coverage_inner (spanacc spans (l, r)) edges

let coverage edgelist =
  coverage_inner [] (sort_edgelist_minx edgelist)

(* Versions for top, bottom as x-coordinate lists. *)
let rec spans_of_edgepoints_inner points spans =
  match points with
  | [] | [_] -> rev spans
  | {pos = p}::{pos = q}::rest ->
      let s = Coord.pix_of_sub (p - Coord.halfips)
      and e = Coord.pix_of_sub (q + Coord.halfips) in
        spans_of_edgepoints_inner rest (spanacc spans (s, e))

let spans_of_edgepoints points =
  spans_of_edgepoints_inner
    (sort (fun {pos = a} {pos = b} -> compare_i a b) points) []

(* Version for antialiasing purposes. *)
let rec spans_of_edgepoints_inner_aa points spans =
  match points with
  | [] | [_] -> rev spans
  | {pos = p}::{pos = q}::rest ->
      let s = Coord.pix_of_sub p
      and e = Coord.pix_of_sub q in
        spans_of_edgepoints_inner_aa rest (spanacc spans (s, e))

let spans_of_edgepoints_aa points =
  spans_of_edgepoints_inner_aa
    (sort (fun {pos = a} {pos = b} -> compare_i a b) points) []

(* A version for nonzero winding rule crossing lists. See \cite{Foley96}. *)
let rec nonzero_findspans_inner points c spans =
  match points with
  | [] | [_] -> rev spans
  | {pos = p; dir = d}::({pos = p'} as r)::r' ->
      let c' = c + (val_of_dir d) in
        if c' <> 0 then
          let s = Coord.pix_of_sub (p - Coord.halfips)
          and e = Coord.pix_of_sub (p' + Coord.halfips) in
            nonzero_findspans_inner (r::r') c' (spanacc spans (s, e))
        else
          nonzero_findspans_inner (r::r') c' spans

let nonzero_findspans points = 
  nonzero_findspans_inner
    (sort (fun {pos = a} {pos = b} -> compare_i a b) points) 0 []

let rec nonzero_findspans_inner_aa points c spans =
  match points with
  | [] | [_] -> rev spans
  | {pos = p; dir = d}::({pos = p'} as r)::r' ->
      let c' = c + (val_of_dir d) in
        if c' <> 0 then
          let s = Coord.pix_of_sub p
          and e = Coord.pix_of_sub p' in
            nonzero_findspans_inner_aa (r::r') c' (spanacc spans (s, e))
        else
          nonzero_findspans_inner_aa (r::r') c' spans

let nonzero_findspans_aa points = 
  nonzero_findspans_inner_aa
    (sort (fun {pos = a} {pos = b} -> compare_i a b) points) 0 []

(* This next set of functions calculate the shape, minshape and maxshape.
\smallgap *)

(* Often we need to calculate the shape and minshape at once. It is faster to
do this in one pass. We calculate the shape, and then remove the coverage from
it to yield the minshape. *)
let shapeminshape_spanline f tops middles bots winding =
  let t = f tops
  and b = f bots
  and dummy = ref 0 in
    let c = coverage middles in
      let tb = Sprite.shape_spanline_union dummy dummy t b in
        let tbc = Sprite.shape_spanline_union dummy dummy tb c in
          let ms = Sprite.shape_spanline_difference dummy dummy tbc c in
            tbc, ms

(* Calculate the gradient of an edge. For ymin = ymax gradient is set to 0 (We
never need the gradient of a horizontal line) *)
let gradient e =
  let denom = ymaxin e - yminin e in
    if denom = 0 then 0., e else
      float (x1in e - x0in e) /. float denom, e

(* This version is for use calculating the shape and minshape. *)
let rec polygon_spanline f y mel ael lines lines_ms winding =
  let top = Coord.left_of_pix y - Coord.halfips in
    let bottom = top + 2 * Coord.ipspacing - 1 in
      let newlyactive, mel' =
        cleavewhile_unordered (fun e -> ymaxin e >= top) mel
      in
        let newlyactive'gradients = map gradient newlyactive in
          let ael' =
            (lose (fun (_, e) -> yminin e > bottom) ael) @ newlyactive'gradients
          in
            match mel, ael' with
            | [], [] ->
              lines, lines_ms, y + 1
            | _ ->
              let tops, middles, bots = clip_yrange_points top bottom ael' in
                let line_shp, line_minshp = f tops middles bots winding in
                  polygon_spanline
                    f (y - 1) mel' ael'
                    (line_shp::lines) (line_minshp::lines_ms) winding

(* We return the vspans for the shape. Minshape function will reaccumulate them
due to possible gaps anyway. Edge list sorted by minimum y upon entry. *)
let polygon f edgelist winding =
   match edgelist with
   | [] -> assert false (* We've already checked this *)
   | e::_ ->
       let starty = Coord.pix_of_sub (ymaxin e + Coord.halfips) in
         let spanlines, spanlines_ms, endy =
           polygon_spanline f starty edgelist [] [] [] winding
         in
           [(endy, length spanlines, spanlines)], [(endy, length spanlines_ms, spanlines_ms)]

(* Calculate the shape and minshape of an edge list at once. *)
let recompress_vspans shape_vspans =
  match shape_vspans with
  | [] -> failwith "internal inconsistency"
  | (s, _, _)::_ ->
      let spanlines =
        flatten (map (fun (_, _, a) -> a) shape_vspans)
      in
        rev_map
          Sprite.vspan_reversespanlines
          (Sprite.vspan_accumulate_spanlines [] s spanlines)

let shapeminshape_of_edgelist edges winding =
  match edges with
  | [] -> Sprite.NullShape, Sprite.NullShape
  | _ ->
      let shapevspans, minshape_vspans =
        let f =
          match winding with
          | Pdfgraphics.NonZero -> nonzero_findspans
          | Pdfgraphics.EvenOdd -> spans_of_edgepoints
        in
          polygon (shapeminshape_spanline f) edges winding
      in
        let shape =
          match recompress_vspans shapevspans with
          | [] -> Sprite.NullShape
          | vspans -> Sprite.boxshape (Sprite.Shape (Sprite.NoBounds, vspans))
        and minshape =
          match recompress_vspans minshape_vspans with
          | [] -> Sprite.NullShape
          | vspans -> Sprite.boxshape (Sprite.Shape (Sprite.NoBounds, vspans))
        in
          shape, minshape

let shapeminshape_polygon path =
  shapeminshape_of_edgelist (sort_edgelist_maxy_rev <| edgelist_of_path path) (fst path)

let shapeminshape_of_unsorted_edgelist edges =
  shapeminshape_of_edgelist (sort_edgelist_maxy_rev edges)

(* \section{Antialiasing} *)

(* Master table---the Gaussian truncated at +3 / -3. Entries multiplied by 255
upon exit to preserve accuracy---we divide the final result out when reading
from the table. *)
let maintable =
  let float_of_tablepos p =
    let scale = float (res - 1) in
      (float (p - 1) *. 6.) /. scale -. 3.
  and sq x = x *. x in
    let table = Array.make_matrix res res 0 in
      for x = 1 to res do
        for y = 1 to res do
          let x' = float_of_tablepos x and y' = float_of_tablepos y in
            table.(x - 1).(y - 1) <-
              toint (exp ~-.((sq x' +. sq y') /. softness) *. 255.)
        done
      done;
      table

(* We calculate the values for edge segments intersecting the filter (or parts
thereof) by numerical integration. Thus, the function [gaussian_mastertable] can
be replaced by any user-defined one. \smallgap *)

(* Calculate the integral of the gaussian over $x\ldots x', y\ldots y'$. *)
let gaussian x x' y y' =
  let t = ref 0 in
    for xp = x - 1 to x' - 1 do
      for yp = y - 1 to y' - 1 do
        t := !t + maintable.(xp).(yp)
      done
    done;
    !t * 256

(* The volume under the entire footprint *)
let volume =
  gaussian 1 res 1 res / 255

(* Extract an opacity from a table value. *)
let opacity_of_tableval t =
  (t + volume / 2) / volume

(* Calculate the value for a span *)
let spanvalue x y l =
  gaussian x (x + l - 1) y y

(* Values in line y with length l *)
let values_of_line_given_width y l =
  Array.of_list (map (function x -> spanvalue x y l) (ilist 1 (res - l + 1)))

(* Values in line y, all of length 1, all of length 2 etc *)
let values_of_line y =
  Array.of_list (map (values_of_line_given_width y) (ilist 1 res))

(* All the values *)
let table =
  Array.of_list (map values_of_line (ilist 1 res))

(* Lookup, given x, y, and length of span *)
let lookup_in_table x y l = 
  table.(y).(l - 1).(x)

let mk_scaled_shape winding = function
  | [] -> Sprite.NullShape
  | edges ->
      let h = res / 2 in
        let scaled_edgelist =
          map
           (fun e ->
             {x0 = e.x0 * h; y0 = e.y0 * h;
              x1 = e.x1 * h; y1 = e.y1 * h})
           edges
        in
          let vspans, _ =
            let f =
              match winding with
              | Pdfgraphics.NonZero -> nonzero_findspans_aa
              | Pdfgraphics.EvenOdd -> spans_of_edgepoints_aa
            in
              polygon (shapeminshape_spanline f) scaled_edgelist winding
          in
            Sprite.boxshape (Sprite.Shape (Sprite.NoBounds, recompress_vspans vspans))

let pixel_coverage scaled_shape x y =
  let dx = ~-(x - 2) * (res / 2)
  and dy = ~-(y - 2) * (res / 2)
  and count = ref 0 in
    let h = res / 2 in
    let minx = (x - 1) * h - h
    and miny = (y - 1) * h - h in
      Sprite.shapespan_iter
        minx (minx + res - 1) miny (miny + res - 1)
        (fun x y l -> count += lookup_in_table (x + dx) (y + dy) l)
        scaled_shape;
      !count

(* Chop everything in a shape before the given y position *)
let remove_up_to y = function
  | Sprite.Shape (Sprite.Box (minx, miny, maxx, maxy), vspans) ->
      let vspans, miny =
        let vspans =
          (* 1. Get rid of completed vspans *)
          dropwhile (fun (s, l, _) -> s + l - 1 < y) vspans
        in
          (* 2. Check to see if current vspan needs chopping *)
          match vspans with
          | [] -> [], 0
          | (s, l, spanlines)::more ->
              if y > s
                then (y, l - (y - s), drop spanlines (y - s))::more, y
                else vspans, s
      in
        if vspans = []
          then Sprite.NullShape
          else Sprite.Shape (Sprite.Box (minx, miny, maxx, maxy), vspans)
  | _ -> Sprite.NullShape

(* Calculate the sprite of a polygon from an edge list. *)
let polygon_sprite_edgelist f shp edges w =
  let scaled_shape = ref (mk_scaled_shape w edges) in
    let fillspan x y l =
      let arr = Array.make l Colour.clear in
        for k = 0 to l - 1 do
          arr.(k) <-
            let coverage = pixel_coverage !scaled_shape (x + k) y
            and colour = f.Fill.fillsingle x y in
              let opacity = opacity_of_tableval coverage in
                Colour.dissolve ~delta:(opacity) colour
        done;
        Fill.Samples arr
    in
      Sprite.map_shape_calling
        (fun y ->
           scaled_shape := remove_up_to (y * (res / 2) - (res / 2)) !scaled_shape)
        (fun (s, l) y -> s, l, [(l, fillspan s y l)])
        shp

(* Calculate the sprite of a polygon, given its fill, shape and path. *)
let polygon_sprite f shp path =
  polygon_sprite_edgelist f shp (sort_edgelist_maxy_rev <| edgelist_of_path path) (fst path)

