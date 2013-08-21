(* \chaptertitle{Shapes}{Stroking lines and making shapes} *)

(* This module provides for the stroking of lines, and production of shape
primitives (circles, regular polygons etc). *)
open Pdfutil
open Printf

(* \section{Common geometric functions} *)

(* The factor by which we multiply the radius to find the length of the bezier
control lines when approximating quarter arcs to make semicircles and circles.
*)
let kappa = ((sqrt 2. -. 1.) /. 3.) *. 4.

(* Calculate rotation from [p] to [p'] about [c] with the shorter arc-length.
When arc-lengths are equal, the result may be either. *)
let rotation (cx, cy) (px, py) (px', py') =
  let px = px -. cx and py = py -. cy
  and px' = px' -. cx and py' = py' -. cy in
    let a = px *. py' -. py *. px'
    and b = px *. px' +. py *. py' in
      atan2 a b

(* The absolute angle to a point [p] from a centre [c]. The angle is the
rotation clockwise (i.e the first quadrant encountered has positive [x] and [y]
values) from East. When the point is [(0, 0)], the result is [0].*)
let angle_to (cx, cy) (px, py) =
  let r = atan2 (py -. cy) (px -. cx) in
    if r < 0. then r +. 2. *. pi else r

(* Restrict an angle [a] to one of those at $s, 2s, 3s\ldots$. We find the two
candidate angles, and see which [a] is numerically closer to. The candidate
points are taken modulo $2\pi$ for this to work. *)
let restrict_angle s a =
  let p = mod_float (floor (a /. s) *. s) (2. *. pi) in
    let p' = mod_float (p +. s) (2. *. pi) in
      if abs_float (p -. a) < abs_float (p' -. a) then p else p'

(* \section{Some Useful Shapes} *)

(* Make a quarter-circle from a single bezier curve from [s] to $(s + \pi / 2)
\bmod 2\pi$ with centre [c] and radius [r]. We cheat by making the standard
quarter from [(1, 0)] to [(0, 1)] and rotating using the [Transform] module.
*)
let quarter s (cx, cy) r =
  let standard_quarter_points =
    [(1., 0.); (1., kappa); (kappa, 1.); (0., 1.)]
  and transform =
    [Pdftransform.Translate(cx, cy);
    Pdftransform.Scale((0., 0.), r, r);
    Pdftransform.Rotate((0., 0.), s)]
  in
    match
      map (Pdftransform.transform transform) standard_quarter_points
    with
    | [p; q; r; s] -> Pdfgraphics.Bezier(p, q, r, s)
    | _ -> failwith "Shapes.quarter: inconsistency"

(* The anticlockwise variant. *)
let quarter_anticlockwise s c r =
  match quarter s c r with
  | Pdfgraphics.Bezier(p, q, r, s) -> Pdfgraphics.Bezier(s, r, q, p)
  | _ -> failwith "Shapes.quarter_anticlockwise: inconsistency"

(* Some of the following functions generate what is supposed to be a connected
list of segments. However, since they operate by calculating each segment
seperately, floating point inaccuracies can arise, making the end of one
segment misalign with the start of the next. This function corrects the defect
by copying the end of one segment to the beginning of the next. We only need to
deal with bezier segments for now. *)
let rec joinsegs segments =
  match segments with
  | [] -> []
  | [x] -> [x]
  | Pdfgraphics.Bezier(_, _, _, d) as s::Pdfgraphics.Bezier(_, b', c', d')::rest ->
      s::joinsegs (Pdfgraphics.Bezier(d, b', c', d')::rest)
  | _ -> failwith "Shapes.joinsegs: Segment not supported"

(* This version sets the start and end points to p1 and p2 respectively. Used
for ensuring round joins join correctly to the rails they connect *)
let joinsegs_ends p1 p2 segments =
  match joinsegs segments with
  | [] -> []
  | [Pdfgraphics.Bezier(a, b, c, d)] -> [Pdfgraphics.Bezier(p1, b, c, p2)]
  | segs ->
    match extremes_and_middle segs with
    | Pdfgraphics.Bezier(_, b, c, d), m, Pdfgraphics.Bezier(a', b', c', _) ->
        Pdfgraphics.Bezier(p1, b, c, d)::m @ [Pdfgraphics.Bezier(a', b', c', p2)]
    | _ -> failwith "Shapes.joinsegs_ends: Segment not supported"

(* The shorter arc made from bezier curves from [p1] to [p2] with centre [c].
The arc is formed from zero or more quarter arcs rotated accordingly, and at
most one partial arc produced by truncating a quarter arc, again rotated. If
[p1=p2], no segments are produced. If the two curves defined by the arguments
are of equal length, the one chosen is undefined. *)
let arc p1 p2 c =
  let ninety = pi /. 2.
  and angletogo = rotation c p1 p2 (*r signed angle to turn through *)
  and abs_angle = angle_to c p1 (*r absolute angle to the first point *)
  and r = distance_between p1 c in (*r radius of the resultant arc *)
    let quarter, ninety_abs =
      if angletogo > 0.
        then quarter, ninety
        else quarter_anticlockwise, ~-.ninety
    in
      let segments = ref []
      and angletogo = ref (abs_float angletogo) (*r Have dealt with sign. *)
      and abs_angle = ref abs_angle in
        while !angletogo > 0. do
          if !angletogo >= ninety then
            begin
              angletogo := !angletogo -. ninety;
              segments := (quarter !abs_angle c r)::!segments;
              abs_angle := mod_float (!abs_angle +. ninety_abs) (2. *. pi)
            end
          else
            (* Calculate a partial arc to finish, if required. *)
            if !angletogo > 0. then
              begin
                let q = quarter !abs_angle c r in
                  let portion_needed = !angletogo /. ninety in
                    let portion, _ = Polygon.bezier_split portion_needed q in
                      segments := portion::!segments;
                angletogo := 0.
              end;
        done;
        joinsegs_ends p1 p2 (rev !segments)

(* Approximate a circle using four bezier curves.*)
let circle x y r =
  Pdfgraphics.NonZero,
    [(Pdfgraphics.Not_hole,
      Pdfgraphics.Closed,
     joinsegs
       [quarter 0. (x, y) r;
       quarter (pi /. 2.) (x, y) r;
       quarter pi (x, y) r;
       quarter (3. *. pi /. 2.) (x, y) r ])]

let rectangle x y w h =
  (Pdfgraphics.EvenOdd,
    ([(Pdfgraphics.Not_hole,
       Pdfgraphics.Closed,
      [Pdfgraphics.Straight ((x, y), (x +. w, y));
       Pdfgraphics.Straight ((x +. w, y), (x +. w, y +. h));
       Pdfgraphics.Straight ((x +. w, y +. h), (x, y +. h));
       Pdfgraphics.Straight ((x, y +. h), (x, y))])]))

(* \section{Stroking Lines} There are two tasks: the calculation (as quickly as
possible) of an edge list from the specification of a stroke; and the
calculation (in the nicest possible form) of the path of its outline. For the
moment, we use identical methods. The lines and curves are calculated with the
usual methods, and joined and capped. The subdivision of curves takes into
account flaring which occurs in wide strokes, but this treatment is not
optimal.

In the longer term, we ought to add the ability to return a path with bezier
curves approximating the offset curves---but this will require lots of thought.

Stroked curves are intended to be identical to those described in the PDF v1.5
specification, in particular with regard to boundary cases such as paths which
consist of a single point, though this has not yet been certified. Paths
generated should be rendered using the nonzero winding rule. \smallgap*)

(* Caps and joins.*)
type cap = ButtCap | RoundCap | ProjectingCap

type join = RoundJoin | MitredJoin | BevelJoin

(* A \emph{stroke specification} consists of the start cap, join, end cap, and
stroke width. *)
type strokespec =
  {startcap : cap;
   join : join;
   endcap : cap;
   mitrelimit : float;
   linewidth : float}

let string_of_join = function
  | RoundJoin -> "RoundJoin"
  | MitredJoin -> "MitredJoin"
  | BevelJoin -> "BevelJoin"

let string_of_cap = function
  | ButtCap -> "ButtCap"
  | RoundCap -> "RoundCap"
  | ProjectingCap -> "ProjectingCap"

let string_of_strokespec s =
  Printf.sprintf "%s-%s-%s, %f, %f" (string_of_cap s.startcap)
  (string_of_join s.join) (string_of_cap s.endcap) s.mitrelimit s.linewidth

(* The following algorithms build up the path representing the outline of a
stoked path. This involves capping and joining together lists of straight
segments representing parts of the line offset either side of the origin. We
call these lists of segments \emph{rails}. They are typically processed in
pairs, representing the offset curves in either direction.\smallgap *)

(* Form a list of polygon segments representing the cap for the end [(p1, p2)]
of a line, given a cap type, width and [perp_vector], a unit vector pointing
outward from the end of the line in its direction. The segment list starts with
a segment starting from [p1] and ends with a segment ending at [p2].*)
let mkcap captype p1 p2 width perp_vector =
  match captype with
  | ButtCap -> [Pdfgraphics.Straight(p1, p2)]
  | ProjectingCap ->
      let perp_vec_half_width = scalevectolength (width /. 2.) perp_vector in
        let p = offset_point perp_vec_half_width p1
        and q = offset_point perp_vec_half_width p2 in
          couple (fun x y -> Pdfgraphics.Straight(x, y)) [p1; p; q; p2]
  | RoundCap ->
      let radius = width /. 2. in
      let midpoint = between p1 p2 in
        let perpscaled = scalevectolength radius perp_vector in
          let centrearc = offset_point perpscaled midpoint
          and control_length = radius *. kappa in
            let lvector = scalevectolength control_length perp_vector
            and cleftvector = scalevectolength control_length (mkvector p2 p1)
            and crightvector = scalevectolength control_length (mkvector p1 p2) in
              let p1_up = offset_point lvector p1
              and p2_up = offset_point lvector p2
              and c_left = offset_point cleftvector centrearc
              and c_right = offset_point crightvector centrearc in
                [Pdfgraphics.Bezier(p1, p1_up, c_left, centrearc);
                Pdfgraphics.Bezier(centrearc, c_right, p2_up, p2)]

(* Print a rail. *)
let print_rail =
  iter
    (function
     | Pdfgraphics.Straight ((a, a'), (b, b')) ->
         printf "%f, %f ---> %f, %f\n" a a' b b'
     | Pdfgraphics.Bezier (a, b, c, d) ->
         printf "bezier...\n")

(* Print the pair of rails coming from a call to [bezier] or [straight]. *)
let print_railsfromseg (r, r') =
  printf "rail:\n"; print_rail r; printf "rail':\n"; print_rail r'

(* Reverse a rail. *)
let reverserail r =
  rev
    (map
      (function
       | Pdfgraphics.Straight (a, b) -> Pdfgraphics.Straight (b, a)
       | Pdfgraphics.Bezier (a, b, c, d) -> Pdfgraphics.Bezier (d, c, b, a))
      r)

(* Produce a capped list of segments, given an open segment as two (not
necessarily equal-length) rails [r] and [r'] consisting of straight segments.
We calculate the endpoints and outward perpendicular vectors. *)
let capsegment strokespec r r' =
  (* Find the four corners and two perpendicular unit vectors of the
  rail ends. *)
    let startcap = strokespec.startcap
    and endcap = strokespec.endcap
    and width = strokespec.linewidth in
    let p1, p4, v, v' =
      match r with
      | [] -> failwith "Shapes.capsegment: empty rail"
      | [(Pdfgraphics.Straight(s, e))] ->
          s, e, mkunitvector e s, mkunitvector s e
      | _ ->
        match extremes r with
        | (Pdfgraphics.Straight(s, m)), (Pdfgraphics.Straight(n, e)) ->
          s, e, mkunitvector m s, mkunitvector n e
        | _ -> failwith "Shapes.capsegment: malformed rail"
    and p2, p3 =
      match r' with
      | [] -> failwith "Shapes.capsegment: empty rail"
      | [(Pdfgraphics.Straight(s', e'))] -> s', e'
      | _ ->
        match extremes r' with
        | (Pdfgraphics.Straight(s, _)), (Pdfgraphics.Straight(_, e)) -> s, e
        | _ -> failwith "Shapes.capsegment: malformed rail"
    in
      let start_segments = mkcap startcap p1 p2 width v
      and end_segments = mkcap endcap p3 p4 width v' in
        start_segments @ r' @ end_segments @ (reverserail r)

(* Where do two lines specified by point, vector pairs [p1, v1] and [p2, v2]
cross?  Assumes lines are not parallel---can throw division by zero or give
incorrect result if they are. Vectors must be non-zero. *)
let crosspoint (px, py) (vx, vy) (px', py') (vx', vy') =
  (* Perpendicular *)
  if vy = 0. && vx' = 0. then px', py
  else if vx = 0. && vy' = 0. then px, py'
  (* One vertical. *)
  else if vx' = 0. then px', (vy /. vx) *. (px' -. px) +. py
  else if vx = 0. then px, (vy' /. vx') *. (px -. px') +. py'
  (* One horizontal. *)
  else if vy' = 0. then (py' -. py) /. (vy /. vx) +. px, py'
  else if vy = 0. then (py -. py') /. (vy' /. vx') +. px', py
  else
    (* General case. Solve equations $y = mx + c$ and $y = m'x + c'$ *)
    let m = vy /. vx and m' = vy' /. vx' in
      let c = py +. (~-.px *. m)
      and c' = py' +. (~-.px' *. m') in
        let p = m /. m' in
          let c'' = c' *. p in
            let ycoeff = 1. -. p in
              let y = (c -. c'') /. ycoeff in
                let x = (c -. y) /. ~-.m in
                  x, y

(* Same with line (rather than vector) inputs. *)
let crosspoint_lines (a, b) (c, d) =
  crosspoint a (mkvector a b) c (mkvector c d)

(* Given two lines and a point, determine if the point definitely does not lie
on either of the lines due to its being outside their bounding boxes. *)
let point_possibly_on_lines ((a, b), (c, d)) ((a', b'), (c', d')) (x, y) =
  let min_x = fmin a c and max_x = fmax a c
  and min_y = fmin b d and max_y = fmax b d
  and min_x' = fmin a' c' and max_x' = fmax a' c'
  and min_y' = fmin b' d' and max_y' = fmax b' d' in
    x >= min_x && x <= max_x && y >= min_y && y <= max_y ||
    x >= min_x' && x <= max_x' && y >= min_y' && y <= max_y'

(* Make a join, given centre point, start point and end point, and the vectors
of start and end. This should not be called if there there is no space for a
join. The join starts at [p1] and ends at [p2]. *)
let rec mkjoin strokespec c p1 p2 v1 v2 =
  match strokespec.join with
  | BevelJoin -> [Pdfgraphics.Straight(p1, p2)]
  | RoundJoin -> arc p1 p2 c
  | MitredJoin ->
      let angle_between = abs_float (rotation c p1 p2)
      and phi = 2. *. asin (1. /. strokespec.mitrelimit) in
        (* Is the mitre limit exceeded? *)
        if angle_between < phi then
          mkjoin {strokespec with join = BevelJoin} c p1 p2 v1 v2
        else
          let cp = crosspoint p1 v1 p2 v2 in
            couple (fun x y -> Pdfgraphics.Straight(x, y)) [p1; cp; p2]

(* Join two pairs of rails, checking first to see if there is space for a join.
Rails must not be null, nor may they have bezier segments at their ends. The
result is a pair of rails representing the joined segments.  If one or other of
the sets of rails is empty, we just return the other.  This allows
[joinsegments] to be applied via [List.fold].  \FIXME{We don't generate the
correct result for the join of two segments of which the second goes completely
back on the first.} *)
let joinsegments strokespec (s1, s2) (s1', s2') =
  match s1, s2, s1', s2' with
  | [], [], [], [] ->
      failwith "Shapes.joinsegments: both sections empty"
  | [], [], _, _ ->
      failwith "Shapes.joinsegments: empty section"
  | _, _, [], [] ->
      failwith "Shapes.joinsegments: empty section"
  | _ ->
    (* Find left rail, left and right join lines, right rails.*)
    let left, ((ab, cd) as left_middle), ((a'b', c'd') as right_middle), right =
      (all_but_last s1, all_but_last s2), (last s1, last s2),
      (hd s1', hd s2'), (tl s1', tl s2')
    in
      let a, b, c, d =
        match ab, cd with
        | Pdfgraphics.Straight(a, b), Pdfgraphics.Straight(c, d) -> a, b, c, d
        | _ -> failwith "joinsegments: Not implemented"
      and a', b', c', d' =
        match a'b', c'd' with
        | Pdfgraphics.Straight(a, b), Pdfgraphics.Straight(c, d) -> a, b, c, d
        | _ -> failwith "joinsegments: Not implemented"
      in
      (* Calculate cross *)
      let lr_cross = crosspoint_lines (a, b) (a', b')
      and l'r'_cross = crosspoint_lines (c, d) (c', d') in
        match
          point_possibly_on_lines (a, b) (a', b') lr_cross,
          point_possibly_on_lines (c, d) (c', d') l'r'_cross
        with
        | (true, true) | (false, false) ->
            (* Either joined already (angle of $\pi$), or goes back on itself. *)
            let middlerail = Pdfgraphics.Straight (a, b')
            and middlerail' = Pdfgraphics.Straight (c, d') in
              fst left @ [middlerail] @ fst right,
              snd left @ [middlerail'] @ snd right
        | false, true ->
            (* Join on rail [lr] *)
            let centre = between b d
            and vl = mkunitvector a b
            and vr = mkunitvector b' a' in
              let join = mkjoin strokespec centre b a' vl vr
              and cross =
                [Pdfgraphics.Straight (c, l'r'_cross);
                 Pdfgraphics.Straight (l'r'_cross, d')]
              in
                fst left @ [fst left_middle] @ join @ [fst right_middle] @ fst right,
                snd left @ cross @ snd right
        | true, false ->
            (* Join on rail [lr'] *)
            let centre = between b d
            and vl = mkunitvector c d
            and vr = mkunitvector d' c' in
              let join = mkjoin strokespec centre d c' vl vr
              and cross =
                [Pdfgraphics.Straight(a, lr_cross);
                 Pdfgraphics.Straight(lr_cross, b')]
              in
                fst left @ cross @ fst right,
                snd left @ [snd left_middle] @ join @ [snd right_middle] @ snd right

(* Calculate pair of rails representing the line $s\rightarrow e$ stroked with
thickness [width]. *)
let straight (s, e) width =
  let offset = perpendicular (mkvector s e) in
    let scaled_offset = scalevectolength (width /. 2.) offset in
      let scaled_offset' = invert scaled_offset in
        let a = offset_point scaled_offset  s
        and b = offset_point scaled_offset' s
        and c = offset_point scaled_offset' e
        and d = offset_point scaled_offset  e in
          [Pdfgraphics.Straight (a, d)], [Pdfgraphics.Straight (b, c)]

(* Stroke a non-degenerate bezier curve segment to a pair of rails. *)
let bezier (p1, p2, p3, p4) width =
  let subdivided =
    Polygon.bezier_subdivide (Polygon.bezier_epsilon Polygon.curve_accuracy) p1 p2 p3 p4
  in
    let points = map fst subdivided @ [snd (last subdivided)] in
      (* Find normal lines associated with each point. *)
      let edges = couple (fun s e -> mkvector s e) points in
        (*i let start_perp_unit, end_perp_unit =
          match edges with
          | [v] -> scalevectolength 1. (invert v), scalevectolength 1. v
          | _ ->
            match List.hd edges, last edges with
              s, e -> scalevectolength 1. (invert s), scalevectolength 1. e
        in i*)
        (* Find the mid-edge offsets *)
        let midedge_offsets = map perpendicular edges in
          let point_offsets = couple between midedge_offsets in
            (* Add start/end ones (now equal numbers of points/offsets) *)
            let point_offsets_complete =
              map
              (scalevectolength (width /. 2.))
              ([hd midedge_offsets] @ point_offsets @ [last midedge_offsets])
            in
              let point_offsets'_complete =
                map invert point_offsets_complete
              in
                let offset_points =
                  map2 offset_point point_offsets_complete points
                and offset'_points =
                  map2 offset_point point_offsets'_complete points
                in
                  let mkrail = couple (fun a b -> Pdfgraphics.Straight (a, b)) in 
                    mkrail offset_points, mkrail offset'_points

(* Calculate an edge list from a path and its stroke specification. *)
let strokesubpath strokespec segments =
  let stroke_segment = function
    | Pdfgraphics.Straight (a, b) -> straight (a, b) strokespec.linewidth
    | Pdfgraphics.Bezier (a, b, c, d) -> bezier (a, b, c, d) strokespec.linewidth
  in
    let rails = map stroke_segment segments in
      let joined_rails = pair_reduce (joinsegments strokespec) rails in
        capsegment strokespec (fst joined_rails) (snd joined_rails)

(* To stroke a whole path. *)
let strokepath_inner spec subpaths =
  let subpaths' = map (fun (_, _, s) -> s) subpaths in
    let stroked_seglists = map (strokesubpath spec) subpaths' in
      map (fun x -> Pdfgraphics.Not_hole, Pdfgraphics.Closed, x) stroked_seglists

(* The functions above are not written to cope with degenerate paths. The PDF
specification defines a degenerate curve as one consisting of a two or more
points at the same position. A degenerate path is not stroked (since the caps
cannot be orientated), except in the case of round caps, when a circle of
diameter equal to the line's width is drawn. In addition, we remove duplicate
points from the middle of curves. This does not change the effect of drawing
the curve, and allows our functions to avoid a number of special
cases.\smallgap *)

(* Is a path degenerate (Single subpath, straight line with points the same or
bezier with all four points the same)? Return the single point if it is. *)
let degenerate = function
  | [_, _, [Pdfgraphics.Straight (p1, p2)]]
      when p1 = p2 -> Some p1
  | [_, _, [Pdfgraphics.Bezier (p1, p2, p3, p4)]]
      when p1 = p2 && p3 = p4 && p2 = p3 -> Some p1
  | _ -> None

(* Clean up a path by removing degenerate cases. The check to see if the whole
path suitable for rendering as just a circle is done later. We remove:
\begin{itemize}
  \item Straight segments with both points the same
  \item Bezier segments with [p1 = p2] or [p3 = p4]
\end{itemize}
*)
let segment_ok = function
  | Pdfgraphics.Straight (p1, p2) when p1 = p2 -> false
  | Pdfgraphics.Bezier (p1, p2, p3, p4) when p1 = p2 || p3 = p4 -> false
  | _ -> true

let clean_subpath (h, c, segments) =
  h, c, keep segment_ok segments

let clean_path subpaths =
  lose (fun (_, _, segs) -> isnull segs) (map clean_subpath subpaths)

(* Stroke a path, checking for degenerate cases and cleaning the path first. *)
let strokepath_polygon spec (_, subpaths) =
  match degenerate subpaths, spec.startcap, spec.endcap with
  | Some (x, y), RoundCap, RoundCap ->
      (* Circle about point *)
      circle x y (spec.linewidth /. 2.)
  | _ ->
    let subpaths' = clean_path subpaths in
      (*i Pdfgraphics.NonZero i*)Pdfgraphics.EvenOdd, strokepath_inner spec subpaths'

(* Like strokepath, but calculates a polygon instead. *)
let strokepath spec path =
  Polygon.sort_edgelist_maxy_rev (Polygon.edgelist_of_path (strokepath_polygon spec path))

(* Find the bounds of a thick stroke given its path, by finding the bounds of
the stroke, and adding the [oversize] in all directions. This is determined by
the types of caps and joins. FIXME: This can be rather too large if the mitre
limit is large. We can be more accurate by actually looking at the angles..*)
let bounds_stroke path strokespec =
  let oversize =
    if strokespec.startcap = ProjectingCap
    || strokespec.endcap = ProjectingCap then strokespec.linewidth
    else strokespec.linewidth /. 2.
  in
    let oversize' =
      if strokespec.join = MitredJoin then
        fmax oversize (strokespec.mitrelimit *. strokespec.linewidth)
      else oversize
    in
      let oversize_i = toint (ceil oversize') in
        match Polygon.bounds_polygon path with
          (minx, maxx, miny, maxy) ->
            let minx = minx - oversize_i
            and maxx = maxx + oversize_i
            and miny = miny - oversize_i
            and maxy = maxy + oversize_i in
              minx, maxx, miny, maxy
