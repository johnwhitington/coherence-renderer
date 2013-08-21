(* \part{Other types of object} *)

(* \chaptertitle{Brush}{Brushes and brushstrokes} *)

(* Brushes and brushstrokes. For now, only stamps at whole pixel
boundaries---must improve. *)
open Pdfutil
open Printf

(* \section{Types and basic functions} *)

(* Different kinds of brush. The brush canvas is of width [2w + 1] and height
[2h + 1] for some [w], [h], so it can be centred properly *)
type brushkind =
  | Dummy of int * int (*r x radius, y radius *)
  | Gaussian of float (*r radius *)

(* A brush consists a pair of its opacity and its brush kind. *)
type brush = float * brushkind

(* A brush stroke is the pair of a brush and a path *)
type brushstroke = brush * Pdfgraphics.path

(* Find the width and height of a brush. *)
let sizeof_brush (_, brushkind) =
  match brushkind with
  | Gaussian r -> let w = toint (ceil r) * 2 + 1 in w, w
  | Dummy (rx, ry) -> rx * 2 + 1, ry * 2 + 1

let translateof = function
  | Pdftransform.Translate (x, y) -> x, y
  | _ -> 0., 0.

let scaleof = function
  | Pdftransform.Scale (_, sx, sy) -> sx, sy
  | _ -> 1., 1.

(* Apply an affine transform to a brush or brushstroke. *)
let transform_brushkind m kind =
  match kind with
  | Gaussian radius ->
      let sx, sy = scaleof m in
        Gaussian (radius *. ((fabs sx +. fabs sy) /. 2.))
  | Dummy (w, h) ->
      let w', h' = float w, float h in
        let sx, sy = scaleof m in
          let w'', h'' = w' *. ((sx +. sy) /. 2.), h' *. ((sx +. sy) /. 2.) in
            Dummy (toint (w'' +. 0.5), toint (h'' +. 0.5))

let transform_brush m (opacity, brushkind) =
  opacity, transform_brushkind m brushkind

let transform_brush_many br ops =
  fold_right transform_brush ops br

let transform_brushstroke m (brush, path) =
  transform_brush_many brush m, Polygon.transform_path m path

(* The standard gaussian function. *)
let g x y r =
  let r = r /. 2. in
    let sq x = x *. x in
      255. *. exp ~-.(sq (x /. r) +. sq (y /. r))

(* Make a round, gaussian brush. *)
let mkround radius opacity =
  opacity, Gaussian radius

(* Make a dummy shadow of a brushstroke *)
let mkdummy (((opacity, brushkind) as brush), path) =
  let w, h = sizeof_brush brush in
    (1., Dummy ((w - 1) / 2, (h - 1) / 2)), path

(* Draw a round gaussian brush. *)
let drawround radius opacity colour =
  assert (radius >= 0.);
  assert (opacity >= 0. && opacity <= 1.);
  let intopacity = toint (opacity *. 255.)
  and intr = toint (ceil radius) in
    let size = intr * 2 + 1 in
      let brush = Canvas.newcanvasclear size size in
        for x = 1 to size do
          for y = 1 to size do
            let x' = x - intr - 1
            and y' = y - intr - 1 in
              brush.(y - 1).(x - 1) <-
                let v = (g (float x') (float y') radius) in
                  let v' = toint (v *. 1.) in
                    assert (v' >= 0 && v' <= 255);
                    Colour.dissolve (Colour.dissolve colour intopacity) v'
          done
        done; brush

(* Draw a given brush. *)
let drawbrush (opacity, brushkind) colour =
  match brushkind with
  | Gaussian radius -> drawround radius opacity colour
  | Dummy _  ->
      failwith "Brush.drawbrush : One cannot draw a dummy brush"

(* Stamp a rectangular brush on a canvas. Fails if brush falls outside canvas *)
let stamp f brushcanvas canvas (x, y) =
  let bwidth = Canvas.canvas_width brushcanvas
  and bheight = Canvas.canvas_height brushcanvas
  and cwidth = Canvas.canvas_width canvas
  and cheight = Canvas.canvas_height canvas in
    assert (odd bwidth && bwidth > 0);
    let startx = x - (bwidth - 1) / 2
    and starty = y - (bheight - 1) / 2
    and endx = x + (bwidth - 1) / 2
    and endy = y + (bheight - 1) / 2 in
      if startx >= 1 && endx <= cwidth && starty >= 1 && endy <= cheight then
        for py = starty to endy do
          let canvas' = canvas.(py - 1) in
            let by = py - starty + 1 in
              let brushcanvas' = brushcanvas.(by - 1) in
                for px = startx to endx do
                  let bx = px - startx + 1 in
                    canvas'.(px - 1) <- f canvas'.(px - 1) brushcanvas'.(bx - 1) bx by
                done
        done
      else failwith "Brush.stamp: Attempted to stamp outside canvas"

(* Find the points needed for a smooth stroke. Attempt to estimate interval
needed based on brush size. \textbf{FIXME: Improve this}. *)
let points_of_brushstroke (brush, path) =
  let interval =
    let w, _ = sizeof_brush brush in (float w /. 20.)
  in
    Polygon.points_on_path interval path

(* \section{Drawing brushstrokes} *)

(* Calculate the shape and minshape of a brush stroke. *)
let rec spanline_of_points points =
  rev (Sprite.spans_accumulate [] (map (fun (x, _) -> (x, 1)) points))

let rec spanlines_of_points points y =
  match points with [] -> [] | _ ->
    let thislinepoints, rest = cleavewhile (fun (_, y') -> y = y') points in
      (spanline_of_points thislinepoints)::spanlines_of_points rest (y + 1)

let shape_of_brushstroke_given_points brush points =
  let w, h = sizeof_brush brush in
    let wr, hr = (w - 1) / 2, (h - 1) / 2 in
      let sorted =
        sort
          (fun (xa, ya) (xb, yb) ->
             let major = compare_i ya yb in
               if major = 0 then compare_i xa xb else major)
          points
      in
        match sorted with
        | [] -> Sprite.NullShape, Sprite.NullShape
        | _ ->
          let miny = snd (hd sorted) in
            let vspans = 
              rev
                (Sprite.vspan_accumulate_spanlines [] miny (spanlines_of_points sorted miny))
            in
              let vspans' =
                map
                  (fun (s, l, spanlines) -> (s, l, rev spanlines))
                  vspans 
              in
                let boxed = Sprite.boxshape (Sprite.Shape (Sprite.NoBounds, vspans')) in
                  let shape = Sprite.bloat wr hr boxed in
                    shape, Sprite.NullShape

let shape_of_brushstroke ((brush, _) as brushstroke) =
  let points = points_of_brushstroke brushstroke in
    let points' = map (fun (x, y) -> toint (x +. 0.5), toint (y +. 0.5)) points in
      shape_of_brushstroke_given_points brush points'

(* Stroke a path with a brush, returning a sprite. *)
let sprite_of_brushstroke ((((opacity, brushkind) as brush), path) as brushstroke) fill shp =
  match brushkind with
  | Dummy _ ->
      Sprite.fillshape
        (fst (shape_of_brushstroke brushstroke)) (Fill.plain Colour.white)
  | _ ->
    match shp with
    | Sprite.NullShape -> Sprite.NullSprite
    | Sprite.Shape (Sprite.NoBounds, _) ->
        failwith "Brush.sprite_of_brushstroke: malformed shape"
    | Sprite.Shape _ ->
        let bw, bh = sizeof_brush brush in
          let bwr, bhr = (bw - 1) / 2, (bh - 1) / 2 in
            let bloated = Sprite.bloat bwr bhr shp
            and points = points_of_brushstroke brushstroke in
              let points' =
                map
                  (fun (x, y) -> (toint (x +. 0.5), toint (y +. 0.5)))
                  points 
              in 
                let points'' = keep (Sprite.point_in_shape bloated) points'
                and bloatedtwice = Sprite.bloat bwr bhr bloated in
                  match bloatedtwice with
                  | Sprite.NullShape | Sprite.Shape (Sprite.NoBounds, _) ->
                      failwith "Brush.sprite_of_brushstroke: internal inconsistency"
                  | Sprite.Shape (Sprite.Box (x0, y0, x1, y1), _) ->
                      let canvas = Canvas.newcanvasclear (x1 - x0 + 1) (y1 - y0 + 1) in
                         (* Paint with brush at each point. *)
                         let pointlist' =
                           map (fun (x, y) -> (x - x0 + 1), (y - y0 + 1)) points''
                         in
                           iter
                             (stamp
                               (fun a b _ _ -> Colour.alpha_over a b)
                               (drawbrush brush Colour.white)
                               canvas)
                             pointlist';
                         (* Apply the fill, in place on the canvas. *)
                         (Sprite.shape_iter
                             (fun y x ->
                                canvas.(y - y0).(x - x0) <-
                                  Colour.dissolve
                                    (fill.Fill.fillsingle x y)
                                    (Colour.alpha_of_colour canvas.(y - y0).(x - x0)))
                             shp);
                         (* Pick the canvas up and return. *)
                         Sprite.pickup shp (~-x0 + 2) (~-y0 + 2) canvas

(* The effective bounds of a brushstroke. *)                       
let bounds_brushstroke brushstroke =
  let width, height =
    match sizeof_brush (fst brushstroke) with
      w, h -> w / 2, h / 2
  in
    match Polygon.bounds_polygon (snd brushstroke) with
      minx, maxx, miny, maxy ->
         minx - width, maxx + width, miny - height, maxy + height

(* \section{Smearing with a brush} *)

(* Smearing. This needs more work. *)

(* This version needs to subdivide to points on adjacent pixels. *)
let points_of_brushstroke_smear (_, path) =
  let adjacent =
    fun p1 _ _ p4 ->
      distance_between p1 p4 <= 2.
  in
    let points_of_segment = function
      | Pdfgraphics.Straight (p1, p2) ->
          let p = between p1 p2 in
            Polygon.bezier_subdivide adjacent p1 p p p2
      | Pdfgraphics.Bezier (p1, p2, p3, p4) ->
          Polygon.bezier_subdivide adjacent p1 p2 p3 p4
    in
      let segs =
        flatten (map points_of_segment ((fun p -> flatten (map (fun (_, _, s) -> s) (snd p))) path))
      in
        map fst segs

let drop_duplicates = function
  | [] -> []
  | l ->
      let l' = ref [hd l] in
        iter (fun x -> if x <> hd !l' then l' =| x) (tl l);
        rev !l'

let find_smear_directions = function
  | [] -> []
  | points ->
      let sgn = function
        | x when x > 0 -> ~-1
        | x when x < 0 -> 1
        | _ -> 0
      in
        let intpoints = map (fun (x, y) -> toint x, toint y) points in
          let intpoints' = drop_duplicates intpoints in
            let dirs =
              couple
                (fun (x, y) (x2, y2) -> (sgn (x2 - x), sgn (y2 - y)))
                (hd intpoints'::intpoints')
            in
              combine intpoints' dirs

(* Smear a sprite using a given brush *)
let smear spr ((brush, path) as brushstroke) =
  (* Flesh sprite out to shape of brush *)
  let spr =
    let shp = fst (shape_of_brushstroke brushstroke) in
      let spr' = Sprite.fillshape shp (Fill.plain Colour.clear) in
        fst (Sprite.caf Colour.over Colour.opaque spr spr')
  in
    match spr with
      Sprite.NullSprite -> spr | _ -> 
        let brushx, brushy = sizeof_brush brush in
          let radx, rady = (brushx - 1) / 2, (brushy - 1) / 2 in
            (* 1. Calculate points shape *)
            let points = points_of_brushstroke_smear brushstroke in
              match points with [] -> spr | _ ->
                (* 2. Calculate the direction pairs associated with each one *)
                let points_with_directions = find_smear_directions points in
                  let xoff, yoff =
                    match spr with
                      Sprite.NullSprite | Sprite.Sprite (Sprite.NoBounds, _) ->
                        failwith "smear: Internal inconsistency"
                    | Sprite.Sprite (Sprite.Box (x0, y0, x1, y1), _) -> x0, y0
                  in
                   (* 4. Create canvas and accumulation canvas *)
                   let canvas = Sprite.flatten_sprite 1 spr Colour.clear
                   and opacbrush = drawbrush brush (Colour.dissolve Colour.white 255)
                   and brush = Canvas.newcanvas brushx brushy in
                     (* 5. Smear each one. *)
					   for x = 1 to 2 do
                       iter
                         (fun ((x, y), (dx, dy)) ->
						   try
                             let x = x - xoff + 1 and y = y - yoff + 1 in
                               (* 1. Read brush *)
                               Canvas.subcopy canvas brush (x - radx + 1 - dx) (y - rady + 1 - dy) brushx brushy;
                               (* 2. Blur *)
                               let blurred = brush in 
                                 (* 3. Stamp brush *)
                                 stamp
                                   (fun a b brushx brushy ->
                                      Colour.dissolve_between b a
                                        (Colour.alpha_of_colour
                                           opacbrush.(brushy - 1).(brushx - 1)))
                               blurred canvas (x + 1, y + 1)
							 with
							   _ -> () (*FIXME - subcopy fails*)
					       )
                         points_with_directions
						 done;
                     (* 6. Pick up sprite *)
                     let shp = Sprite.shape_of_sprite spr in
                       Sprite.pickup shp (~-xoff + 2 + 1) (~-yoff + 2 + 1) canvas

