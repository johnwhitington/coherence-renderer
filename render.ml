(* \chaptertitle{Renderscene}{Making and rendering scenes} *)
open Printf
open Bigarray
open Pdfutil

let pdf_debug_active = ref false

let pdf_filter_debug_active = ref false

(* Import the basic sprite operations, avoiding the use of [open]. \verb+---+ binds
tighter than \verb+|||+ binds tighter than \verb+&&&+. *)
let ( --- ) = Sprite.( --- )
let ( &&& ) = Sprite.( &&& )
let ( ||| ) = Sprite.( ||| )

(* \section{Types for objects, scenes and filters} *)

(* Constructive Planar Geometry operations. *)
type cpg_op =
  Union | Intersection | Subtraction | ExclusiveOr

(* Resolution independent primitives for pages, grids etc. Specified in floats
for continuity. *)
and primitive =
  | HLine of float * float * float (*r [y, xmin, xmax] *)
  | VLine of float * float * float (*r [x, ymin, ymax] *)
  | Rectangle of float * float * float * float (*r [xmin, ymin, xmax, ymax] *)

and basicshape =
  | Path of Pdfgraphics.path
  | Brushstroke of Brush.brushstroke
  | StrokedPath of Pdfgraphics.path * Shapes.strokespec
  | CPG of cpg_op * basicshape * basicshape

and filter_function =
  Sprite.sprite -> renderobject -> Sprite.shape -> Sprite.sprite

(* In: current dirty region, filter renderobject *)
and dirty_function =
  Sprite.shape -> renderobject -> Sprite.shape

(* In: shptorender, idset of object, whole renderobject, whole scene.
 Returns 1. Reading shape, 2. Modified shptorender, 3. Modified scene. *)
and reading_scene_function =
  Sprite.shape -> Id.idset -> renderobject -> scene ->
  (Sprite.shape * Sprite.shape * scene)

and filterkind = FilterPlain | FilterFancy

and filter =
  {geometry : geometry;
  reading_scene : reading_scene_function;
  filter : filter_function;
  dirty : dirty_function;
  filterkind : filterkind}

and geometry =
  | Filter of filter
  | Basic of Fill.fill * basicshape
  | Convolved of Convolve.kernel * geometry
  | Group of scene
  | Primitive of Colour.colour * primitive

(* Compositing operators *)
and compop =
  | NoCover
  | Over
  | PreTrans of float * compop

(* For now, anything which isn't a basic shape should have an identity
transform. We'll re-evaluate this decision later. *)
and renderobject =
  Obj of Id.idset * geometry * Pdftransform.transform * compop

and scene = renderobject list

let string_of_basicshape = function
  | Path p -> "Path " ^ Pdfgraphics.string_of_path p
  | Brushstroke _ -> "brushstroke"
  | StrokedPath (p, _) -> "Stroked Path " ^ Pdfgraphics.string_of_path p
  | CPG (_, _, _) -> "CPG"

let rec string_of_geom = function
  | Filter _ -> "filter"
  | Basic (_, b) -> "basicshape\n" ^ string_of_basicshape b
  | Convolved (_, g) -> "convolved: " ^ string_of_geom g
  | Group _ -> "group"
  | Primitive (_, _) -> "primitive"

let string_of_obj (Obj (id, geom, _, _)) =
  ("Object " ^ Id.string_of_idset id ^ "\n" ^ string_of_geom geom ^ "\n")

let print_renderobj o =
  flprint (string_of_obj o ^ "\n")

(* \section{Utility functions} *)

(* Project the ID set from an object *)
let idset_in (Obj (i, _, _, _)) = i

(* Are two objects the same? *)
let obj_eq o o' =
  Id.set_eq (idset_in o) (idset_in o')

(* Make a group from individual objects. *)
let mkgroup objs =
  let idset = Id.new_ids () in
    Obj (idset, Group objs, Pdftransform.i, Over)

let translate dx dy points =
  map (fun (x, y) -> x +. dx, y +. dy) points

let fakeobj geom =
  Obj (Id.new_ids (), geom, Pdftransform.i, NoCover)

let fakeobj_t geom t =
  Obj (Id.new_ids (), geom, t, NoCover)

let transform_in (Obj (_, _, t, _)) =
  t

(* A primitive object of colour [c] and thing [p]. *)
let primobj c p = 
  Obj (Id.new_ids (), Primitive (c, p), Pdftransform.i, Over)

(* This is set when we're under any filter (i.e in the filter's modified
scene). Currently only used for ghost sprite production for demo purposes. *)
let underanyfilter = ref false

(* Extract the kind of filter from an object. *)
let filterkind_in (Obj (_, geom, _, _)) =
  match geom with
  | Filter {filterkind = filterkind} -> Some filterkind
  | _ -> None

(* Assign fill types to geometries, for use when calculating dirty regions. *)
let rec findfill = function
  | Filter _ -> Fill.Fancy
  | Basic ({Fill.fillkind = fillkind}, _) -> fillkind
  | Convolved (_, g) -> findfill g
  | Group _ -> Fill.Fancy
  | Primitive _ -> Fill.Plain

(* Does an object contain a filter anywhere within it? *)
let rec containsfilter (Obj (_, geom, _, _)) =
  match geom with
  | Filter _ -> true
  | Group objs -> fold_left ( || ) false (map containsfilter objs)
  | _ -> false 

(* Find the filltype of an object. *)
let rec fillkind_in (Obj (_, geom, _, _)) =
  match geom with
  | Basic (fill, _) -> fill.Fill.fillkind
  | Group _ -> Fill.Fancy
  | Convolved (_, g) ->
      fillkind_in (Obj (Id.new_ids (), g, Pdftransform.i, NoCover))
  | Filter {geometry = geometry} ->
      fillkind_in (Obj (Id.new_ids (), geometry, Pdftransform.i, NoCover))
  | Primitive _ -> Fill.Plain

(* Find the fill of an object, or return the fancy dummy fill if it's a group. *)
let rec fillin_obj (Obj (_, geom, _, _)) =
  match geom with
  | Basic (fill, _) -> fill
  | Group _ -> Fill.dummyfancy
  | Convolved (_, g) ->
      fillin_obj (Obj (Id.new_ids (), g, Pdftransform.i, NoCover))
  | Filter{geometry = geometry} ->
      fillin_obj (Obj (Id.new_ids (), geometry, Pdftransform.i, NoCover))
  | Primitive _ -> Fill.dummyfancy

(* Some functions for making various kinds of objects from primitives defined
in other objects. \smallgap *)

(* Make a polygon *)
let mkpoly path fill transform compop =
  Obj (Id.new_ids (), Basic (fill, Path path), transform, compop)

(* Make a rectangle object. *)
let mkrectangle x y w h =
  mkpoly
    (Shapes.rectangle x y w h)

(* Make a rectangle basic shape. *)
let rectangle x y w h =
  Path
    (Shapes.rectangle x y w h)

(* \section{Transforming renderobjects, shapes and parts of shapes} *)
let rec transform_shapespec f spec =
  let scale, _, _, _, _, _ =
    Pdftransform.decompose (Pdftransform.matrix_of_transform f)
  in
    {spec with Shapes.linewidth = spec.Shapes.linewidth *. fabs scale}

and transform_shapekind f = function
  | Path p ->
      Path (Polygon.transform_path f p)
  | Brushstroke b ->
      Brushstroke (Brush.transform_brushstroke f b)
  | StrokedPath (path, spec) ->
      StrokedPath (Polygon.transform_path f path, transform_shapespec f spec)
  | CPG (op, a, b) ->
      CPG (op, transform_shapekind f a, transform_shapekind f b)

(* Transform a renderobject by a [Transform.transform_op] *)
and transform_renderobject f (Obj (idset, geom, transform, compop)) =
  match geom with
  (* If it's a group, go inside, since groups always have identity transforms.
  FIXME: What about convolved groups? *)
  | Group objs ->
     Obj (Id.new_ids (), Group (map (transform_renderobject f) objs), transform, compop)
  | _ ->
    let transform' = Pdftransform.compose f transform in
      Obj (Id.new_ids (), geom, transform', compop)

(* Transform a renderobject by a [Transform.transform]. *)
and transform_renderobject_many tr (Obj (idset, geom, transform, compop)) =
  match geom with
  (* If it's a group, go inside, since groups always have identity transforms.
  FIXME: What about convolved groups? *)
  | Group objs ->
      (Obj (Id.new_ids (), Group (map (transform_renderobject_many tr) objs), transform, compop))
  | _ ->
    let transform' = Pdftransform.append tr transform in
      Obj (Id.new_ids (), geom, transform', compop)

(* Transform a basicshape by list of transformation 'f' *)
and transform_basicshape f = function
  | Filter filter ->
      Filter {filter with geometry = transform_basicshape f filter.geometry}
  | Basic (fill, shapekind) ->
      Basic (fill.Fill.filltransform f fill, transform_shapekind f shapekind)
  | Convolved (k, Group objs) ->
      Convolved (k, Group (map (transform_renderobject_many f) objs))
  | Convolved (k, b) -> Convolved (k, transform_basicshape f b)
  | Group _ -> failwith "transform_basicshape: This is a group"
  | Primitive _ -> failwith "transform_basicshape: This is a primitive"

(* Transforming primitives. Should never be given a transformation which
contains anything other than scaling and translation. *)
and transform_primitive f p =
  let tr = Pdftransform.transform f in
    match p with
    | HLine (y, xmin, xmax) ->
        let xmin', y' = tr (xmin, y) and xmax', _ = tr (xmax, y) in
          HLine (y', xmin', xmax')
    | VLine (x, ymin, ymax) ->
        let x', ymin' = tr (x, ymin) and _, ymax' = tr (x, ymax) in
          VLine (x', ymin', ymax')
    | Rectangle (xmin, ymin, xmax, ymax) ->
        let xmin', ymin' = tr (xmin, ymin) and xmax', ymax' = tr (xmax, ymin) in
          Rectangle (xmin', ymin', xmax', ymax')

(* Transform a renderobject so it can be used as a translation in the cache. We
must add the translation to the cache, and those of constituent parts of a
group *)
let rec translate_renderobject
  dx dy (Obj (idset, geometry, transform, compop) as obj)
=
  let tr = Pdftransform.Translate (float dx, float dy) in
    let transform', geometry' =
      match geometry with
  	  | Group objs -> Pdftransform.i, Group (map (translate_renderobject dx dy) objs)
	  | _ -> Pdftransform.compose tr transform, geometry
    in
      let idset' = Id.new_ids () in
        if not (containsfilter obj) then
          Cache.addtranslation idset' idset dx dy;
        Obj (idset', geometry', transform', compop)

(* \section{Finding bounding boxes for an object} *)

(* Build a list of [Pdfgraphics.segment] elements from a list of points. *)
let segmentlist_of_points = function
  | [] | [_] ->
    failwith "Clip.segmentlist_of_points: Insufficient points"
  | points ->
    let points' = map (fun v -> v.Clip.x, v.Clip.y) points in
      couple
        (fun (a, b) (c, d) -> Pdfgraphics.Straight ((a, b), (c, d)))
        (points' @ [hd points']) 

(* \intf Build a Pdfgraphics.path from a gpc polygon. *)
let path_of_gpcpolygon windingrule ((_, holes, contours) : Clip.gpc_polygon) =
  let holes' =
    map
      (function
       | 1 -> Pdfgraphics.Hole
       | 0 -> Pdfgraphics.Not_hole
       | _ -> failwith "Clip.path_of_gpcpolygon: Bad hole value")
      (Array.to_list holes)
  in
    let contours' =
      map Array.to_list (map snd (Array.to_list contours))
    in
      let segments = map segmentlist_of_points contours' in
        windingrule, combine3 holes' (many Pdfgraphics.Closed (length holes')) segments

(* Give the proper (floating point) bounds of an object when transformed. These
do not necessarily fully contain the object --- they are aesthetic. For
example, the bounds of a stroke are the bounds of its defining line. We might
want to introduce another, real, floating point box function for when we wish
to calculate the outline once all effects have been applied. Returns [xmin,
xmax, ymin, ymax]. *)
let rec proper_bounds_geom = function
  | Filter ({geometry = geometry}) -> proper_bounds_geom geometry
  | Convolved (_, b) -> proper_bounds_geom b
  | Primitive (_, p) ->
      begin
        match p with
        | HLine (y, xmin, xmax) -> xmin, xmax, y, y
        | VLine (x, ymin, ymax) -> x, x, ymin, ymax
        | Rectangle (xmin, ymin, xmax, ymax) -> xmin, xmax, ymin, ymax
      end
  | Basic (_, (Path p | Brushstroke (_, p) | StrokedPath (p, _))) ->
      Polygon.path_proper_bounds p
  | Group [] ->
      raise (Failure "Empty groups aren't allowed")
  | Group objs ->
      let bounds = map proper_bounds objs in
        pair_reduce
        (fun
           (a_xmin, a_xmax, a_ymin, a_ymax)
           (b_xmin, b_xmax, b_ymin, b_ymax)
         ->
           let poly_a = Clip.gpc_polygon_of_box a_xmin a_xmax a_ymin a_ymax
           and poly_b = Clip.gpc_polygon_of_box b_xmin b_xmax b_ymin b_ymax in
             let poly' = Clip.gpcml_clippolygon Clip.Union poly_a poly_b in
               proper_bounds_geom
                 (Basic (Fill.dummy, Path (path_of_gpcpolygon Pdfgraphics.EvenOdd poly'))))
        bounds
  | Basic(_, CPG (op, a, b)) ->
      let a_xmin, a_xmax, a_ymin, a_ymax =
        proper_bounds_geom (Basic (Fill.dummy, a))
      and b_xmin, b_xmax, b_ymin, b_ymax =
        proper_bounds_geom (Basic (Fill.dummy, b))
      in
        let gpc_op =
          match op with
          | Union | Subtraction | ExclusiveOr -> Clip.Union
          | Intersection -> Clip.Intersection
        in
          let poly_a = Clip.gpc_polygon_of_box a_xmin a_xmax a_ymin a_ymax
          and poly_b = Clip.gpc_polygon_of_box b_xmin b_xmax b_ymin b_ymax in
            let poly' = Clip.gpcml_clippolygon gpc_op poly_a poly_b in
              proper_bounds_geom
                (Basic (Fill.dummy, Path (path_of_gpcpolygon Pdfgraphics.EvenOdd poly')))

and proper_bounds (Obj (_, geom, transform, _)) =
  (* find the bounds of the untransformed object, then transform the result *)
  let xmin, xmax, ymin, ymax = proper_bounds_geom geom in
    let pt1 = (xmin, ymin)
    and pt2 = (xmin, ymax)
    and pt3 = (xmax, ymax)
    and pt4 = (xmax, ymin) in
      let p1x, p1y = Pdftransform.transform transform pt1
      and p2x, p2y = Pdftransform.transform transform pt2
      and p3x, p3y = Pdftransform.transform transform pt3
      and p4x, p4y = Pdftransform.transform transform pt4 in
        fmin (fmin p1x p2x) (fmin p3x p4x),
        fmax (fmax p1x p2x) (fmax p3x p4x),
        fmin (fmin p1y p2y) (fmin p3y p4y),
        fmax (fmax p1y p2y) (fmax p3y p4y)

(* Calculate integer bounding boxes for shapes. Not guaranteed minimal. *)
let rec bounds_of_basicshape (Obj (idset, geom, f_transform, compop)) =
  match geom with
  | Filter {geometry = g} ->
      bounds_of_basicshape (Obj (idset, g, f_transform, compop))
  | Convolved (k, Group objs) ->
      (* We need to compose transforms here *)
      let objs'=
        map
          (fun (Obj (idset, geom, f_transform', compop)) ->
             (Obj (idset, geom, Pdftransform.append f_transform f_transform', compop)))
          objs
      in
        let bounds = map bounds_of_basicshape objs'
        and width = Convolve.radius_of_kernel k in
          (match pair_reduce box_union bounds with
           xmin, xmax, ymin, ymax -> xmin - width, xmax + width, ymin - width, ymax + width)
  | Convolved (k, s) ->
      let width = Convolve.radius_of_kernel k in
        (match bounds_of_basicshape (fakeobj_t s f_transform) with
           xmin, xmax, ymin, ymax -> xmin - width, xmax + width, ymin - width, ymax + width)
  | Primitive (_, p) ->
      let xmin, xmax, ymin, ymax =
        begin match p with
        | HLine (y, xmin, xmax) -> toint xmin, toint xmax, toint y, toint y
        | VLine (x, ymin, ymax) -> toint x, toint x, toint ymin, toint ymax
        | Rectangle (xmin, ymin, xmax, ymax) ->
            toint xmin, toint xmax, toint ymin, toint ymax
        end
      in
        xmin, xmax, ymin, ymax
  | Group objs ->
      (* We need to compose transforms here *)
      let objs'=
        map
          (fun (Obj (idset, geom, f_transform', compop)) ->
             (Obj (idset, geom, Pdftransform.append f_transform f_transform', compop)))
          objs
      in
        let bounds = map bounds_of_basicshape objs' in
          pair_reduce box_union bounds
  | Basic (_, shapekind) ->
      match transform_shapekind f_transform shapekind with
      | Path path -> Polygon.bounds_polygon path
      | Brushstroke brushstroke -> Brush.bounds_brushstroke brushstroke
      | StrokedPath (path, spec) -> Shapes.bounds_stroke path spec
      | CPG (op, a, b) ->
        let a_xmin, a_xmax, a_ymin, a_ymax =
          bounds_of_basicshape
            (Obj (Id.new_ids (), (Basic (Fill.dummy, a)), Pdftransform.i, compop))
        and b_xmin, b_xmax, b_ymin, b_ymax =
          bounds_of_basicshape
            (Obj (Id.new_ids (), (Basic(Fill.dummy, b)), Pdftransform.i, compop))
        in
          let gpc_op =
            match op with
            | Union | Subtraction | ExclusiveOr -> Clip.Union
            | Intersection -> Clip.Intersection
          in
            let poly_a =
              Clip.gpc_polygon_of_box
                (float a_xmin) (float a_xmax) (float a_ymin) (float a_ymax)
            and poly_b =
              Clip.gpc_polygon_of_box
                (float b_xmin) (float b_xmax) (float b_ymin) (float b_ymax)
            in
              let poly' = Clip.gpcml_clippolygon gpc_op poly_a poly_b in
                bounds_of_basicshape
                  (Obj (Id.new_ids (),
                    (Basic (Fill.dummy, Path (path_of_gpcpolygon Pdfgraphics.EvenOdd poly'))),
                    Pdftransform.i, compop))

(* \section{Positioning objects} *)

(* Possible positions on the bounding box of an object at which we may like to
anchor an object. *)
type anchor =
  | Left | TopLeft | Top | TopRight | Right
  | BottomRight | Bottom | BottomLeft | Centre

(* Calculate where a given anchor is on an object. *)
let findpoint anchor renderobj =
  let minx, maxx, miny, maxy = proper_bounds renderobj in
    let tl = minx, miny and tr = maxx, miny
    and bl = minx, maxy and br = maxx, maxy in
      let l = between tl bl and t = between tl tr
      and r = between tr br and b = between bl br in
        let c = between l r in
          match anchor with
          | Left -> l | TopLeft -> tl | Top -> t | TopRight -> tr | Right -> r
          | BottomRight -> br | Bottom -> b | BottomLeft -> bl | Centre -> c
 
(* Position a renderobject with a given anchor at a given point. *)
let position_anchor anchor p renderobj =
  let dx, dy = mkvector (findpoint anchor renderobj) p in
    transform_renderobject (Pdftransform.Translate (dx, dy)) renderobj

(* \section{Calculating the shape of an object} *)

(* Calculate the shape, minshape or both of a basic shape or filter. The two
optional flags [getshp] and [getminshp] determine what is
returned---[Sprite.NullShape] for any set to [false]. *)
let rec shape_of_basicshape ?(getshp = true) ?(getminshp = true) obj =
  match obj with Obj (idset, geom, f_transform, compop) ->
    match geom with
    | Filter {geometry = g} ->
        shape_of_basicshape (Obj (idset, g, f_transform, compop))
        ~getshp:getshp ~getminshp:getminshp
    | Group objs ->
        begin match Cache.getshape idset with
          | Some (shp, minshp) -> shp, minshp
          | None ->
              (* Compose the group's transform onto all the objects within it *)
              let objs'=
                map
                  (fun (Obj (idset, geom, f_transform', compop)) ->
                     (Obj (Id.new_ids (), geom,
                       Pdftransform.append f_transform f_transform', compop)))
                  objs
              in
                let shps, minshps =
                  split
                    (map
                      (fun o -> shape_of_basicshape o ~getshp:getshp ~getminshp:getminshp)
                      objs')
                in
                  let shp = pair_reduce ( ||| ) shps in
                    let minshp = Sprite.NullShape in
                      (*i flprint "CACHE: Adding shape of group\n"; i*)
                      Cache.addshape idset shp minshp;
                      shp, minshp
        end
    | Basic (_, shapekind) ->
        begin match Cache.getshape idset with
          | Some (shp, minshp) -> shp, minshp
          | None ->
            let shp, minshp =
              match transform_shapekind f_transform shapekind with
              | Path path ->
                  Polygon.shapeminshape_polygon path
              | Brushstroke brushstroke ->
                  Brush.shape_of_brushstroke brushstroke
              | StrokedPath (path, spec) ->
                  Polygon.shapeminshape_of_unsorted_edgelist (Shapes.strokepath spec path) Pdfgraphics.NonZero
              | CPG (op, a_basicshape, b_basicshape) ->
                  (* Here we need both shape and minshape in order to find either *)
                  let a_shp, a_minshp =
                    shape_of_basicshape
                      (Obj (Id.new_ids (), Basic (Fill.dummy, a_basicshape), Pdftransform.i, compop))
                      ~getshp:getshp ~getminshp:getminshp
                  and b_shp, b_minshp =
                    shape_of_basicshape
                      (Obj (Id.new_ids (), Basic (Fill.dummy, b_basicshape), Pdftransform.i, compop))
                      ~getshp:getshp ~getminshp:getminshp
                  in
                    match op with
                    | Union -> a_shp ||| b_shp, a_minshp ||| b_minshp
                    | Intersection -> a_shp &&& b_shp, a_minshp &&& b_minshp
                    | Subtraction -> a_shp --- b_minshp, a_minshp --- b_shp
                    | ExclusiveOr ->
                        (a_shp ||| b_shp) --- (a_minshp &&& b_minshp),
                        b_minshp --- a_shp ||| a_minshp --- b_shp
            in
              (*i flprint "CACHE: Adding shape of basic shape\n"; i*)
              Cache.addshape idset shp minshp;
              shp, minshp
        end
    | Convolved (k, s) ->
        begin
          match Cache.getshape idset with
          | Some (shp, minshp) -> shp, minshp
          | None ->
            let r = Convolve.radius_of_kernel k in
              clear Cache.usecache; (* FIXME: Need to do this properly *)
              let shp =
                (Sprite.bloat r r
                  (shapeonly_of_basicshape (Obj (idset, s, f_transform, compop))))
              and minshp =
                (* The minshape is null if the fill is fancy in convolved objects.
                Otherwise it's the minshape eroded *)
                match findfill s with
                | Fill.Plain ->
                    (Sprite.erode r r
                      (minshapeonly_of_basicshape (Obj (idset, s, f_transform, compop))))
                | Fill.Fancy -> Sprite.NullShape
              in
                set Cache.usecache;
                (*i flprint "CACHE: Adding shape of convolved object\n"; i*)
                Cache.addshape idset shp minshp;
                shp, minshp
        end
    | Primitive (c, p) ->
        let shape = 
          match p with
          | HLine (y, xmin, xmax) ->
              let yi = toint y and xmini = toint xmin and xmaxi = toint xmax in
                assert (xmaxi >= xmini);
                if xmaxi = xmini then Sprite.NullShape else
                  Sprite.Shape
                    (Sprite.Box (xmini, yi, xmaxi, yi), 
                     [(yi, 1, [[(xmini, xmaxi - xmini + 1)]])])
          | VLine (x, ymin, ymax) ->
              let xi = toint x and ymini = toint ymin and ymaxi = toint ymax in
                assert (ymaxi >= ymini);
                if ymaxi = ymini then Sprite.NullShape else
                  let length = ymaxi - ymini + 1 in
                    Sprite.Shape
                      (Sprite.Box (xi, ymini, xi, ymaxi),
                       [(ymini, length, many [(xi, 1)] length)])
          | Rectangle (xmin, ymin, xmax, ymax) ->
              assert (xmax >= xmin && ymax >= ymin);
              let xmini = toint xmin and ymini = toint ymin
              and xmaxi = toint xmax and ymaxi = toint ymax in
                let height = ymaxi - ymini + 1
                and width = xmaxi - xmini + 1 in
                  Sprite.Shape
                    (Sprite.Box (xmini, ymini, xmaxi, ymaxi),
                     [(ymini, height, many [(xmini, width)] height)])
        in
          shape, shape

(* Only get the shape. *)
and shapeonly_of_basicshape r =
  fst (shape_of_basicshape r ~getshp:true ~getminshp:false)

(* Only get the minshape. *)
and minshapeonly_of_basicshape r =
  snd (shape_of_basicshape r ~getshp:false ~getminshp:true)

(* \section{Selection functions} *)

(* Most of the structure is there for multiple discrete selections but for now,
we support only a single selection in each view. *)

(* A selection box is either for sizing or rotating/shearing. *)
type selectbox = Size | Rotate of int * int

(* Possible handle locations *)
type handle =
  | HandleTopLeft
  | HandleTopMiddle
  | HandleTopRight
  | HandleLeftMiddle
  | HandleRightMiddle
  | HandleBottomLeft
  | HandleBottomMiddle
  | HandleBottomRight
  | HandleRotationCentre

(* A handle list for a single selection. *)
type handlelist = (handle * Id.idset * renderobject option ref) list

(* A single selection, containing some objects and a list of handles. *)
type selection = renderobject list * handlelist 

(* A selection, together with box type. *)
type selections = selectbox * selection

(* Find the renderobject which currently represents a handle. *)
let renderobject_of_handle (_, selection) handle =
  let rec renderobject_of_handlelist = function
    | [] -> failwith "renderobject_of_handle: handle not found"
    | (handle', _, {contents = Some obj})::t when handle = handle' -> obj
    | _::t -> renderobject_of_handlelist t
  in
    renderobject_of_handlelist (snd selection)

let handles_size () =
  [(HandleTopLeft, Id.new_ids (), ref None);
   (HandleTopMiddle, Id.new_ids (), ref None);
   (HandleTopRight, Id.new_ids (), ref None);
   (HandleLeftMiddle, Id.new_ids (), ref None);
   (HandleRightMiddle, Id.new_ids (), ref None);
   (HandleBottomLeft, Id.new_ids (), ref None);
   (HandleBottomMiddle, Id.new_ids (), ref None);
   (HandleBottomRight, Id.new_ids (), ref None)]

let handles_rotate () =
  (HandleRotationCentre, Id.new_ids (), ref None)::handles_size ()

(* Make a segment list representing a closed shape defined by a series of
points. *)
let segments_of_points = function
  | [] -> []
  | ps ->
      let rec makesegs = function
        | [] | [_] -> []
        | h::h'::t -> Pdfgraphics.Straight (h, h')::makesegs (h'::t)
      in
        makesegs (ps @ [hd ps])

(* A similar version, but with no closing stroke. *)
let segments_of_points_open =
  couple (fun a b -> Pdfgraphics.Straight (a, b))

(* Selection fill *)
let selection_colour = Colour.red

let selection_fill = Fill.plain selection_colour

(* A sizing handle, 7 units long, pointing along the y axis in a positive
direction, with the origin at the middle of the arrow. *)
let arrow =
  segments_of_points
    [(1., ~-.1.5); (2., ~-.1.5); (0., ~-.3.5); (~-.2., ~-.1.5); (~-.1., ~-.1.5);
     (~-.1., 1.5); (~-.2., 1.5); (0., 3.5); (2., 1.5); (1., 1.5)]

(* The arrow, centre at point [(x, y)], rotated by the [angle]. *)
let arrow_at (x, y) angle =
  let transform =
    [Pdftransform.Translate (x, y);
     Pdftransform.Rotate ((0., 0.), rad_of_deg angle);
     Pdftransform.Scale ((0., 0.), 3., 3.)]
  in
    let geometry =
      Basic (selection_fill, Path (Pdfgraphics.EvenOdd, [(Pdfgraphics.Not_hole, Pdfgraphics.Closed, arrow)]))
    in
      Obj (Id.new_ids (), geometry, transform, Over)

(* A rotation handle, origin at middle of curve, ``right'' arrow pointing to
positive [x], ``down'' arrow pointing to positive [y].*)
let rotate =
  let k = Shapes.kappa in
    let downarrow =
      segments_of_points_open
        [(0.5, ~-.2.); (1.5, ~-.2.); (0., ~-.4.); (~-.1.5, ~-.2.); (~-.0.5, ~-.2.)]
    and outercurve =
      [Pdfgraphics.Bezier ((~-.0.5, ~-.2.), (~-.0.5, 2.5 *. k -. 2.), (2. -. 2.5 *. k, 0.5), (2., 0.5))]
    and rightarrow =
      segments_of_points_open
        [(2., 0.5); (2., 1.5); (4., 0.); (2., ~-.1.5); (2., ~-.0.5)]
    and innercurve =
      [Pdfgraphics.Bezier ((2., ~-.0.5), (2. -. 1.5 *. k, ~-.0.5), (0.5, 1.5 *. k -. 2.), (0.5, ~-.2.))]
    in
      downarrow @ outercurve @ rightarrow @ innercurve

(* A rotate handle at a given angle offset. *)
let rotate_at (x, y) angle =
  let transform =
    [Pdftransform.Translate (x, y);
     Pdftransform.Rotate ((0., 0.), rad_of_deg angle);
     Pdftransform.Scale ((0., 0.), 4., 4.)]
  in
    let geometry =
      Basic (selection_fill, Path (Pdfgraphics.EvenOdd, [(Pdfgraphics.Not_hole, Pdfgraphics.Closed, rotate)]))
    in
      Obj (Id.new_ids (), geometry, transform, Over)

(* Rotation centre. *)
let rotation_centre =
  segments_of_points
    [(1., 2.); (2., 2.); (2., 1.); (3., 1.); (3., 2.); (4., 2.);
     (4., 3.); (3., 3.); (3., 4.); (2., 4.); (2., 3.); (1., 3.)]

(* Make an object from a fill and geometry, using standard values for the other
parts. *)
let mkobj fill geom =
  Obj (Id.new_ids (), Basic (fill, geom), Pdftransform.i, Over)

(* The same. centered at [(x, y)]. *)
let rotation_centre_at (x, y) =
  let transform = Pdftransform.Scale ((0., 0.), 4., 4.) in
    let scaled =
      transform_renderobject(*i _freeze i*)
      transform
      (mkobj selection_fill (Path (Pdfgraphics.EvenOdd, [(Pdfgraphics.Not_hole, Pdfgraphics.Closed, rotation_centre)])))
    in
      position_anchor Centre (x, y) scaled 

(* Make a list of renderobjects representing an axis-aligned rectangle in a
given colour. *)
let drawable_primitive_rectangle col xmin xmax ymin ymax =
  assert (xmax >= xmin && ymax >= ymin);
  let top = HLine (ymin, xmin, xmax)
  and bottom = HLine (ymax, xmin, xmax)
  and left = VLine (xmin, ymin, ymax)
  and right = VLine (xmax, ymin, ymax)
  and mkprim p = Primitive (col, p) in
    map mkprim [top; bottom; left; right]

(* Drawable objects for a rubber band. *)
let drawable_of_rubberband x0 y0 x1 y1 =
  let xmin = float (min x0 x1) and xmax = float (max x0 x1)
  and ymin = float (min y0 y1) and ymax = float (max y0 y1) in
    map
      (fun g -> Obj (Id.new_ids (), g, Pdftransform.i, Over))
      (drawable_primitive_rectangle Colour.black xmin xmax ymin ymax)

(* Make a list of renderobjects which represent the selections in question.
This involves building the selection handle, and planting the required idsets
in the handle objects. *)
let drawable_of_selection (box, (renderobjects, handles)) =
  match renderobjects with [] -> [] | _ ->
    (* 1. Calculate box. *)
    let boundslist = map bounds_of_basicshape renderobjects in
      let xmin, xmax, ymin, ymax = pair_reduce box_union boundslist in
        let xmin = float xmin and xmax = float xmax and ymin = float ymin and ymax = float ymax in
          let geometries = drawable_primitive_rectangle selection_colour xmin xmax ymin ymax in
            let rectangle =
              map
                (fun g -> Obj (Id.new_ids (), g, Pdftransform.i, Over))
                geometries
            in
              let rec setobj handle obj = function
                | [] -> failwith "Failed to set object in selection code....\n"
                | (handle', idset, reference)::_ when handle = handle' -> reference := Some obj
                | _::t -> setobj handle obj t
              in
                let setobj handle obj = setobj handle obj handles in
                  (* 2. Plant idsets. *)
                  match box with
                  | Size ->
                      let p x = x -. 8. and m x = x +. 8.
                      and pp x = x -. 12. and mm x = x +. 12. in
                        let topleft = arrow_at (p xmin, p ymin) 135.
                        and topmiddle = arrow_at ((xmax +. xmin) /. 2., pp ymin) 180.
                        and topright = arrow_at (m xmax, p ymin) 225.
                        and rightmiddle = arrow_at (mm xmax, (ymax +. ymin) /. 2.) 270.
                        and bottomright = arrow_at (m xmax, m ymax) 315.
                        and bottommiddle = arrow_at ((xmax +. xmin) /. 2., mm ymax) 0.
                        and bottomleft = arrow_at (p xmin, m ymax) 45.
                        and leftmiddle = arrow_at (pp xmin, (ymax +. ymin) /. 2.) 90. in
                          List.iter2 setobj
                            [HandleTopLeft; HandleTopMiddle; HandleTopRight; HandleRightMiddle;
                             HandleBottomRight; HandleBottomMiddle; HandleBottomLeft; HandleLeftMiddle]
                            [topleft; topmiddle; topright; rightmiddle;
                             bottomright; bottommiddle; bottomleft; leftmiddle];
                            ([topleft; topmiddle; topright; rightmiddle; bottomright;
                              bottommiddle; bottomleft; leftmiddle] @ rectangle)
                  | Rotate (x, y) ->
                      let pp x = x -. 12. and mm x = x +. 12. in
                        let topleft = rotate_at (pp xmin, pp ymin) 90.
                        and topmiddle = arrow_at ((xmax +. xmin) /. 2., pp ymin) 90.
                        and topright = rotate_at (mm xmax, pp ymin) 180.
                        and rightmiddle = arrow_at (mm xmax, (ymax +. ymin) /. 2.) 180.
                        and bottomright = rotate_at (mm xmax, mm ymax) 270.
                        and bottommiddle = arrow_at ((xmax +. xmin) /. 2., mm ymax) 270.
                        and bottomleft = rotate_at (pp xmin, mm ymax) 0.
                        and leftmiddle = arrow_at (pp xmin, (ymax +. ymin) /. 2.) 0.
                        and rotationcentre = rotation_centre_at (float x, float y) in
                          List.iter2 setobj
                            [HandleTopLeft; HandleTopMiddle; HandleTopRight; HandleRightMiddle;
                             HandleBottomRight; HandleBottomMiddle; HandleBottomLeft; HandleLeftMiddle;
                             HandleRotationCentre]
                            [topleft; topmiddle; topright; rightmiddle; bottomright;
                             bottommiddle; bottomleft; leftmiddle; rotationcentre];
                            ([topleft; topmiddle; topright; rightmiddle; bottomright;
                              bottommiddle; bottomleft; leftmiddle; rotationcentre] @ rectangle)

(* Is an object in a selection? *)
let rec is_selected (box, selection) obj =
  let rec is_selected_selection obj = function
    | [] -> false
    | obj'::t -> obj_eq obj obj' || is_selected_selection obj t
  in
    is_selected_selection obj (fst selection)

(* Make an outline suitable for outline dragging from a given scene. At the
moment this is just a box. *)
let outline_of_scene scene =
  let boundslist = map bounds_of_basicshape scene in
    let xmin, xmax, ymin, ymax = pair_reduce box_union boundslist in
      let xmin = float xmin and xmax = float xmax and ymin = float ymin and ymax = float ymax in
        map
         (fun g -> Obj (Id.new_ids (), g, Pdftransform.i, Over))
         (drawable_primitive_rectangle Colour.black xmin xmax ymin ymax)

(* \section{Rendering scenes} *)

(* A [view] is a window displaying a pdf. There may be multiple views per pdf.*)
type view =
  {mutable scene : scene;
   mutable pages : scene;
   mutable window : Wxgui.window;
   mutable background : scene;
   mutable selections : selections;
   mutable master_update : Sprite.shape;
   mutable rubberband : (int * int * int * int) option; (* [x0, y0, x1, y1] *)
   mutable tool : Wxgui.tool}

(* This consists of a number of mutually recursive functions mirroring the
definition of a scene. \smallgap *)

(* Invert the fill level in a sprite --- for constructive geometry. Assumes the
sprite only has alpha values. *)
let invert_fill =
  Sprite.sprite_map
    (fun x -> Colour.colour_of_channel (255 - Colour.alpha_of_colour x))

(* An approximation to a Continous Exclusive OR on $[0\ldots 255]$, $[0\ldots
255]$ *)
let eor a b =
  let inv a = 255 - a in
    match  a < 128, b < 128 with
    | true, true -> max a b
    | false, true -> inv (max (inv a) b)
    | true, false -> inv (max a (inv b))
    | false, false -> max (inv a) (inv b)

(* Constructive Plane Geometry. *)
let rec sprite_of_cpg shp fill f_transform op a b =
  let fill' = fill.Fill.filltransform f_transform fill
  and dummy_a =
    Obj (Id.new_ids (), Basic (Fill.plain Colour.black, a), Pdftransform.i, Over)
  and dummy_b =
    Obj (Id.new_ids (), Basic (Fill.plain Colour.black, b), Pdftransform.i, Over)
  in
   (* 1. Calculate the areas for rendering a and b *)
   let shp_a, minshp_a = shape_of_basicshape dummy_a
   and shp_b, minshp_b = shape_of_basicshape dummy_b in
     let shp_a, minshp_a = shp_a &&& shp, minshp_a &&& shp
     and shp_b, minshp_b = shp_b &&& shp, minshp_b &&& shp in
       let maxshp_a = shp_a --- minshp_a
       and maxshp_b = shp_b --- minshp_b
       and torender_a = shp &&& shp_a
       (* Don't calculate both minshapes in the region where they overlap *)
       and torender_b = (shp &&& shp_b) --- (minshp_a &&& minshp_b) in
         (* 2. Render a and b in the given areas *)
         let spr_a = sprite_of_basicshape dummy_a torender_a []
         and spr_b = sprite_of_basicshape dummy_b torender_b [] in
           let shprendered_a = Sprite.shape_of_sprite spr_a
           and shprendered_b = Sprite.shape_of_sprite spr_b in
             let totalrendered = shprendered_a ||| shprendered_b in
               (* 3. Calculate the four resultant areas, modulo intersection
               with the union shape of the rasterized a and b *)
               let min_a_min_b = minshp_a &&& minshp_b &&& totalrendered
               and min_a_max_b = minshp_a &&& maxshp_b &&& totalrendered
               and max_a_min_b = maxshp_a &&& minshp_b &&& totalrendered
               and max_a_max_b = maxshp_a &&& maxshp_b &&& totalrendered in
                (* 4. Perform operations, depending upon op *)
                let minmin, minmax, maxmin, maxmax =
                  match op with
                  | Union ->
                      Sprite.portion spr_a min_a_min_b,
                      Sprite.portion spr_b min_a_max_b,
                      Sprite.portion spr_a max_a_min_b,
                      (fst (Sprite.caf
                             (fun a b -> let alpha_a = Colour.alpha_of_colour a
                                         and alpha_b = Colour.alpha_of_colour b in
                                           let t = alpha_a + alpha_b in
                                             if t > 255 then Colour.colour_of_rgba 0 0 0 255 else Colour.colour_of_rgba 0 0 0 t)
                             Colour.opaque
                             (Sprite.portion spr_a max_a_max_b)
                             (Sprite.portion spr_b max_a_max_b)))
                  | Subtraction ->
                      Sprite.NullSprite,
                      invert_fill (Sprite.portion spr_b min_a_max_b),
                      Sprite.NullSprite,
                      (fst (Sprite.caf
                             (fun a b ->
                                let a = Colour.alpha_of_colour a
                                and b = Colour.alpha_of_colour b in
                                  Colour.colour_of_channel (max 0 (a - b)))
                             Colour.opaque
                             (Sprite.portion spr_a max_a_max_b)
                             (Sprite.portion spr_b max_a_max_b)))
                  | Intersection ->
                      Sprite.portion spr_a min_a_min_b,
                      Sprite.portion spr_b min_a_max_b,
                      Sprite.portion spr_a max_a_min_b,
                      (fst (Sprite.caf
                              (fun a b ->
                                Colour.colour_of_channel
                                (min
                                  (Colour.alpha_of_colour a)
                                  (Colour.alpha_of_colour b)))
                              Colour.opaque
                              (Sprite.portion spr_a max_a_max_b)
                              (Sprite.portion spr_b max_a_max_b)))
                  | ExclusiveOr ->
                      Sprite.NullSprite,
                      invert_fill (Sprite.portion spr_b min_a_max_b),
                      invert_fill (Sprite.portion spr_a max_a_min_b),
                      (fst (Sprite.caf
                              (fun a b ->
                                 let a = Colour.alpha_of_colour a
                                 and b = Colour.alpha_of_colour b in
                                   (* Book 8, Pg 12 *)
                                   let x = eor a b in
                                   Colour.colour_of_rgba 0 0 0 x)
                              Colour.opaque
                              (Sprite.portion spr_a max_a_max_b)
                              (Sprite.portion spr_b max_a_max_b)))
                in
                  let shape_covered =
                    pair_reduce
                      ( ||| ) 
                      [min_a_min_b; min_a_max_b; max_a_min_b; max_a_max_b]
                  in
                    let min_a =
                      Sprite.portion
                        spr_a ((minshp_a --- shape_covered) &&& shprendered_a)
                    and min_b =
                      Sprite.portion
                        spr_b ((minshp_b --- shape_covered) &&& shprendered_b)
                    and max_a =
                      Sprite.portion
                        spr_a ((maxshp_a --- shape_covered) &&& shprendered_a)
                    and max_b =
                      Sprite.portion
                        spr_b ((maxshp_b --- shape_covered) &&& shprendered_b)
                    in
                      (* 5. Caf the parts together *)
                      let alpha =
                        pair_reduce
                          (fun a b ->
                            fst (Sprite.caf (fun _ _ -> failwith "CPG caf") Colour.opaque a b))
                          [minmin; minmax; maxmin; maxmax; min_a; min_b; max_a; max_b]
                      in
                        (* 6. Apply the fill *)
                        Sprite.map_coords
                          (fun y x alpha ->
                             let c' = fill'.Fill.fillsingle x y in
                               Colour.dissolve c' ~delta:(Colour.alpha_of_colour alpha))
                          alpha

(* Calculate the sprite of a basic shape or filter. *)
and sprite_of_basicshape ?(force_fancy=false) (Obj (idset, geom, f_transform, compop)) shp whole_scene =
  match geom with
  | Filter {geometry = g} ->
      sprite_of_basicshape ~force_fancy:true (Obj (idset, g, f_transform, compop)) shp whole_scene
  | Group objs ->
      (* Transform the objects within the group. *)
      let objs'=
        map
          (fun (Obj (idset, geom, f_transform', compop)) ->
             (Obj (Id.new_ids (), geom,
                Pdftransform.append f_transform f_transform', compop)))
          objs
      in
        (match render_scene
          (Id.new_ids ()) false (shp, Sprite.NullSprite) objs'
          whole_scene Sprite.NullShape
         with
          (a, _, _) -> a)
  | Basic (fill, basicgeom) ->
      (match transform_shapekind f_transform basicgeom with
       | Path path ->
           (*i flprint "PATH+===============================\n";
           flprint (Pdfgraphics.string_of_path path);
           flprint "END+================================\n"; i*)
           let sprite =
             Polygon.polygon_sprite
               (fill.Fill.filltransform f_transform fill) shp path
           in
             if Sprite.spritecheck sprite
               then sprite
               else failwith "Scene.spriteof_basicshape: malformed"
       | StrokedPath (path, spec) ->
           let stroked = Shapes.strokepath spec path in
             Polygon.polygon_sprite_edgelist
               (fill.Fill.filltransform f_transform fill) shp stroked Pdfgraphics.EvenOdd
       | Brushstroke brushstroke ->
           Brush.sprite_of_brushstroke
             brushstroke (fill.Fill.filltransform f_transform fill) shp
       | CPG (op, a, b) ->
           sprite_of_cpg shp fill f_transform op a b)
  | Convolved (k, Group s) ->
      begin
        (* Considered like the fancy case below for now. *)
        let r = Convolve.radius_of_kernel k in
          let shp' = Sprite.bloat r r shp in
            let rasterized_toconvolve =
              sprite_of_basicshape (fakeobj_t (Group s) f_transform) shp' whole_scene
            in
              let convolve_sprite =
                Convolve.convolve_sprite k rasterized_toconvolve
              in
                Sprite.portion convolve_sprite shp
      end
  | Convolved (k, s) ->
      begin
        let s' = transform_basicshape f_transform s in
          let plain_or_fancy =
            if force_fancy then Fill.Fancy else findfill s'
          in
          match plain_or_fancy with
          | _ ->
              (* 1. Fancy fills -- Convolve the whole thing *)
			  (* FIXME: Does this fail like convolved group above did? *)
              let r = Convolve.radius_of_kernel k in
                let shp' = Sprite.bloat r r shp in
                  let rasterized_toconvolve =
                    sprite_of_basicshape (fakeobj s') shp' whole_scene
                  in
                    Sprite.portion (Convolve.convolve_sprite k rasterized_toconvolve) shp
          (*i FIXME: This calls Convolve.convolve_in_shape wrongly i*)
          (*i | Fill.Plain ->
              (* 2. Plain fills -- Convolve maxshape but not minshape *)
              let r = Convolve.radius_of_kernel k in
                let shp' = Sprite.bloat r r shp in
                  let minshape = minshapeonly_of_basicshape (fakeobj s') in
                    let minshape' = Sprite.erode (r * 2) (r * 2) minshape in
                      let don't_convolve = minshape' &&& shp' in
                        let to_convolve = shp' --- don't_convolve in
                          (* We needn't do all of don'tconvolve *)
                          let don't_convolve' = don't_convolve &&& shp in
                            let rasterized_to_convolve =
                              sprite_of_basicshape (fakeobj s') to_convolve whole_scene
                            and rasterized_don't_convolve =
                              sprite_of_basicshape (fakeobj s') don't_convolve' whole_scene
                            in
                              let pickup_convolve = to_convolve &&& shp in
                                let convolved =
                                  Convolve.convolve_sprite_in_shape
                                    k rasterized_to_convolve to_convolve pickup_convolve
                                in
                                  fst
                                    (Sprite.caf Colour.nocover Colour.opaque
                                    convolved rasterized_don't_convolve) i*)
      end
  | Primitive (c, p) -> failwith "Internal inconsistency: Should already have been rendered"

and spriteof_filter
  lmo lower' whole_scene obj idset objects_below shptorender
  {geometry = geometry; reading_scene = frs; filter = fl}
=
  let setanyfilter = ref (!underanyfilter = false) in
    if !setanyfilter then underanyfilter := true;
  let scene'torender, shptorender', scene' =
    frs shptorender idset obj objects_below
  in
    let renderlist = scene' in
      let scene'rendered, lower', _ =
        render_scene
          lmo lower' (scene'torender, Sprite.NullSprite) renderlist whole_scene Sprite.NullShape
      in
        let scene'renderedfiltered = fl scene'rendered obj shptorender' in
          (* Now we work out which pixels are finished in the filter geometry*)
          (* First, render the geometry in the update region *)
          let alpha_in_update = sprite_of_basicshape obj shptorender' whole_scene in
           let pixels_finished =
             snd
               (Sprite.caf Colour.nocover Colour.opaque Sprite.NullSprite alpha_in_update)
           in
             (* Render the scene below in the pixels not finished *)
             let pixels_for_normal_scene = shptorender' --- pixels_finished in
               let scenerendered, lower', _ =
                 render_scene
                   lmo lower' (pixels_for_normal_scene, Sprite.NullSprite)
                   objects_below whole_scene Sprite.NullShape
               in
                let r =
                  blend' scenerendered scene'renderedfiltered alpha_in_update
                and e =
                  (* The extrafinish must be the whole of the filter's shape *)
                  shapeonly_of_basicshape obj
                in
                  (* Unset the flag only if we had to set it. *)
                  if !setanyfilter then underanyfilter := false;
                  if !pdf_filter_debug_active then
                    begin
                      Sprite.add_debug_shape shptorender;
                      Sprite.add_debug_shape ~dx:300 scene'torender;
                      Sprite.add_debug_sprite ~dx:600 scene'rendered;
                      Sprite.add_debug_sprite ~dx:900 scene'renderedfiltered;
                      Sprite.add_debug_sprite ~dx:1200 alpha_in_update;
                      Sprite.add_debug_shape ~dy:300 pixels_finished;
                      Sprite.add_debug_shape ~dx:300 ~dy:300 pixels_for_normal_scene;
                      Sprite.add_debug_sprite ~dx:600 ~dy:300 scenerendered;
                      Sprite.add_debug_sprite ~dx:900 ~dy:300 r;
                      Sprite.add_debug_shape ~dx:1200 ~dy:300 e;
                      Sprite.write_debug_page "Filter"
                    end;
                  r, e

(* This is the main function for hidden surface removal with cache. *)
and spriteof lmo lower obj shp objects_below whole_scene =
  match obj with
  Obj ((oid, _) as idset, geom, f_transform, compop) as obj ->
    (* The objects in a scene below idset in its group or, if no group, at top level. *)
    (*let rec groupfilter idset scene =
      match scene with
      | [] -> []
      | h::t ->
        match h with
        | Obj (_, Group objs, _, _) -> groupfilter idset objs
        | Obj (idset', _, _, _) -> if idset' = idset then t else groupfilter idset t
    (* The objects in the current group (if any) below the idset and all else below that group. *)
    (* This may need to be made more efficient---many calls to isin *)
    and fullfilter idset scene =
      let rec isin idset scene =
        match scene with
        | [] -> false
        | Obj (idset', geom, _, _)::t ->
            idset = idset' ||
              match geom with
              | Group objs -> isin idset objs || isin idset t
              | _ -> isin idset t
      in
        match scene with
        | [] -> []
        | Obj (idset', geom, tr, compop)::t ->
            if idset = idset' then (printf "Found idset\n"; t) else
              begin
                match geom with
                | Group objs when isin idset objs->
                    Obj (idset', Group (fullfilter idset objs), tr, compop)::t
                | _ -> fullfilter idset t
              end
    in*)
      let lower' = lower in
        let cachedwholesprite, pshape =
          match geom, idset with
          | Filter _, _ when not lower ->
              Sprite.NullSprite, Sprite.NullShape
          | Filter _, _ ->
              (match Cache.getsprite idset with
               | None -> Sprite.NullSprite, Sprite.NullShape
               | Some s -> s)
          | _ -> 
              (match Cache.getsprite idset with
               | None -> Sprite.NullSprite, Sprite.NullShape
               | Some s -> s)
        in
          let shptorender = shp --- pshape in
            match shptorender with
            | Sprite.NullShape ->
                (* We still need to return the extra finish if it's a filter. *)
                let portion = Sprite.portion cachedwholesprite shp in
                  let ef =
                    match geom with
                      | Filter _ -> Sprite.shape_of_sprite portion
                      | _ -> Sprite.NullShape
                  in 
                    portion, ef, lower'
            | _ ->
              let rendered, ef =
                match geom with
                | Convolved (_, (Convolved _ | Primitive _ | Filter _)) ->
                    failwith "Renderscene.spriteof: Malformed scene"
                | Basic _ | Convolved (_, (Basic _ | Group _)) | Group _ ->
                    let fill = fillin_obj obj in
                      let shp, minshp = shape_of_basicshape obj in
                        let maxshape = Sprite.shape_difference shp minshp in
                          let maxshapebitshape = shptorender &&& maxshape in
                            let maxshapebit =
                              sprite_of_basicshape obj maxshapebitshape whole_scene
                            in
                              let minshapebitshape = minshp &&& shptorender in
                                let transformed_fill =
                                  fill.Fill.filltransform f_transform fill
                                in 
                                  let minshapebit =
                                    Sprite.fillshape minshapebitshape transformed_fill
                                  in
                                    fst (Sprite.caf Colour.nocover Colour.opaque minshapebit maxshapebit),
                                    Sprite.NullShape
                | Primitive (c, p) ->
                    (* Notice we render the whole of it here, since it'll
                    always be a plain fill, so won't use any (or much) more
                    memory. We don't want fragmentation. *)
                    let shape = shapeonly_of_basicshape obj in
                      let torender = shptorender &&& shape in
                        Sprite.fillshape torender (Fill.plain c), Sprite.NullShape 
                | Filter f ->
                    spriteof_filter lmo lower' whole_scene obj idset objects_below shptorender f
                in
                  (* Add back to the cache. *)
                  let should_cache_sprite = function
                    | Primitive _ -> false
                    | _ -> true
                  in
                    let newwholesprite =
                      fst (Sprite.caf Colour.nocover Colour.opaque cachedwholesprite rendered)
                    in
                      let pshape' = Sprite.shape_of_sprite newwholesprite in
                        if should_cache_sprite geom then
                          begin 
                            (*i flprint "CACHE: Adding sprite\n"; i*)
                            Cache.addsprite idset newwholesprite pshape';
                          end;
                        let part =
                          Sprite.portion newwholesprite (Sprite.shape_intersection shp pshape')
                        in
                          part, ef, lower'

(* When we have calculated the changed scene and original scene for a filter,
we need to blend the two together. [alpha] is in the whole area of the filter
requring updating---at least the union of [sprsc] and [sprsc'] where [sprsc]
and [sprssc'] are the rendered scene and scene'. *)
and blend' sprsc sprsc' alpha =
  let alpha_in_sprsc = Sprite.portion alpha (Sprite.shape_of_sprite sprsc) in
    let alpha_in_sprsc' = Sprite.portion alpha (Sprite.shape_of_sprite sprsc') in
      let sprsc_att =
        fst
          (Sprite.caf
            (fun col alphacol ->
               Colour.dissolve col ~delta:(255 - Colour.alpha_of_colour alphacol))
            Colour.opaque sprsc alpha_in_sprsc)
      in
        let sprsc'_att =
          fst
            (Sprite.caf
              (fun col alphacol ->
                 Colour.dissolve col ~delta:(Colour.alpha_of_colour alphacol))
              Colour.opaque sprsc' alpha_in_sprsc')
        in
          fst (Sprite.caf Colour.pd_plus Colour.opaque sprsc_att sprsc'_att)

(* Render an object. *)
and renderobj lmo lower obj objects_below whole_scene (u, a) master_update =
  match obj with Obj (id, _ as idset, geom, _, op) ->
    let xmin, xmax, ymin, ymax = bounds_of_basicshape obj
    and xmin_u, xmax_u, ymin_u, ymax_u =
      match u with
      | Sprite.NullShape -> failwith "renderobj called for null dirty region"
      | Sprite.Shape (Sprite.NoBounds, _) -> failwith "renderobj: malformed shape"
      | Sprite.Shape (Sprite.Box (x0, y0, x1, y1), _) -> x0, x1, y0, y1
    in
      match box_overlap xmin ymin xmax ymax xmin_u ymin_u xmax_u ymax_u with
      | None ->
          u, a, (lower || lmo = idset) (*r Trivial reject on bounds *)
      | _ ->
        let r = shapeonly_of_basicshape obj in
          let r' = r &&& u in
            match r' with
            | Sprite.NullShape ->
                u, a, lower
            | _ ->
              let s, ef, lower' = spriteof lmo lower obj r' objects_below whole_scene in
                let a', f =
                  match geom with
                  | Filter _ ->
                      fst (Sprite.caf Colour.over Colour.opaque a s), ef
                  | _ ->
                    match op with
                    | Over -> Sprite.caf Colour.over Colour.opaque a s
                    | PreTrans (v, Over) ->
                        let f = Colour.dissolve ~delta:(toint (v *. 255.)) in
                          let s' = Sprite.sprite_map f s in
                            Sprite.caf Colour.over Colour.opaque a s'
                    | _ -> failwith "Unknown compop"
                in
if !pdf_debug_active && not !underanyfilter then
  begin
    Sprite.add_debug_sprite (Sprite.translate_sprite 1280 0 (Sprite.fillshape r (Fill.plain Colour.black)));
    Sprite.add_debug_sprite (Sprite.translate_sprite 1920 0 (Sprite.fillshape r' (Fill.plain Colour.black)));
    Sprite.add_debug_sprite (Sprite.translate_sprite 0 640 s); 
    Sprite.add_debug_sprite (Sprite.translate_sprite 640 640 (Sprite.fillshape f (Fill.plain Colour.black)));
  end;
                  u --- f --- ef, a', lower'
                              
and render_scene lmo lower (u, a) objlist whole_scene master_update =
  match objlist with
  | [] ->
      if !pdf_debug_active && not !underanyfilter then
        begin
          Sprite.add_debug_sprite a;
          Sprite.write_debug_page "Final result of render_scene\n"
        end;
      a, lower, u
  | obj::objs ->
      (*i flprint ("Processing object " ^ string_of_renderelt obj ^ "\n"); i*)
      match u with
      | Sprite.NullShape -> a, lower, u
      | _ ->
        let u', a', lower' =
          renderobj lmo lower obj objs whole_scene (u, a) master_update
        in
          if !pdf_debug_active && not !underanyfilter then
            begin
              Sprite.add_debug_sprite (Sprite.translate_sprite 0 0 a); 
              Sprite.add_debug_sprite (Sprite.translate_sprite 640 0 (Sprite.fillshape u (Fill.plain Colour.black)));
              Sprite.add_debug_sprite (Sprite.translate_sprite 1920 640 a');
              Sprite.add_debug_sprite (Sprite.translate_sprite 1280 640 (Sprite.fillshape u' (Fill.plain Colour.black)));
              Sprite.write_debug_page ("During object" ^ string_of_obj obj);
            end;
          render_scene lmo lower' (u', a') objs whole_scene master_update

(* \section{Rendering a frame} *)

let null_selection = Size, ([], [])

(* Render a frame with LMO [lmo], view [view] and dirty region [update]. We
build a render list and pass to [render_scene]. Display of selections can be
suppressed. [topobjects] is an optional scene to be placed on top of the real
one. *)
let render_frame ?(display_selection = true) ?(topobjects = []) lmo view update =
  let selections =
    if display_selection then view.selections else null_selection
  and scene =
    topobjects @ view.scene
  in
   let selection_scene = drawable_of_selection selections
   and rubberband_scene =
     match view.rubberband with
     | None -> []
     | Some (x0, y0, x1, y1) -> drawable_of_rubberband x0 y0 x1 y1
   in
      let render s =
        match
          render_scene lmo false (update, Sprite.NullSprite) s s view.master_update
        with
          a, _, _ -> a
      in
        let allbutbackground = render (rubberband_scene @ selection_scene @ scene) in
          let background = render (view.pages @ view.background) in
            fst (Sprite.caf Colour.over Colour.opaque allbutbackground background)

(* Just render a scene in the given shape. *)
let render_simple_scene scene shape =
  match render_scene (Id.new_ids ()) false (shape, Sprite.NullSprite) scene scene shape with
    a, _, _ -> a

(* \section{Finding dirty regions} *)

(* The dirty region when an object is transformed which has a plain fill. $o$ =
old, [n] = new, [u] = master update (typically the window rectangle). *)
let plaindirty o n u =
  let shp_o, minshp_o =
    shape_of_basicshape o ~getshp:true ~getminshp:true
  and shp_n, minshp_n =
    shape_of_basicshape n ~getshp:true ~getminshp:true
  in
    shp_o --- minshp_n ||| shp_n --- minshp_o &&& u

(* The dirty region when an object with a fancy fill is transformed. *)
let alldirty o n u =
  let shp_o = shapeonly_of_basicshape o
  and shp_n = shapeonly_of_basicshape n in
    shp_o ||| shp_n &&& u

let compop_in (Obj (_, _, _, c)) = c 

(* Calculate the dirty region for an object and an action performed upon it. We
check (a)~the fillkind of the geometry (b)~if a filter, the filterkind. *)
(* FIXME: This needs to be more comprehensive, if [view_map_change_scene] is to be
generic. *)
let dirty_region obj obj' =
  match fillkind_in obj, filterkind_in obj, compop_in obj, compop_in obj' with
  | Fill.Plain, Some FilterPlain, a, b when a = b -> plaindirty obj obj'
  | Fill.Plain, None, a, b when a = b -> plaindirty obj obj'
  | _ -> alldirty obj obj'

(* Calculate the dirty region when a selection box [s] is altered to [s'] with
master update [u]. We do the equivalent of [alldirty] here, but this could be
refined to the equivalent of [plaindirty].*)

(* The dirty region when moving from selection [s] to [s']. *)
let dirty_selections s s' u =
  let s_objs = drawable_of_selection s
  and s'_objs = drawable_of_selection s' in
    let getshapes =
      map (fun x -> shapeonly_of_basicshape x)
    in
      let s_shapes = getshapes s_objs and s'_shapes = getshapes s'_objs in
        let foldshapes = fold_left ( ||| ) Sprite.NullShape in
          let s_total = foldshapes s_shapes and s'_total = foldshapes s'_shapes in
            s_total ||| s'_total &&& u

(* Compose dirty functions of all filters above the LMO. To deal with groups,
we extract filters from them too. *)
let dirty_filter lmo initial_dirty scene =
  let rec extract_filters scene =
    match scene with
    | [] -> []
    | Obj (_, geom, _, _) as obj::t ->
        match geom with
        | Filter _ -> obj::extract_filters t
        | Group objects -> extract_filters objects @ extract_filters t
        | _ -> extract_filters t
  in
    let above_lmo = takewhile (fun (Obj (i, _, _, _)) -> i <> lmo) scene in
      let filters = extract_filters above_lmo in
        fold_left
          (function shape -> function
            | Obj (_, Filter f, _, _) as filterobj ->
                f.dirty shape filterobj
            | _ -> assert false)
          initial_dirty
          (rev filters)

(* Find the topmost object whose shape intersects with [(x, y)] \FIXME{This
should use bounds checking, too, rather than just shapes}. *)
type picked =
  | PickedObject of renderobject
  | PickedSelectionHandle of renderobject * renderobject list * handle
  | PickedNone

(* Pick the topmost object matching [x, y]. *)
let rec pickobj x y = function
  | [] -> PickedNone
  | h::t ->
     if Sprite.point_in_shape (shapeonly_of_basicshape h) (x, y)
       then PickedObject h
       else pickobj x y t

(* Pick an object from within a handle. *)
let rec pickhandle x y = function
  | (handle, _, {contents = Some obj})::t ->
      if Sprite.point_in_shape (shapeonly_of_basicshape obj) (x, y)
        then Some (handle, obj)
        else pickhandle x y t
  | _ -> None

(* Pick the topmost selection handle. *)
let picksel x y (b, (objs, handles)) =
  match pickhandle x y handles with
  | None -> PickedNone
  | Some (handle, obj) -> PickedSelectionHandle (obj, objs, handle)

(* Check selections first, then objects. *)
let rec pick x y view =
  let pickedsel = picksel x y view.selections in
    if pickedsel = PickedNone
      then pickobj x y view.scene
      else pickedsel

let rgb_of_cmyk c m y k =
  1. -. fmin 1. (c *. (1. -. k) +. k),
  1. -. fmin 1. (m *. (1. -. k) +. k),
  1. -. fmin 1. (y *. (1. -. k) +. k)

let rec fill_of_pdf_colour pdf resources vals transparency = function
  | Pdfspace.DeviceRGB  ->
      let transparency = toint (transparency *. 255.) in
        begin match vals with
        | Pdfgraphics.Floats [r; g; b] ->
            Fill.plain
              (Colour.dissolve ~delta:transparency (Colour.colour_of_rgba_float r g b 1.))
        | _ -> flprint "COL devicergb/floats mismatch\n"; Fill.plain Colour.red
        end
  | Pdfspace.DeviceCMYK ->
      let transparency = toint (transparency *. 255.) in
       begin match vals with
       | Pdfgraphics.Floats [c; m; y; k] ->
           let r, g, b = rgb_of_cmyk c m y k in
             Fill.plain
              (Colour.dissolve ~delta:transparency (Colour.colour_of_rgba_float r g b 1.))
       | _ ->  flprint "COL devicecmyk/floats mismatch\n"; Fill.plain Colour.red
       end
  | Pdfspace.DeviceGray ->
      let transparency = toint (transparency *. 255.) in
       begin match vals with
       | Pdfgraphics.Floats [g] ->
           let r, g, b = g, g, g in
             Fill.plain
              (Colour.dissolve ~delta:transparency (Colour.colour_of_rgba_float r g b 1.))
       | _ ->  flprint "COL devicegray/floats mismatch\n"; Fill.plain Colour.red
       end
  | Pdfspace.ICCBased {Pdfspace.icc_alternate = cs} ->
      fill_of_pdf_colour pdf resources vals transparency cs
  | c -> Printf.printf "COL ERROR: Not handling colourspace \n%s\n" (Pdfspace.string_of_colourspace c); Fill.plain Colour.red

let cap_of_pdfcap = function
  | 0 -> Shapes.ButtCap
  | 1 -> Shapes.RoundCap
  | 2 -> Shapes.ProjectingCap
  | _ -> failwith "Bad PDF cap"

let join_of_pdfjoin = function
  | 0 -> Shapes.MitredJoin
  | 1 -> Shapes.RoundJoin
  | 2 -> Shapes.BevelJoin
  | _ -> failwith "bad PDF join"

let rec scene_of_graphic pdf resources graphic = function
  | Pdfgraphics.Path (p, a)::t ->
      let fill =
        match a.Pdfgraphics.path_fill with
        | Some (colourspace, vals) ->
            let fill =
              fill_of_pdf_colour pdf resources vals
              a.Pdfgraphics.path_transparency.Pdfgraphics.fill_transparency colourspace
            in
              let fill_geom = Basic (fill, Path p) in
                [Obj (Id.new_ids (), fill_geom, Pdftransform.i, Over)]
        | None -> []
      and line =
        match a.Pdfgraphics.path_line with
        | Some (colourspace, vals) ->
            let fill =
              fill_of_pdf_colour pdf resources vals
              a.Pdfgraphics.path_transparency.Pdfgraphics.line_transparency colourspace
            and strokespec =
              {Shapes.startcap = cap_of_pdfcap a.Pdfgraphics.path_capstyle;
               Shapes.join = join_of_pdfjoin a.Pdfgraphics.path_joinstyle;
               Shapes.endcap = cap_of_pdfcap a.Pdfgraphics.path_capstyle;
               Shapes.linewidth = a.Pdfgraphics.path_linewidth;
               Shapes.mitrelimit = a.Pdfgraphics.path_mitrelimit}
            in
              let line_geom = Basic (fill, StrokedPath (p, strokespec)) in
                [Obj (Id.new_ids (), line_geom, Pdftransform.i, Over)]
        | None -> [] 
      in
       line @ fill @ scene_of_graphic pdf resources graphic t
  | Pdfgraphics.MCSection (_, g)::t
  | Pdfgraphics.MCSectionProperties (_, _, g)::t ->
      scene_of_graphic pdf resources graphic g @
      scene_of_graphic pdf resources graphic t
  | Pdfgraphics.Clip (_, elts)::t ->
      scene_of_graphic pdf resources graphic elts @
      scene_of_graphic pdf resources graphic t
  | h::t -> scene_of_graphic pdf resources graphic t
  | [] -> []

let scene_of_graphic pdf graphic =
  scene_of_graphic pdf graphic.Pdfgraphics.resources graphic graphic.Pdfgraphics.elements

