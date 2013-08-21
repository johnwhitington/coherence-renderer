(* \chaptertitle{Filters}{Making filters} *)
open Pdfutil
open Render
open Sprite

(* \section{Common functions} *)

(* A null filter function. *)
let nullfilterfunction = fun spr _ _ -> spr

(* A null dirty function. *)
let nulldirty = fun shp _ -> shp

(* This is used when any impact upon an object requires all of it to be
recalculated. For instance, for smudge. *)
let totaldirty =
  fun shp renderobj ->
    let ourshp = shapeonly_of_basicshape renderobj in
      if Sprite.shape_intersects ourshp shp
        then shp ||| ourshp else shp
        
(* Give new, combined IDs to those objects in a scene satisfying some predicate
[p]. *)
let reparent_obj p filter_idset (Obj (i, g, t, c) as obj) =
  if p obj then Obj (Id.combine i filter_idset, g, t, c) else obj

(* List iterator on the previous function. *)
let reparent p filter_idset =
  map (reparent_obj p filter_idset)

(* Dirty region calculation for affine filters. We need to do
raster-to-geometric domain conversion, and use the polygon renderer to find the
tspransformed shape from the transformed geometry. An entirely raster based
version would be better. *)
let edgelist_of_span y (s, l) =
  let e = s + l - 1 in
    let s' = float s and e' = float e and y' = float y in
      [(s', y'), (e', y');
       (e', y'), (s', y')]

(* Transform a shape according to a [Transform.transform] *)
let transform_shape t shp =
  match shp with
  | Sprite.NullShape -> Sprite.NullShape
  | _ ->
    let edges =
      flatten
        (map
          (fun (s, y, l) -> edgelist_of_span y (s, l))
          (Sprite.spanlist_of_shape shp))
    in
      let transformed =
        map (fun (a, b) -> (Pdftransform.transform t a, Pdftransform.transform t b)) edges
      in
        let transformed_integer =
          let sub = Coord.sub_of_float in
            map
              (fun ((a, b), (c, d)) ->
                {Polygon.x0 = sub a; Polygon.y0 = sub b; Polygon.x1 = sub c; Polygon.y1 = sub d})
              transformed
        in
          fst (Polygon.shapeminshape_of_unsorted_edgelist transformed_integer Pdfgraphics.EvenOdd)

(* Standard bloating dirty function for blur / smear etc. We bloat the part of
the shape within the area of the bloated object, but leave the rest unchanged,
clipping also to the unbloated shape of the filter. *)
let bloatdirty xradius yradius =
  (fun shp obj ->
     let filter_shape = shapeonly_of_basicshape obj in
       let bloated_filter = Sprite.bloat xradius yradius filter_shape in
         let in_filter = bloated_filter &&& shp
         and out_filter = shp --- bloated_filter in
           let bloated = Sprite.bloat xradius yradius in_filter in
             let bloated_in_filter = bloated &&& bloated_filter in
               bloated_in_filter ||| out_filter)

(* Change the fills of non-filters according to a function from colours to colours. *)
let rec changefills f = function
  | Filter _ as geom -> geom (* We don't handle filters. *)
  | Convolved(k, g) ->
      Convolved(k, changefills f g)
  | Basic(fill, basicshape) ->
      Basic(fill.Fill.fillchangecolour f, basicshape)
  | Group objs ->
      Group(
        map
          (fun (Obj(id, geom, transform, compop)) ->
             (Obj(id, changefills f geom, transform, compop)))
          objs)
  | Primitive (a, b) -> Primitive (a, b) (*i failwith "Filters.changefills: No filters on
  primitives, please." i*)

(* \section{Wire frame filter} *)

(* Flatten groups, remove filters and primitives, and strip convolutions. *)
let rec wireframe_flatten_scene = function
  | [] -> []
  | Obj (id, geom, tr, c) as h::t ->
      let h' =
        match geom with
        | Filter _ | Primitive _ -> []
        | Group s -> wireframe_flatten_scene s
        | Basic _ -> [h]
        | Convolved (_, g) ->
            wireframe_flatten_scene [Obj (Id.new_ids (), g, tr, c)]
      in
        h' @ wireframe_flatten_scene t

(* Make a wire frame from a basic object *)
let rec mkwire fill strokespec (Obj (_, geom, tr, _)) =
  match geom with
  | Basic (f, b) ->
      let geom' =
        begin match b with
        | Path p | Brushstroke (_, p) | StrokedPath (p, _) ->
            (Basic (fill, StrokedPath (Polygon.transform_path tr p, strokespec)))
        | CPG (_, a, b) ->
            Group
              [mkwire fill strokespec (fakeobj <| Render.transform_basicshape tr (Basic (Fill.dummy, a)));
               mkwire fill strokespec (fakeobj <| Render.transform_basicshape tr (Basic (Fill.dummy, b)))]
        end
      in
        Obj (Id.new_ids (), geom', Pdftransform.i, Over)
  | _ -> failwith "Filters.mkwire"

(* Make a wire frame filter with given transform [f], path specification and
fill. For now, filters in the scene below are ignored. *)
let wireframe geometry strokespec fill =
  Filter{
    geometry = geometry;
    reading_scene =
    (* Reading shape is identity, [shptorender] is unmodified. *)
    (fun shp idset obj scene ->
        shp, shp,
         reparent always idset
           (map (mkwire fill strokespec) (wireframe_flatten_scene scene)));
    filter = nullfilterfunction;
    dirty =
     (* Take the dirty region and bloat it so as to include outlines. \NOTE{A more
     efficient way might be to union the maxshapes of the objects below
     and bloat that.} *)
     (fun d _ ->
       let w = toint (ceil strokespec.Shapes.linewidth) in
         Sprite.bloat w w d);
     filterkind = FilterPlain}

(* \section{Colour shift filter} *)

(* Offset the r, g, b channels of a scene by the given transformations.*)
let
  rgb_transform_object colour_function transform (Obj (_, geom, obj_transform, compop))
=
  let geom' =
    match geom with
    | Filter _ -> geom
    | _ -> changefills colour_function geom
  in
    fold_right
      transform_renderobject transform
      (Obj (Id.new_ids (), geom', obj_transform, compop))

type colourtrans = (float * float) -> Pdftransform.transform

let rgb geometry (rtf, gtf, btf) filltype =
  let centre = (0., 0.) in
  Filter{
    geometry = geometry;
    reading_scene =
      (fun shp _ (Obj(_, _, f, _)) scene ->
         shp,
         shp,
         flatten
          (rev (rev_map (fun obj ->
            [rgb_transform_object
              (fun c -> Colour.dissolve
                 (Colour.red_channel c) 64) (rtf (Pdftransform.transform f centre)) obj;
             rgb_transform_object
               (fun c -> Colour.dissolve
                 (Colour.green_channel c) 64) (gtf (Pdftransform.transform f centre)) obj;
             rgb_transform_object
                (fun c -> Colour.dissolve
                 (Colour.blue_channel c) 64) (btf (Pdftransform.transform f centre)) obj])
          scene)));
   filter = nullfilterfunction;
   (* Transform the dirty shape for all three channel transformations, and then union *)
   (* Making sure we take it module intersection with the shape of the filter *)
   dirty =
     (fun shp ((Obj(_, _, f, _)) as renderobject) ->
        let shp_r = transform_shape (rtf (Pdftransform.transform f centre)) shp
        and shp_g = transform_shape (gtf (Pdftransform.transform f centre)) shp
        and shp_b = transform_shape (btf (Pdftransform.transform f centre)) shp in
          let union = shp_b ||| shp_r ||| shp_g in
            let ourshape = shapeonly_of_basicshape renderobject in
              let infilter = union &&& ourshape in
                infilter ||| shp);
   filterkind = filltype}

(* \section{Smear filter} *)

(* Make a filter which smears along a brushstroke. *)
let smear ((brush, _) as brushstroke) =
  let w, h = Brush.sizeof_brush brush in
    let rx, ry = (w - 1) / 2, (h - 1) / 2 in
      Filter{
        geometry =
          Basic
            (Fill.plain Colour.white, Brushstroke (Brush.mkdummy brushstroke));
        reading_scene =
          (fun shp _ _ scene -> Sprite.bloat rx ry shp, shp, scene);
        filter =
          (fun spr (Obj (_, _, tr, _)) shp ->
             let spr' = Brush.smear spr (Brush.transform_brushstroke tr brushstroke) in
               let shp' = shp &&& (Sprite.shape_of_sprite spr') in
                 Sprite.portion spr' shp');
        dirty = totaldirty;
        filterkind = FilterFancy;
      }

(* \section{Scene hole filter} *)

(* Make a filter which cuts a hole in the scene. *)
let hole geometry =
  Filter {
    geometry = geometry;
    (* reduce the reading region to the intersection of the shape and the filter geometry *)
    reading_scene = (fun shp idset obj scene -> shp, shp, []);
    filter = nullfilterfunction;
    dirty = nulldirty;
    filterkind = FilterPlain}

(* \section{Monochrome filter} *)

(* Make the scene monochrome. *)
let monochrome geometry =
  Filter{
    geometry = geometry;
    reading_scene =
      (fun shp idset obj scene -> shp, shp, reparent never idset scene);
    filter =
      (fun spr _ _ -> Sprite.sprite_map Colour.monochrome spr);
    dirty = nulldirty;
    filterkind = FilterPlain}

(* \section{Blur filter} *)

(* Blur the scene. *)
let blur geometry kernel =
  let radius = Convolve.radius_of_kernel kernel in
    Filter{
      geometry = geometry;
      reading_scene =
        (fun shp idset obj scene ->
           let shp' = Sprite.bloat (radius * 2 + 1) (radius * 2 + 1) shp in
             shp', shp, reparent never idset scene);
      filter =
        (fun spr _ result_shape ->
           let bloated = Sprite.bloat radius radius (Sprite.shape_of_sprite spr) in
             let pickup = bloated &&& result_shape in
               Convolve.convolve_sprite_in_shape kernel spr bloated pickup);
      dirty = bloatdirty radius radius;
      filterkind = FilterPlain}

(* Filter out objects with certain IDs. *)
let rec remove_ids renderobjects ids =
  match renderobjects with
  | [] -> []
  | h::t ->
      if mem (idset_in h) ids
        then remove_ids t ids
        else h::remove_ids t ids

(* \section{Affine filter} *)

(* Affine transform filter *)
let affine filter_transform geometry =
  Filter
    {geometry = geometry;
     reading_scene =
       (fun shp idset obj scene ->
         shp, shp, map (transform_renderobject_many filter_transform) scene);
     filter = nullfilterfunction;
     dirty =
       (fun shp obj ->
         let filter_shape = shapeonly_of_basicshape obj in
           shp ||| (transform_shape filter_transform shp &&& filter_shape));
     filterkind = FilterPlain}

(* \section{Cutting through an object} *)

(* A single-object hole. *)
let minus geometry =
  Filter
    {geometry = geometry;
     reading_scene =
       (fun shp idset obj scene ->
          let intersection =
            (shapeonly_of_basicshape obj) &&& (shapeonly_of_basicshape (hd scene))
          in
            let intersection' = intersection &&& shp in
              intersection', intersection', tl scene);
       filter = nullfilterfunction;
       dirty = nulldirty;
       filterkind = FilterPlain}

(* A filter to swap the depth of objects. This is specific to objects, since
one cannot compare functions. Result not defined if elements do not exist *)
let swap_in_list o o' scene =
  let swap_in_array o o' scene_arr =
    let find_in_arr arr x =
      let s = ref 0 in
        while idset_in arr.(!s) <> x && !s < (Array.length arr) - 1 do incr s done; !s
    in
      let pos_o = find_in_arr scene_arr o
      and pos_o' = find_in_arr scene_arr o' in
        let tmp = scene_arr.(pos_o) in
          scene_arr.(pos_o) <- scene_arr.(pos_o');
          scene_arr.(pos_o') <- tmp;
          scene_arr
  in
    Array.to_list (swap_in_array o o' (Array.of_list scene))

(* \section{Depth-swapping filter} *)

(* This lens needs to be rebuilt when one or more of o, o' changes *)
let swapdepth o o' geometry =
  Filter
    {geometry = geometry;
     reading_scene =
       (fun shp idset renderobject scene -> shp, shp, swap_in_list o o' scene);
     filter = nullfilterfunction;
     dirty = nulldirty;
     filterkind = FilterPlain}

