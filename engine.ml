(* \chaptertitle{Engine}{The main program} *)
open Pdfutil
open Render
open Sprite
open Examples

(* There is at most one debug window. *)
let debug = true

(* Do we open the cache status window? *)
let cache_debug = false

let _ = flprint "ML: Beginning of Engine module\n"

let path = Wxgui.executable_path

let _ = Examples.path := path

let _ = clear Sprite.debug_spritecheck

let captured = ref false

(* \section{Defining the example objects} *)

(* Convolve a given render object with kernel [k]. If it's a filter, the
geometry of the filter is convolved instead. Only one level of convolution is
allowed. *)
let rec convolve_renderobject k (Obj (_, geom, t, c)) =
  match geom with
  | Convolved (_, x) ->
      convolve_renderobject k (Obj (Id.new_ids (), x, t, c))
  | Filter ({geometry = Convolved (_, x)} as f) ->
      Obj (Id.new_ids (), Filter {f with geometry = Convolved (k, x)}, t, c)
  | Filter f ->
      Obj (Id.new_ids (), Filter {f with geometry = Convolved (k, f.geometry)}, t, c)
  | _ ->
      Obj (Id.new_ids (), Convolved (k, geom), t, c)

(* Use [convolve_renderobject] to blur a given object [o].*)
let blur_renderobject o =
  convolve_renderobject (Convolve.mkgaussian o)

(*i let convolved_group () =
  blur_renderobject 4 (curves ()) i*)

(* Remove any convolution from a renderobject *)
let unconvolve_renderobject ((Obj (_, geom, t, c)) as obj) =
  match geom with
  | Convolved (_, x) ->
      Obj (Id.new_ids (), x, t, c)
  | Filter ({geometry = Convolved (_, x)} as filter) ->
      Obj (Id.new_ids (), Filter {filter with geometry = x}, t, c)
  | _ ->
      obj

let trans_geometry_filter v = function
  | {geometry = Basic (_, basicshape)} as f ->
      {f with geometry =
        Basic (Fill.plain (Colour.dissolve ~delta:v Colour.white), basicshape)}
  | x -> x

(* Change transparency. If it's a filter, we alter the geometry of the filter.
If it's anything else, we just change the compositing operator *)
let trans_renderobject v (Obj (id, geom, t, op)) =
  match geom with
  | Filter f ->
     Obj (id, Filter (trans_geometry_filter v f), t, op)
  | _ -> 
     Obj (id, geom, t, PreTrans (float_of_int v /. 255., Over))

(* Canvas background *)
let background =
  primobj Colour.lightgrey (Rectangle (0., 0., 1280., 1024.))

(* A page. Primitives cannot currently be transformed. *)
let page x y w h =
  let r = x +. w and b = y +. h in
    [primobj
       Colour.white
       (Rectangle (x, y, x +. w, y +. h));
     primobj
       (Colour.dissolve_between Colour.black Colour.white 200)
       (Rectangle (x -. 1., y -. 1., r +. 1., b +. 1.));
     blur_renderobject 4
       (Obj (Id.new_ids (),
            Basic (Fill.plain (Colour.dissolve_between Colour.black Colour.white 120),
                   rectangle (x +. 6.) (y +. 6.) w h), Pdftransform.i, Over))]

let debug_background =
  primobj Colour.white (Rectangle (0., 0., 1280., 1024.))

let getshape = shapeonly_of_basicshape

(* \section{Window setup} *)

(* The scene and selection list. These are calculated from the PDF each time
something needs rendering. *)
let pages = page 100. 100. 400. 400.

let master_update =
  Sprite.box 0 0 400 400

let debug_view =
  {scene = [];
   pages = [];
   window = Wxgui.nullwindow;
   background = [debug_background];
   selections = null_selection;
   master_update = master_update;
   rubberband = None;
   tool = Wxgui.Select}

let views =
  if debug then ref [debug_view] else ref []

let remove_view win =
  views := lose (fun v -> v.window = win) !views

let rec pickview window = function
  | [] ->
      Printf.printf "Window is %s\n" (string_of_int window);
      failwith "pickview: window didn't exist!"
  | {window = w} as h::_ when w = window -> h
  | _::t -> pickview window t

let pickview window =
  pickview window !views

(* Icons. *)
let fit_icon pdf =
  let scene =
    match Pdfpage.pages_of_pagetree pdf with
    | [] -> failwith "Icon file has no pages"
    | page::_ ->
        let graphic, pdf, resources =
          Pdfgraphics.graphic_of_page pdf page, pdf, page.Pdfpage.resources
        in
          let objs = Render.scene_of_graphic pdf graphic in
            [Obj (Id.new_ids (), Group objs, Pdftransform.i, Over)]
  in
    Icons.fit_icon_scene scene

let icon_of_pdf file =
  Icons.render_icon <| fit_icon (Pdfread.pdf_of_file None None file)

let _ =
  clear Cache.usecache;
  try
    let select = icon_of_pdf (path ^ "/pointer.pdf")
    and zoom = icon_of_pdf (path ^ "/zoom.pdf")
    and totop = icon_of_pdf (path ^ "/up.pdf")
    and tobot = icon_of_pdf (path ^ "/down.pdf") in
      Wxgui.startup
        {Wxgui.select_icon = select;
         Wxgui.zoom_icon = zoom;
         Wxgui.totop_icon = totop;
         Wxgui.tobottom_icon = tobot}
  with
    e -> flprint (Printexc.to_string e ^ "\n"); Wxgui.shutdown ()

(* Dragging objects around. For [DragObject], we hold the [x] and [y]
coordinates of the last motion, the original scene and original selections. For
[DragHandle], we hold the [x] and [y] coordinates of the initial start of drag,
the handle type, renderobject representing the handle, list of objects in the
original selection, original scene and original selections. The state for
[DragRubberband] is stored in the [Render.view]. *)
type dragtype =
  | DragNone
  | DragObject of int * int * scene * selections 
  | DragHandle of int * int * handle * renderobject * scene * scene * selections 
  | DragRubberband

(* Global state. *)
type stateflags =
  {mutable previous_event : Wxgui.event option; (*r The previous event *)
   mutable previous_mouse_event : Wxgui.event option;
   mutable dragging : dragtype; (*r What, if anything is being dragged. *)
   mutable outlines : bool; (*r Is this an outline drag? *)
   mutable justpicked : bool} (*r if we've just picked an object *)

let stateflags =
  {previous_event = None;
   previous_mouse_event = None;
   dragging = DragNone;
   outlines = false;
   justpicked = false}

let old_debug_coords = ref NullShape

let minimal_window_number = ref 0

(* When we're asked to render something by the interface. *)
let render_rect win x y w h =
  Printf.printf "passive render: win %i, x %i, y %i, w %i, h %i, debug = %i\n" win x y w h !minimal_window_number;
  flprint "\n";
  (* Intersect the box with 0..1279, 0..1023 *)
  match box_overlap 0 0 1279 1023 x y (x + w - 1) (y + h - 1) with
  | None -> ()
  | Some (x0, y0, x1, y1) ->
    let x = x0 and y = y0 and w = x1 - x0 + 1 and h = y1 - y0 + 1 in
      let sprite =
        render_frame
          ~display_selection:true (Id.new_ids ()) (pickview win) (Sprite.box x y w h)
      in
        Wxgui.plot_sprite win 0 0 sprite;
        Wxgui.refresh_window win (x, y, x + w - 1, y + h - 1)

(* When we force the update. Force an update in an lmo, view and update shape. *)
let force_update selections lmo view shape =
  (* 1. Get the part of the canvas visible *)
  let w, h = Wxgui.get_window_size view.window
  and dx, dy = Wxgui.get_window_scroll view.window in
  let x, y, w, h = dx, dy, w, h in
  (* 2. Intersect the shape with that *)
  let shape = shape &&& Sprite.box x y w h in
  (* 3. And then with master update (FIXME: Should not be necessary. Check) *)
  let shape = shape &&& view.master_update in
  match render_frame ~display_selection:selections lmo view shape with
  | Sprite (Box (x0, y0, x1, y1), _) as rendered ->
      Wxgui.plot_sprite view.window 0 0 rendered;
      Wxgui.refresh_window view.window (x0, y0, x1, y1);
      (* Debug window code *)
      if debug && view.window = !minimal_window_number then
        begin let tx0, ty0, tx1, ty1 =
          match (!old_debug_coords ||| (shape_of_sprite rendered)) &&& view.master_update with
          | Shape (Box (x0, y0, x1, y1), _) -> x0, y0, x1, y1
          | _ -> failwith "nullshape / debug force update"
        in
          Wxgui.plot_shape debug_view.window 0 0 Colour.white !old_debug_coords;
          Wxgui.plot_shape debug_view.window 0 0 Colour.lightgray (Sprite.shape_of_sprite rendered);
          Wxgui.refresh_window debug_view.window (tx0, ty0, tx1, ty1);
          old_debug_coords := Sprite.shape_of_sprite rendered
        end
  | _ -> ()

(* Update whole view (such as is visible on screen) *)
let force_update_whole view =
  let w, h = Wxgui.get_window_size view.window
  and dx, dy = Wxgui.get_window_scroll view.window in
    force_update true (Id.new_ids ()) view (Sprite.box dx dy w h)

(* Update a given rectangle [x0, y0, x1, y1]. *)
let update_rect selections lmo view x0 y0 x1 y1 =
  let xmin, xmax, ymin, ymax =
    min x0 x1, max x0 x1, min y0 y1, max y0 y1
  in
    let w = xmax - xmin + 1 and h = ymax - ymin + 1 in
      force_update selections lmo view (Sprite.box xmin ymin w h)

(* Update when a rubberband changes. *)
let update_rubberband r' view =
  match view.rubberband, r' with
  | None, None -> ()
  | None, Some (x0, y0, x1, y1) | Some (x0, y0, x1, y1), None ->
      view.rubberband <- r';
      update_rect true (Id.new_ids ()) view x0 y0 x1 y1
  | Some (x0, y0, x1, y1), Some (x0', y0', x1', y1') ->
      let xmin, xmax, ymin, ymax =
        box_union
          (min x0 x1, max x0 x1, min y0 y1, max y0 y1)
          (min x0' x1', max x0' x1', min y0' y1', max y0' y1')
      in
        view.rubberband <- r';
        update_rect true (Id.new_ids ()) view xmin ymin xmax ymax

(* Update the blursider to reflect the blur level of the selection. If the blur
level is mixed or the selection empty, set it to zero. *)
let update_blurslider_selection view =
  let level =
    let convolve_level = function
      | Obj (_, (Convolved (l, _) | Filter {geometry = Convolved (l, _)}), _, _) ->
          Some (Convolve.radius_of_kernel l)
      | _ -> None
    in
      let levels =
        option_map
          (fun obj ->
             if is_selected view.selections obj
               then convolve_level obj else None)
          view.scene
      in
        match levels with
        | [] -> 0
        | ls ->
            let ls = sort compare ls in
              if last ls = hd ls then hd ls else 0
  in
    Wxgui.set_blurslider view.window level

(* Update the transslider to reflect the global transparency level of the
selection. If the selection is mixed or empty, set it to fully opaque (255). *)
(* FIXME: Doesn't reflect filter properly at the moment - encode transparency
properly in filter.*)
let update_transslider_selection view =
  let level =
    let trans_level = function
      | Obj (_, Filter {geometry = Basic ({Fill.fillsingle = fs}, _)}, _, _) ->
          Colour.alpha_of_colour (fs 0 0)
      | Obj (_, _, _, op) ->
          match op with
          | PreTrans (v, _) -> toint (v *. 255.)
          | _ -> 255
    in
      let levels =
        option_map
          (fun obj ->
             if is_selected view.selections obj
               then Some (trans_level obj) else None)
          view.scene
      in
        match levels with
        | [] -> 255
        | ls ->
            let ls = sort compare ls in
              if last ls = hd ls then hd ls else 255
  in
    Wxgui.set_transslider view.window level

(* Update the canvas to reflect a change in selection. \FIXME{Make this only do
the minimum change!, by not using view.selections, but passing it explicitly}
Also update the blur slider. *)
let change_selection ?(noupdate = false) view selections' =
  let drawn = drawable_of_selection view.selections
  and drawn' = drawable_of_selection selections' in
    let shapes = map getshape (drawn @ drawn') in
      let totalshape = fold_left ( ||| ) Sprite.NullShape shapes in
        view.selections <- selections';
        if not noupdate then
          begin
            update_blurslider_selection view;
            update_transslider_selection view;
            force_update
              true (Id.new_ids ()) view (totalshape &&& view.master_update)
          end

let redraw_selections view =
  change_selection view view.selections

(* Select all objects in a view. *)
let select_all view =
  let selections' = Size, (view.scene, handles_size ()) in
    change_selection view selections'

(* Take a scene, and split it into two lists of lists (both empty, or of
numerically adjacent lengths), the first containing those contiguous sections,
in order, which are true under [pred], and the second the remaining objects in
the same fashion. It also returns a boolean indicating if the top one is
selected. These three pieces of information are sufficient to reconstruct the
original list (actually, the boolean is not strictly required since it is
related to which list is longer, but we don't want to have to calculate that to
reconstruct). The function [add_to_lol] adds an element to a list of lists,
placing it in a new list if [isnew] is [true], otherwise appending it to the
front of the first list. *)
let rec unleave_scene tr fa last scene pred =
  let add_to_lol isnew k = function
    | [] -> [[k]]
    | l when isnew -> [k]::l
    | h::t -> (k::h)::t
  in
    match scene, last with
    | [], _ -> rev_map rev tr, rev_map rev fa
    | o::os, true ->
        if pred o
          then unleave_scene (add_to_lol false o tr) fa true os pred
          else unleave_scene tr (add_to_lol true o fa) false os pred
    | o::os, false ->
        if pred o
          then unleave_scene (add_to_lol true o tr) fa true os pred
          else unleave_scene tr (add_to_lol false o fa) false os pred

let unleave_scene p s =
  match s with
  | [] -> [], [], false
  | h::_ ->
      let tr, fa = unleave_scene [] [] false s p in
        tr, fa, p h

(* The reverse operation: interleaving *)
let rec interleave a = function
  | [] -> a
  | h::t -> h::interleave t a

let interleave (tr, fa, trfirst) =
  (* Resolve this name clash between Utility and Sprite *)
  flatten
    (if trfirst then interleave fa tr else interleave tr fa)

(* [os] and [os'] are equal length lists. If [obj] is in [os], return the
analogous element of [os']. *)
let rec getpair os os' obj =
  match os, os' with
  | [], [] -> None
  | h::t, h'::t' ->
      if obj_eq h obj then Some h' else getpair t t' obj
  | _ -> failwith "Engine.getpair: unequal list lengths."

(* Change the objects in [os] for those in [os'] in [selections], also putting
new handles in for each one. *)
let selection_change_object box os os' (renderobjects, _) =
  let renderobjects' =
    map
      (fun obj -> match getpair os os' obj with None -> obj | Some obj' -> obj')
      renderobjects
  in
    let handles' =
      match box with
      | Size -> handles_size ()
      | Rotate _ -> handles_rotate () 
    in
      renderobjects', handles'

let selections_change_objects os os' (box, selections) =
  box, selection_change_object box os os' selections

let isfilter = function
  | Obj (_, Filter _, _, _) -> true
  | _ -> false

(* Change a scene according to [f], updating the screen. [f] is a function on
each selected objects. The boolean values [first] and [last] can be set to
indicate that the redraw area ought to include the initial and final selection
respectively. *)
let view_map_selected_objects
  ?(filtersfancy=false) f first last ?originalscene ?originalselections view
=
  let selected, not_selected, topselected =
    unleave_scene (is_selected view.selections) view.scene
  and selected_in_originalscene, _, _ =
    unleave_scene
      (is_selected
         (match originalselections with
          | None -> view.selections
          | Some s -> s))
      (match originalscene with None -> view.scene | Some o -> o)
  in
    let selected' = map_lol f selected_in_originalscene in
      let scene' = interleave (selected', not_selected, topselected) in
        let selected'_flat = flatten selected'
        and selected_flat = flatten selected in
          let selections' =
            selections_change_objects selected_flat selected'_flat view.selections
          in
      let dirtyregions =
        map2
          (fun o o' ->
            (if isfilter o && filtersfancy
               then alldirty o o'
               else (dirty_region o o')) view.master_update)
          selected_flat selected'_flat
      in
        let filtered =
          (* \FIXME{Again, need proper LMO here. (over doing it!)} *)
          map
            (fun region -> dirty_filter (Id.new_ids ()) region scene')
            dirtyregions
        in
          let dirtyselections =
            let old_drawables =
              if first
                then drawable_of_selection view.selections
                else (ignore (drawable_of_selection view.selections); [])
            and new_drawables =
              if last
                then drawable_of_selection selections'
                else (ignore (drawable_of_selection selections'); [])
            in
              map getshape old_drawables @ map getshape new_drawables
          in
            view.scene <- scene';
            view.selections <- selections';
            (* \FIXME{Need a real value of LMO here\ldots} *)
            let totaldirty =
              fold_left ( ||| ) Sprite.NullShape (filtered @ dirtyselections)
            in
              force_update last (Id.new_ids ()) view totaldirty

(* Set operations on scenes. \NOTE{These functions require no duplicated objects
in a scene.} \smallgap *)

(* Member predicate. *)
let rec scene_member o = function
  | [] -> false
  | h::t -> obj_eq h o || scene_member o t

(* The set containing just those elements present in both arguemnts. *)
let rec scene_and a = function
  | [] -> []
  | h::t ->
     if scene_member h a
       then h::scene_and a t
       else scene_and a t

(* The set containing all elements in the first argument which do not appear in
the second. *)
let rec scene_minus a b =
  match a with
  | [] -> []
  | h::t ->
      if scene_member h b
        then scene_minus t b
        else h::scene_minus t b

(* The set containing all the elements in both arguments. *)
let scene_or a b =
  a @ scene_minus b a

(* The set containing all elements in one or both of the input lists but not
both. *)
let scene_exclusive_or a b =
  scene_minus (scene_or a b) (scene_and a b)

(* The function [view_change_scene] changes from one scene to another (for
instance when undoing or moving objects to the back). We compare the two scenes,
and modify dirty regions for filters etc.

In order to calculate the dirty region, we must look at what has changed between
the old and new scenes by looking at their IDs. The set of objects which have
been added, deleted or changed are $s\oplus s'$.

In addition, we must consider objects which have changed depth-order. In this
case, the picture needs updating, but the IDs for the objects which have moved
are identical, since they are still represented by the same rasterized
representation. We calculate two lists of objects, one containing the objects in
$s\wedge s'$ in the order in which they appear in [s], and the other in the
order in which they appear in [s']. These two lists are mutual permutations.
From this we can calculate which objects have changed in depth.

In the above, there is the extra complication of filters: Filters are treated
like with other objects but, in addition, for each filter which appears in both
scenes:

\begin{itemize}

  \item Find the changes between the old scene below the old position of the
  filter and the new scene below the new position of the filter

  \item Pass these changes through the dirty function of the filter, and add
  this to the dirty shape, and intersect with the shape of the filter.

\end{itemize}

In addition, [selections'] may provide a changed selection, so this is also
added to the dirty region. \smallgap*)

(* \NOTE{\onsq} Given two scenes, find the list of pairs of objects which
have changed depth. [allpairs] finds all the pairs made from an element and a
list; [orderpairs] gives all the orderings in a list; [inorder] gives is an
order predicate; [obj_of_idset] finds the object with a given ID set in a list.
In all cases these functions operate only on sets. The main body of the
function finds the mutual permutations and finds those changed in depth. *)
let depthchanged s s' =
  let rec allpairs x = function
    | [] -> []
    | h::t -> (x, h)::allpairs x t 
  and orderpairs = function
    | [] -> []
    | h::t -> allpairs h t @ orderpairs t
  and inorder x y = function
    | [] -> failwith "inorder"
    | h::_ when h = x -> true
    | h::_ when h = y -> false
    | _::t -> inorder x y t
  and obj_of_idset i = function
    | [] -> failwith "depthchanged"
    | Obj (i', _, _, _) as h::t when Id.set_eq i i' -> h
    | _::t -> obj_of_idset i t
  in
    let ids_s = map idset_in s
    and ids_s' = map idset_in s' in
      (* We remove any which are not in both *)
      let ids_s = keep (fun s -> mem s ids_s') ids_s
      and ids_s' = keep (fun s -> mem s ids_s) ids_s' in
        let pairs = orderpairs ids_s in
          let orders = lose (fun (x, y) -> inorder x y ids_s') pairs in
            map (fun (i, i') -> obj_of_idset i s, obj_of_idset i' s) orders

(* Find all objects in a scene below that one which has the id given. *)
let scene_below obj s =
  tl (dropwhile (notpred (obj_eq obj)) s)

let rec scene_changes s s' =
  let changed =
    map getshape (scene_exclusive_or s s')
  and changed_duetodepth =
    map (fun (o, o') -> getshape o &&& getshape o') (depthchanged s s')
  in
    let filter_changes =
      let filters_in_both_scenes =
        let in_before = keep isfilter s
        and in_after = keep isfilter s' in
          scene_and in_before in_after
      in
        map
          (function
            | Obj (_, Filter f, _, _) as obj ->
                let scene_below_before = scene_below obj s
                and scene_below_after = scene_below obj s' in
                  (f.dirty (scene_changes scene_below_before scene_below_after) obj)
                     &&&
                  (getshape obj)

            | _ -> assert false)
          filters_in_both_scenes
    in
      fold_left ( ||| ) Sprite.NullShape (changed @ changed_duetodepth @ filter_changes)

let view_change_scene view scene' selections' first last =
  let main_change =
    scene_changes view.scene scene'
  and selections_before =
    if first then drawable_of_selection view.selections else []
  and selections_after =
    if last then drawable_of_selection selections' else []
  in
    let selections_shapes =
      map getshape (selections_before @ selections_after)
    in
      let totaldirty =
        fold_left ( ||| ) Sprite.NullShape (main_change::selections_shapes)
      in
        view.scene <- scene';
        view.selections <- selections';
        force_update true (Id.new_ids ()) view totaldirty

(* Move the selected objects in [view] by [(dx, dy)]. *)
let move_selected first dx dy view =
  view_map_selected_objects
    (translate_renderobject dx dy) first false view

(* Transform the selected objects in [view] by [t]. *)
let transform_selected first t view originalscene originalselections =
  view_map_selected_objects
    (transform_renderobject t) first false ~originalscene ~originalselections view

(* Deselect an object from the selection. *)
let selections_lose renderobject (box, selections) =
  let selection_lose o (os, handles) =
    let os' = lose (fun obj -> Id.set_eq (idset_in o) (idset_in obj)) os in
      os', handles
  in
    box, (selection_lose renderobject) selections
 
(* Stick new handles in all selections *)
let rehandle_selections (box, selections) =
  let handles = match box with
    | Size -> handles_size ()
    | Rotate _ -> handles_rotate ()
  in
    box, (fun (objects, _) -> objects, handles) selections

(* We add to the first selection in the list. *)
let add_to_existing_selection o handles (box, (os, h)) =
  box, (o::os, h)

(* Find the bounds of the selections [xmin, xmax, ymin, ymax]. *)
let bounds_of_selections selections =
  let boundslist = map bounds_of_basicshape (fst (snd selections)) in
    let xmin, xmax, ymin, ymax = pair_reduce box_union boundslist in
       let xmin = float xmin and xmax = float xmax
       and ymin = float ymin and ymax = float ymax in
         xmin, xmax, ymin, ymax

(* Find the centre point of the selections. *)
let centre_of_selections selections =
  let xmin, xmax, ymin, ymax = bounds_of_selections selections in
    toint ((xmin +. xmax) /. 2.), toint ((ymin +. ymax) /. 2.)

let selection_to_top view =
  let selected, not_selected =
    List.partition (is_selected view.selections) view.scene
  in
    view_change_scene
      view (selected @ not_selected) view.selections false false

let selection_to_bottom view =
  let selected, not_selected =
    List.partition (is_selected view.selections) view.scene
  in
    view_change_scene
      view (not_selected @ selected) view.selections false false

(* \section{Handling mouse events} *)

(* A left button down event with the select tool.
\smallgap

\begin{tabular}{l | l}
  \textbf{Picked} & \textbf {Action} \\ \hline
  Object not in selection & Add to or change the selection. \\
  Object in current selection & Start translation dragging \\
  Handle in current selection & Start handle dragging \\
  None (pasteboard or canvas) & Deselect all; start a new selection box
\end{tabular} *)
let select_leftdown x y view =
  stateflags.justpicked <- false;
  match pick x y view with
  | PickedObject renderobject ->
      flprint (string_of_int 1);
      if not (is_selected view.selections renderobject) then
        begin
          stateflags.justpicked <- true;
          (* Deselect the others visibly, then select ours invisibly. *)
          change_selection view null_selection;
          change_selection
            ~noupdate:true view (Size, ([renderobject], handles_size ()));
          Wxgui.set_status_bar (view.window, Messages.objsel)
        end;
      stateflags.dragging <- DragObject (x, y, view.scene, view.selections);
      stateflags.outlines <- !Wxgui.option_down;
      Wxgui.capture_mouse view.window;
      set captured
  | PickedSelectionHandle (renderobject, renderobjects, handle) ->
      stateflags.dragging <-
        DragHandle
          (x, y, handle, renderobject, renderobjects, view.scene, view.selections);
      stateflags.outlines <- !Wxgui.option_down;
      Wxgui.capture_mouse view.window;
      set captured;
      let statustext =
        match fst view.selections with
        | Size -> Messages.dragsize
        | Rotate _ -> Messages.dragrotate
      in
        Wxgui.set_status_bar (view.window, statustext)
  | PickedNone ->
      stateflags.dragging <- DragRubberband;
      update_rubberband (Some (x, y, x, y)) view

let zoom_leftdown x y view  =
  stateflags.dragging <- DragRubberband;
  update_rubberband (Some (x, y, x, y)) view

(* A left button dragging event with the select tool.
\smallgap

\begin{tabular}{l | l}
  \textbf{Picked} & \textbf {Action} \\ \hline
  Object not in selection & -- \\
  Object in current selection & If dragging, do translation \\
  Handle in current selection & If dragging, update scene \\
  None (pasteboard or canvas) & If dragging, update selection rubberband
\end{tabular} *)
let select_dragging_object view x y xo yo =
  let dx = x - xo and dy = y - yo in
    let first =
      match stateflags.justpicked, stateflags.previous_event with
      | false, Some (Wxgui.LeftDown _) -> true
      | _ -> false
    in
      move_selected first dx dy view;
      stateflags.dragging <-
        match stateflags.dragging with
        | DragObject (_, _, s, sel) -> DragObject (x, y, s, sel)
        | _ -> failwith "select_dragging_object: bad drag state."

let select_dragging_crosshair x y view renderobject renderobjects =
  match view.selections with
  | Size, _ ->
      failwith "Not in rotation mode when dragging rotation crosshair"
  | Rotate (xo, yo), ss ->
      let selections' = Rotate (x, y), ss
      and old_shape = shapeonly_of_basicshape renderobject in
        let new_shape = translate_shape (x - xo) (y - yo) old_shape in
          view.selections <- rehandle_selections selections';
          force_update true (Id.new_ids ()) view (old_shape ||| new_shape);
          match stateflags.dragging with
          | DragHandle (_, _, HandleRotationCentre, _, _, oscene, oselections) ->
              let renderobject' = renderobject_of_handle view.selections HandleRotationCentre in
                let draghandle' =
                  DragHandle
                    (x, y, HandleRotationCentre, renderobject', renderobjects, oscene, oselections)
                in
                  stateflags.dragging <- draghandle'
          | _ -> failwith "select_dragging_crosshair: Inconsistency"

(* Calculate the transformation for a size handle given the mouse coordinates,
the original position of the the handle at the beginning of the drag, the
coordinates of the object box and the handle type. *)
let dragging_action_size x y xo yo xmin xmax ymin ymax handle =
  let mkscale (cx, cy) sx sy =
    Pdftransform.Scale ((cx, cy), sx, sy)
  and mkscale_corner (cx, cy) sx sy =
    let sx = safe_float sx and sy = safe_float sy in
      let (cx, cy) =
        if !Wxgui.shift_down
          then ((xmax +. xmin) /. 2., (ymax +. ymin) /. 2.)
          else (cx, cy)
      in
        if !Wxgui.command_down
          then Pdftransform.Scale ((cx, cy), fmin sx sy, fmin sx sy)
          else Pdftransform.Scale ((cx, cy), sx, sy)
  in
    match handle with
    | HandleTopLeft ->
        let scale_x = (x -. xmax) /. (xo -. xmax)
        and scale_y = (y -. ymax) /. (yo -. ymax) in
          mkscale_corner (xmax, ymax) scale_x scale_y 
    | HandleTopMiddle ->
        let scale_y = (y -. ymax) /. (yo -. ymax) in
          mkscale (xmax, ymax) 1. scale_y
    | HandleTopRight ->
        let scale_x = (x -. xmin) /. (xo -. xmin)
        and scale_y = (y -. ymax) /. (yo -. ymax) in
          mkscale_corner (xmin, ymax) scale_x scale_y
    | HandleLeftMiddle ->
        let scale_x = (x -. xmax) /. (xo -. xmax) in
          mkscale (xmax, ymax) scale_x 1.
    | HandleRightMiddle ->
        let scale_x = (x -. xmin) /. (xo -. xmin) in
          mkscale (xmin, ymin) scale_x 1.
    | HandleBottomLeft ->
        let scale_x = (x -. xmax) /. (xo -. xmax)
        and scale_y = (y -. ymin) /. (yo -. ymin) in
          mkscale_corner (xmax, ymin) scale_x scale_y
    | HandleBottomMiddle ->
        let scale_y = (y -. ymin) /. (yo -. ymin) in
          mkscale (xmin, ymin) 1. scale_y
    | HandleBottomRight ->
        let scale_x = (x -. xmin) /. (xo -. xmin)
        and scale_y = (y -. ymin) /. (yo -. ymin) in
          mkscale_corner (xmin, ymin) scale_x scale_y
    | HandleRotationCentre ->
        failwith "Engine.transform_of_handle: Inconsistency."

let dragging_action_rotate cx cy x y xo yo handle =
  let cx = float cx and cy = float cy in
    match handle with
    | HandleTopLeft
    | HandleTopRight
    | HandleBottomLeft
    | HandleBottomRight ->
        let angle = Shapes.rotation (cx, cy) (xo, yo) (x, y) in
          let angle' =
            if !Wxgui.shift_down
              then Shapes.restrict_angle (rad_of_deg 45.) angle
              else angle
          in
            Pdftransform.Rotate ((cx, cy), angle')
    | HandleTopMiddle
    | HandleBottomMiddle ->
        Pdftransform.ShearX ((cx, cy), (x -. cx) /. (y -. cy))
    | HandleLeftMiddle
    | HandleRightMiddle ->
        Pdftransform.ShearY ((cx, cy), (y -. cy) /. (x -. cx))
    | HandleRotationCentre ->
        failwith "Engine.transform_of_handle: Inconsistency. "

let select_dragging_handle x y xo yo view _ rs handle originalscene originalselections =
  let first =
    match stateflags.justpicked, stateflags.previous_event with
    | false, Some (Wxgui.LeftDown _) -> true
    | _ -> false
  in
    let transform_of_handle (xo, yo) (x, y) view handle =
      let xmin, xmax, ymin, ymax = bounds_of_selections originalselections in
        let xo, yo, x, y = float xo, float yo, float x, float y in
          match fst view.selections with
          | Size -> dragging_action_size x y xo yo xmin xmax ymin ymax handle
          | Rotate (cx, cy) -> dragging_action_rotate cx cy x y xo yo handle
    in
      let t = transform_of_handle (xo, yo) (x, y) view handle in
        transform_selected first t view originalscene originalselections;
        let renderobject' = renderobject_of_handle view.selections handle in
          stateflags.dragging <-
            DragHandle (xo, yo, handle, renderobject', rs, originalscene, originalselections)

let select_dragging x y view =
  stateflags.justpicked <- false;
  match stateflags.dragging with
  | DragObject (xo, yo, _, _) ->
      select_dragging_object view x y xo yo
  | DragHandle (_, _, HandleRotationCentre, r, rs, _, _) ->
      select_dragging_crosshair x y view r rs
  | DragHandle (xo, yo, handle, r, rs, oscene, oselections) ->
      select_dragging_handle x y xo yo view r rs handle oscene oselections
  | DragRubberband ->
      Wxgui.set_status_bar (view.window, Messages.dragrubberband);
      let rubberband' =
        match view.rubberband with
        | None -> None
        | Some (x0, y0, _, _) -> Some (x0, y0, x, y)
      in
        update_rubberband rubberband' view
  | DragNone -> ()

let zoom_dragging x y view =
  Wxgui.set_status_bar (view.window, Messages.dragrubberband);
  let rubberband' =
    match view.rubberband with
    | None -> None
    | Some (x0, y0, _, _) -> Some (x0, y0, x, y)
  in
    update_rubberband rubberband' view

(* A left button up event with the select tool.
\smallgap

\begin{tabular}{l | l}
  \textbf{Picked} & \textbf {Action} \\ \hline
  Object not in selection & -- \\
  Object in current selection & If dragging, stop \\
  Handle in current selection & If dragging, stop \\
  None (pasteboard or canvas) & If dragging, end rubberband else deselect all
\end{tabular} *)
let select_leftup x y view =
  begin
    match pick x y view with
    | PickedObject renderobject ->
        begin
          let selected = is_selected view.selections renderobject in
            match selected, stateflags.previous_event, stateflags.justpicked with
            | true, Some (Wxgui.LeftDown _), false ->
                flprint (string_of_int 2);
                let selections' =
                  match view.selections with
                  | Size, x ->
                      let cx, cy = centre_of_selections view.selections in
                         Rotate (cx, cy), x
                  | Rotate _, x -> Size, x
                in
                  change_selection view (rehandle_selections selections')
            | true, Some (Wxgui.LeftDown _), true ->
                flprint (string_of_int 3);
                redraw_selections view
            | _, Some (Wxgui.LeftDragging _), _ ->
               (* Reset rotation centre if required. *)
               flprint (string_of_int 4);
               begin
                 match view.selections with
                 | Rotate _, ss ->
                     let cx, cy = centre_of_selections view.selections in
                       change_selection view (Rotate (cx, cy), ss)
                 | _ -> redraw_selections view
               end
            | _ -> ()
        end
    | _ -> ()
  end;
  begin
    match stateflags.dragging with
    | DragHandle _ -> redraw_selections view
    | DragRubberband ->
        (* Remove the rubberband if required. *)
        let old = view.rubberband in
          update_rubberband None view;
        (* Now see what's been selected. *)
        begin
          match old with
          | None -> ()
          | Some (x0, y0, x1, y1) when x0 = x1 && y0 = y1 ->
              change_selection view null_selection
          | Some (x0, y0, x1, y1) ->
              let xmax = max x0 x1 and xmin = min x0 x1
              and ymax = max y0 y1 and ymin = min y0 y1 in
                let selected_objects =
                  keep
                    (fun o ->
                       let ox0, ox1, oy0, oy1 = bounds_of_basicshape o in
                         let oxmax = max ox0 ox1 and oxmin = min ox0 ox1
                         and oymax = max oy0 oy1 and oymin = min oy0 oy1 in
                           match
                             box_overlap xmin ymin xmax ymax oxmin oymin oxmax oymax
                           with
                           | None -> false
                           | Some _ -> true)
                    view.scene
                in
                  match selected_objects with
                  | [] -> change_selection view null_selection
                  | _ ->
                    let selections = Size, (selected_objects, handles_size ()) in
                      change_selection view selections
        end
    | _ -> ()
  end;
  stateflags.dragging <- DragNone;
  if view.selections = null_selection
    then Wxgui.set_status_bar (view.window, Messages.nonesel)
    else Wxgui.set_status_bar (view.window, Messages.objsel);
  if !captured then
    begin
      clear captured;
      Wxgui.release_mouse view.window
    end

(* Update the selections in a scene when no objects are added or deleted. Causes
no update.  This is used for instance in zooming, when the whole window will be
updated anyway. *)
let update_selections objs view =
  let _, objs_sel' =
    split
      (keep
        (fun (o, _) -> Render.is_selected view.selections o)
        (combine objs view.scene))
  in
    view.selections <-
      match view.selections with
        selectbox, (renderobjects, handles) ->
           rehandle_selections (selectbox, (objs_sel', handles))

(* Zoom a view according to a [Transform.transform]. *)
let zoom_transform view t =
  let zoom o = Render.transform_renderobject_many t o in
    (*i view.background <- List.map g view.scene;
    view.pages <- List.map g view.scene; i*)
    let old = view.scene in
      view.scene <- map zoom view.scene;
      update_selections old view;
      force_update_whole view

(* Zoom a view by factor [z] about [cx, cy] *)
let zoom_about view cx cy z =
  zoom_transform view [Pdftransform.Scale ((cx, cy), z, z)] 

(* Left mouse button up with the zoom tool. *)
let zoom_leftup _ _ view =
  match stateflags.dragging, view.rubberband with
  | DragRubberband, Some (x0, y0, x1, y1) when x0 <> x1 && y0 <> y1 ->
      (* Rubberband has finished, zoom to rectangle. \NOTE{We don't erase the
      rubber band here --- zooming will update the window anyway.} *)
      stateflags.dragging <- DragNone;
      view.rubberband <- None;
      if view.selections = null_selection
        then Wxgui.set_status_bar (view.window, Messages.nonesel)
        else Wxgui.set_status_bar (view.window, Messages.objsel);
      (* Calculate the centre and scale of the zoom. \NOTE{No possibility of
      division by zero here because of the [when] condition above} *)
      let view_w, view_h = Wxgui.get_window_size view.window in
        let dx = float (view_w / 2 - (x0 + x1) / 2)
        and dy = float (view_h / 2 - (y0 + y1) / 2)
        and sx = float view_w /. float (abs (x0 - x1)) 
        and sy = float view_h /. float (abs (y0 - y1)) in
          let scale = fmin sx sy (*r Pick the right scale. *)
          and c = float view_w /. 2., float view_h /. 2. in
            let tr =
              [Pdftransform.Scale (c, scale, scale); Pdftransform.Translate (dx, dy)]
            in
              zoom_transform view tr
  | DragRubberband, _ ->
      flprint "Zooming by two\n";
      let cx = float (Wxgui.get_window_width view.window / 2)
      and cy = float (Wxgui.get_window_height view.window / 2) in
        zoom_about view cx cy 2.
  | _ -> ()

(* A right button down event with the select tool. Only use is in selection
modification. *)
let select_rightdown x y view =
  stateflags.justpicked <- false;
  match pick x y view with
  | PickedObject renderobject ->
      stateflags.dragging <- DragObject (x, y, view.scene, view.selections);
      if is_selected view.selections renderobject
        then (* Deselect just this one. *)
          change_selection view (selections_lose renderobject view.selections)
        else (* Add this one. *)
          let handles = match fst view.selections with
            | Size -> handles_size ()
            | Rotate _ -> handles_rotate ()
          in
            change_selection
              view
              (add_to_existing_selection renderobject handles view.selections)
  | _ -> ()

let select_rightup x y view =
  stateflags.dragging <- DragNone

let zoom_rightdown _ _ _ = ()

let zoom_rightup _ _ view =
  match view.rubberband with
  | Some (x0, y0, x1, y1) when (x0, y0) <> (x1, y1) -> ()
  | _ ->
    let cx = float (Wxgui.get_window_width view.window / 2)
    and cy = float (Wxgui.get_window_height view.window / 2) in
      zoom_about view cx cy 0.5

(* Nudge an object by one pixel. Rather inefficient at the moment. *)
let nudge_selection dx dy view =
  view_map_selected_objects (translate_renderobject(*i _freezei*) dx dy) true false view;
  let selections' =
    match view.selections with
    | Rotate (_, _), x ->
        let cx, cy = centre_of_selections view.selections in
           Rotate (cx, cy), x
    | Size, x -> Size, x
  in
    change_selection view (rehandle_selections selections')

(* The escape key has two functions: if nothing is being dragged, it deselects
all objects. If something is being dragged, it ends aborts the drag, restoring
the pre-drag state. *)
let select_keydown view = function
  | Wxgui.Escape ->
      begin
        match stateflags.dragging with
        | DragNone -> change_selection view null_selection
        | DragObject (_, _, oldscene, oldselections)
        | DragHandle (_, _, _, _, _, oldscene, oldselections) ->
            view_change_scene view oldscene oldselections false true;
            stateflags.dragging <- DragNone
        | DragRubberband ->
            update_rubberband None view;
            stateflags.dragging <- DragNone;
            if view.selections = null_selection
              then Wxgui.set_status_bar (view.window, Messages.nonesel)
              else Wxgui.set_status_bar (view.window, Messages.objsel)
      end
  | Wxgui.Left -> nudge_selection ~-1 0 view
  | Wxgui.Right -> nudge_selection 1 0 view
  | Wxgui.Down -> nudge_selection 0 1 view
  | Wxgui.Up -> nudge_selection 0 ~-1 view
  | _ -> ()

(* Key down in select mode. *)
let zoom_keydown view = function
  | Wxgui.Escape ->
      begin match stateflags.dragging with
        | DragRubberband ->
            update_rubberband None view;
            stateflags.dragging <- DragNone;
            if view.selections = null_selection
              then Wxgui.set_status_bar (view.window, Messages.nonesel)
              else Wxgui.set_status_bar (view.window,  Messages.objsel)
        | _ -> ()
      end
  | _ -> ()

(* Key up in select mode. *)
let select_keyup view k =
  match k, !Wxgui.command_down, !Wxgui.shift_down, !Wxgui.option_down with 
  | Wxgui.A, true, false, false ->
     (* Select all done on key up to avoid it re-occurring on key repeat. *)
     select_all view
  | Wxgui.B, true, false, false ->
     (* Move selection to back. *)
     selection_to_bottom view
  | Wxgui.F, true, false, false ->
     (* Move selection to front. *)
     selection_to_top view
  | _ -> ()

(* Key up in zoom mode. *)
let zoom_keyup _ _  = ()

(* Called when the select tool is chosen. *)
let selecttool window =
  let view = pickview window in
    view.tool <- Wxgui.Select;
    match view.selections with
    | (_, ([], [])) -> Wxgui.set_status_bar (window, Messages.nonesel)
    | _ -> Wxgui.set_status_bar (window, Messages.objsel)

(* Called when the zoom tool is chosen. *)
let zoomtool window =
  let view = pickview window in
    view.tool <- Wxgui.Zoom;
    Wxgui.set_status_bar (window, Messages.zoomhints)

(* Called when move-to-top is clicked *)
let totoptool window =
  let view = pickview window in
    selection_to_top view

(* Called when move-to-bottom is clicked *)
let tobottool window =
  let view = pickview window in
    selection_to_bottom view 

(* Called when the blur slider is altered. We alter the objects and the
selection.  *)
let changeblur view value =
  assert (value >= 0);
  let f =
    if value = 0
      then unconvolve_renderobject
      else convolve_renderobject (Convolve.mkgaussian value)
  in
    view_map_selected_objects f true true view

(* Ditto for transparency *)
let changetrans view value =
  assert (value >= 0 && value <= 255);
  view_map_selected_objects ~filtersfancy:true (trans_renderobject value) true true view

let openfile name =
  let graphic, pdf, resources = pdf_graphic_from_file name in
    let page_w, page_h =
      match Pdfpage.pages_of_pagetree pdf with
      | page::_ ->
          let x, y, x2, y2 = Pdf.parse_rectangle page.Pdfpage.mediabox in
            fabs (x2 -. x), fabs (y2 -. y)
      | [] -> failwith "PDF file has no pages"
    in
      let objs =
        Render.scene_of_graphic pdf graphic
      and tr =
        let scale = Pdftransform.Scale ((0., page_h /. 2.), 1., ~-.1.)
        and translate = Pdftransform.Translate (100., 100.) in
          let transform = [translate; scale] in
            Render.transform_renderobject_many transform
      in
        let objs' = rev_map tr objs in
          let view =
            {scene = objs';
             pages = page 100. 100. page_w page_h;
             window = Wxgui.nullwindow;
             background = [background];
             selections = null_selection;
             master_update = master_update;
             rubberband = None;
             tool = Wxgui.Select}
          in
            views =| view;
            view.window <- Wxgui.make_window name 600 400 200 200 1280 1024 true

(* Make a standard window for the demo, not starting with a file *)
let opendemo (sx, sy, dx, dy, canvas_w, canvas_h) renderobjects name =
  let view =
    {scene = renderobjects;
     pages = page 50. 50. (float canvas_w) (float canvas_h);
     window = Wxgui.nullwindow;
     background = [background];
     selections = null_selection;
     master_update = Sprite.box 0 0 (canvas_w + 150) (canvas_h + 150);
     rubberband = None;
     tool = Wxgui.Select}
  in
    views =| view;
    view.window <- Wxgui.make_window name sx sy dx dy 0 0 true

let onexit s =
  Sprite.write_debug_pdf s

exception AppExit

(* Code to be executed after initialisation of the WxWidgets part. *)
let _ =
  if debug then
    debug_view.window <-
      begin match Wxgui.get_platform () with
      | Wxgui.Mac ->
          Wxgui.make_window "Debug window" 600 300 0 400 560 240 false
      | _ ->
          Wxgui.make_window "Debug window" 600 300 0 400 560 240 false
      end;
      Wxgui.set_status_bar (debug_view.window, "This window shows the redraw region of the window above.")

let x = ref 0
and y = ref 0

let isnew x' y' =
  x' <> !x || y' <> !y

let mouse x' y' =
  x := x';
  y := y'

(* A couple of quick functions to ease things - simple, since we know
[position_anchor] will be used. *)
let scale n =
  transform_renderobject (Pdftransform.Scale ((0., 0.), n, n))

let flipy =
  transform_renderobject (Pdftransform.Scale ((0., 0.), 1., ~-.1.))

let flipx =
  transform_renderobject (Pdftransform.Scale ((0., 0.), ~-.1., 1.))

let rotate a =
  transform_renderobject (Pdftransform.Rotate ((0., 0.), rad_of_deg a))

let blur = blur_renderobject

let fade = trans_renderobject

let move = position_anchor

let movex n =
  Render.transform_renderobject (Pdftransform.Translate (n, 0.))

let movexy (x, y) =
  Render.transform_renderobject (Pdftransform.Translate (x, y))

let line col thick p p' =
  let path =
    Pdfgraphics.EvenOdd, [Pdfgraphics.Not_hole, Pdfgraphics.Open, [Pdfgraphics.Straight (p, p')]] 
  and strokespec =
    {Shapes.startcap = Shapes.ButtCap;
     Shapes.join = Shapes.BevelJoin;
     Shapes.endcap = Shapes.ButtCap;
     Shapes.mitrelimit = 10.;
     Shapes.linewidth = thick}
  in
    object_of_geometry (Basic (Fill.plain col, StrokedPath (path, strokespec)))

(* Put a black border around an object to highlight it. *)
let border_of_obj width colour (Obj (_, geom, tr, _)) =
  match geom with
  | Basic (_, Path p)
  | Filter {geometry = Basic (_, Path p)} ->
      let spec =
        {Shapes.startcap = Shapes.ButtCap;
         Shapes.join = Shapes.MitredJoin;
         Shapes.endcap = Shapes.ButtCap;
         Shapes.mitrelimit = 1.;
         Shapes.linewidth = width}
      in
      Obj (Id.new_ids (),
           Basic (Fill.plain colour, StrokedPath (p, spec)),
           tr, Over)
  | _ -> failwith "Can't use border here"

let border width colour obj =
  Obj (Id.new_ids (), Group [border_of_obj width colour obj; obj], Pdftransform.i, Over)

let move_scene (x, y) =
  map (Render.translate_renderobject x y)

let rotate n =
  Render.transform_renderobject (Pdftransform.Rotate ((0., 0.), rad_of_deg n))

let random_circle () =
  let rc = Colour.colour_of_rgba 35 67 23 255
  and rx = float_of_int <| Random.int 400
  and ry = float_of_int <| Random.int 400
  and rr = float_of_int <| Random.int 80 in
    object_of_geometry
      (Basic (Fill.plain rc, (Path (Shapes.circle rx ry rr))))

let p1, p2, p3, p4 =
  match Wxgui.get_platform () with
  | Wxgui.Mac ->
      (600, 300, 0, 30, 500, 150),
      (650, 600, 650, 30, 550, 350),
      (650, 500, 650, 610, 550, 250),
      (600, 350, 0, 710, 500, 100)
  | _ ->
      (600, 400, 0, 30, 500, 150),
      (650, 600, 650, 30, 550, 350),
      (650, 500, 650, 610, 550, 250),
      (600, 350, 0, 710, 500, 100)

let opendemos () =
  (* Demonstration of Minimal Rendering *)
  opendemo
    p1
    [move TopLeft (50., 10.) (scale 1.3 (flipy (mintext1 ()))); 
     move TopLeft (50., 220.) (scale 1.3 (flipy (mintext2 ())));
     move Centre (100., 120.) (scale 0.4 (brushcircle ()));
     move Centre (100., 130.) (scale 0.8 blurfilter);
     move Centre (100., 120.) (scale 0.4 (flipy (smalllion ())));
     move Centre (500., 120.) (scale 0.7 (flipx (brush ())));
     ]
    "Minimal Rendering";
  (* Demonstration of Filters *)
  opendemo
    p2
    (
     [move TopLeft (50., 10.) (scale 1.3 (flipy (filtertext1 ()))); 
     move TopLeft (50., 418.) (scale 1.3 (flipy (filtertext2 ())))] @
    (move_scene (10, 0)
    [
     move TopLeft (50., 250.) (fade 128 affinefilter);
     (border_of_obj 1. (Colour.dissolve Colour.black ~delta:128)
     (move Centre (150., 200.) wireframe));
     move Centre (150., 200.) wireframe;
     (border_of_obj 1. (Colour.dissolve Colour.black ~delta:128)
     (move Centre (350., 200.) blurfilter));
     move Centre (350., 200.) blurfilter;
     (border_of_obj 1. (Colour.dissolve Colour.black ~delta:128)
     (move Centre (250., 200.) rgbfilter));
     move Centre (250., 200.) rgbfilter;
     (border_of_obj 1. (Colour.dissolve Colour.black ~delta:128)
     (move Centre (450., 200.) monofilter));
     move Centre (450., 200.) monofilter;
     move Centre (100., 180.) (scale 0.5 (flipy (smalllion ())));
     move Centre (200., 200.) (fade 128 (blur 3 (scale 2. (flipy (logo ())))));
     move Centre (300., 200.) (flipy (q_shape_2 ()));
     move Centre (400., 200.) (brushblue ());
     move Centre (500., 200.) (rotate 25. (scale 0.5 cpg_example))
    ]))
    "Filters";
  (* Brushstroke filter, CPG filters, smearing. *)
  opendemo
    p3
     ([move TopLeft (50., 10.) (scale 1.3 (flipy (lionfilter1 ()))); 
     move TopLeft (50., 320.) (scale 1.3 (flipy (lionfilter2 ())))] @
    (move_scene (40, 20)
    [
     move Centre (125., 150.) (smear ());
     move Centre (125., 150.) (scale 0.6 (flipy (smalllion ())));
     move Centre (300., 150.) (wirebrush ());
     move Centre (300., 150.) (scale 0.6 (flipy (smalllion ())));
     move Centre (475., 150.) (monobrush ());
     move Centre (475., 150.) (scale 0.6 (flipy (smalllion ())))
    ]))
    "Filters II";
  (* text, lines of thickness etc. *)
  opendemo
    p4
    [
      (*i move Centre (60., 60.) & scale 0.4 redblob; i*)
      move TopLeft (50., 10.) (scale 1.3 (flipy (aatext ())));
      move TopLeft (300., 70.) (scale 0.5 (flipy (aatext ())));
      move TopLeft (300., 90.) (scale 0.7 (flipy (aatext ())));
      move TopLeft (300., 110.) (scale 0.9 (flipy (aatext ())));
      movex 30.
      (mkgroup
        [move Centre (200., 100.) (scale 0.5 (rotate 0. (p6_curve ())));
      move Centre (200., 100.) (scale 0.5 (rotate 10. (p6_curve ()))); 
      move Centre (200., 100.) (scale 0.5 (rotate 20. (p6_curve ()))); 
      move Centre (200., 100.) (scale 0.5 (rotate 30. (p6_curve ()))); 
      move Centre (200., 100.) (scale 0.5 (rotate 40. (p6_curve ()))); 
      move Centre (200., 100.) (scale 0.5 (rotate 50. (p6_curve ()))); 
      move Centre (200., 100.) (scale 0.5 (rotate 60. (p6_curve ()))); 
      move Centre (200., 100.) (scale 0.5 (rotate 70. (p6_curve ())))]);
      move TopLeft (80., 55.)
       (mkgroup
        [movex 50. (line Colour.green 1. (10., 60.) (10., 100.));
      movex 60. (line Colour.green 1. (10., 60.) (20., 100.));
      movex 70. (line Colour.green 1. (10., 60.) (30., 100.));
      movex 80. (line Colour.green 1. (10., 60.) (40., 100.));
      movex 90. (line Colour.green 1. (10., 60.) (50., 100.));
      movex 100. (line Colour.green 1. (10., 60.) (60., 100.));
      movexy (50., 50.) (line Colour.green 2. (10., 60.) (10., 100.));
      movexy (60., 50.) (line Colour.green 2. (10., 60.) (20., 100.));
      movexy (70., 50.) (line Colour.green 2. (10., 60.) (30., 100.));
      movexy (80., 50.) (line Colour.green 2. (10., 60.) (40., 100.));
      movexy (90., 50.) (line Colour.green 2. (10., 60.) (50., 100.));
      movexy (100., 50.) (line Colour.green 2. (10., 60.) (60., 100.))
        ])
    ]
    "Antialiasing improvements";
  minimal_window_number := (hd !views).window

let notdebug win =
  win <> debug_view.window

let event_handler event =
  begin match event with
  | Wxgui.AppStartup ->
      Printf.printf "ML: AppStartup:\n";
      if cache_debug then Wxgui.open_cachewindow ();
      (*i openfile (path ^ "/text.pdf"); i*)
      opendemos ()
  | Wxgui.WindowClosed win ->
      flprint "ML: Engine got WindowClosed\n";
      remove_view win;
      Wxgui.delete_window win;
      if !views = [] then raise AppExit
  | Wxgui.AppClose ->
      flprint "ML: AppClose\n";
      iter
        (fun w -> Wxgui.close_window w; remove_view w)
        (map (function v -> v.window) !views);
      raise AppExit
  | Wxgui.OpenFile filename ->
      openfile filename
  | Wxgui.LeftDown (win, x, y) ->
      (*i flprint "D"; i*)
      if notdebug win then begin
        let x2, y2 = Wxgui.mouse_coords win in
        mouse x2 y2;
        let view = pickview win in
          begin match view.tool with
          | Wxgui.Zoom -> zoom_leftdown x y view
          | Wxgui.Select -> select_leftdown x y view
          end
      end
  | Wxgui.LeftUp (win, x, y) ->
      (*i flprint "U\n"; i*)
      if notdebug win then begin
        let x2, y2 = Wxgui.mouse_coords win in
        mouse x2 y2;
        let view = pickview win in
          begin match view.tool with
          | Wxgui.Zoom -> zoom_leftup x y view
          | Wxgui.Select -> select_leftup x y view
          end
      end
  | Wxgui.LeftDragging (win, x, y) ->
      if notdebug win then begin
        if cache_debug then
          Wxgui.set_cachetext (Cache.string_of_cachestate ());
        let x, y = Wxgui.mouse_coords win in
          match stateflags.previous_mouse_event, isnew x y with
          | Some Wxgui.LeftDragging _, true
          | Some Wxgui.LeftDown _, true ->
            let view = pickview win in
              (*i flprint "*"; i*)
              mouse x y;
              begin match view.tool with
              | Wxgui.Zoom -> zoom_dragging x y view
              | Wxgui.Select -> select_dragging x y view
              end
          | _ ->
            (*i flprint "-"; i*)
            mouse x y
      end
  | Wxgui.RightDown (win, x, y) ->
      if notdebug win then begin
        let x2, y2 = Wxgui.mouse_coords win in
          mouse x2 y2;
          let view = pickview win in
            begin match view.tool with
            | Wxgui.Zoom -> zoom_rightdown x y view
            | Wxgui.Select -> select_rightdown x y view
            end
      end
  | Wxgui.RightUp (win, x, y) ->
      if notdebug win then begin
        let x2, y2 = Wxgui.mouse_coords win in
          mouse x2 y2;
          let view = pickview win in
          begin match view.tool with
          | Wxgui.Zoom -> zoom_rightup x y view
          | Wxgui.Select -> select_rightup x y view
          end
      end
  | Wxgui.KeyDown (win, k) ->
      let view = pickview win in
        begin match view.tool with
        | Wxgui.Zoom -> zoom_keydown view k
        | Wxgui.Select -> select_keydown view k
        end
  | Wxgui.KeyUp (win, k) ->
      let view = pickview win in
        begin match view.tool with
        | Wxgui.Zoom -> zoom_keyup view k
        | Wxgui.Select -> select_keyup view k
        end
  | Wxgui.PaintRect (win, x, y, w, h) ->
      render_rect win x y w h;
      (*i clear Render.pdf_debug_active; (* Only once *) i*)
      (*i clear Render.pdf_filter_debug_active i*)
  | Wxgui.ButtonClicked (win, b) ->
      begin match b with
      | 1 -> selecttool win
      | 2 -> zoomtool win
      | 3 -> totoptool win
      | 4 -> tobottool win
      | _ -> ()
      end
  | Wxgui.BlurSlider (win, value) ->
      changeblur (pickview win) value
  | Wxgui.TransSlider (win, value) ->
      changetrans (pickview win) value;
  | _ -> ()
  end;
  if event <> Wxgui.NullEvent then
    stateflags.previous_event <- Some event;
  begin match event with
  | Wxgui.LeftDown _
  | Wxgui.LeftUp _
  | Wxgui.RightDown _
  | Wxgui.RightUp _
  | Wxgui.LeftDragging _ ->
      stateflags.previous_mouse_event <- Some event
  | _ -> ()
  end

(* Remove any drag event which immediately follows a down event with
identical coordinates *)
let filter_event = function
  | Wxgui.LeftDragging (w, x, y) ->
      begin match stateflags.previous_event with
      | Some Wxgui.LeftDown (_, x2, y2) when x = x2 && y = y2 ->
          Wxgui.NullEvent
      | _ ->
        Wxgui.LeftDragging (w, x, y)
      end
  | e ->
     e

let _ =
  clear Render.pdf_debug_active;
  clear Render.pdf_filter_debug_active;
  set Cache.usecache;
  Cache.setsize (100 * 1024 * 1024);
  flprint "------------------------------------CACHE ON\n";
  try
    while true do event_handler <| filter_event (Wxgui.poll ()) done
  with
    | AppExit ->
      flprint "ML: Starting ocaml exit\n";
      Wxgui.close_application ();
      Wxgui.shutdown ();
      onexit "/Users/john/Desktop/debug.pdf";
      flprint "ML: Exiting Ocaml application normally\n"
    | e ->
      flprint (Printexc.to_string e);
      Wxgui.close_application ();
      Wxgui.shutdown ();
      (*i onexit "/Users/john/Desktop/debug.pdf"; i*)
      flprint "Exiting Ocaml application abnormally\n"

