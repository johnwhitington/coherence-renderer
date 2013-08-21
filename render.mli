(** Rendering scenes and changes *)

(** Debug PDFs will be produced during the rendering process *)
val pdf_debug_active : bool ref

(** Debug PDFs will be produced during the filter rendering process *)
val pdf_filter_debug_active : bool ref

(** {2 Scenes} *)

(** Constructive Planar Geometry operations. *)
type cpg_op = Union | Intersection | Subtraction | ExclusiveOr

(** Resolution independent primitives for pages, grids etc. Specified in floats
so rounding is abstracted. We have horizontal and vertical lines and rectangles. *)
and primitive =
  | HLine of float * float * float
  | VLine of float * float * float
  | Rectangle of float * float * float * float

(** Basic types of geometry. *)
and basicshape =
  | Path of Pdfgraphics.path
  | Brushstroke of Brush.brushstroke
  | StrokedPath of Pdfgraphics.path * Shapes.strokespec
  | CPG of cpg_op * basicshape * basicshape

and filter_function = Sprite.sprite -> renderobject -> Sprite.shape -> Sprite.sprite

and dirty_function = Sprite.shape -> renderobject -> Sprite.shape

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

(** More complicated kinds of geometry. *)
and geometry  =
  | Filter of filter
  | Basic of Fill.fill * basicshape
  | Convolved of Convolve.kernel * geometry
  | Group of scene
  | Primitive of Colour.colour * primitive
     
(** Compositing operators *)
and compop =
  | NoCover
  | Over
  | PreTrans of float * compop
  
(** Something which can be rendered. *)  
and renderobject =
  Obj of Id.idset * geometry * Pdftransform.transform * compop

(** A scene consisted of an ordered list of these. *)
and scene = renderobject list

(** Project the Idset from a renderobject *)
val idset_in : renderobject -> Id.idset

(** Equality over objects *)
val obj_eq : renderobject -> renderobject -> bool

(* Build a standard object from given geometry with [NoCover] as the compositing operator. *)
val fakeobj : geometry -> renderobject

(** {2 Selections and handles} *)

(** A selection box is either for sizing or rotating/shearing about a given
centre. *)
type selectbox = Size | Rotate of int * int

(** Possible handle locations *)
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

(** A handle list for a single selection. *)
type handlelist = (handle * Id.idset * renderobject option ref) list

(** A single selection, containing some objects and a list of handles. *)
type selection = renderobject list * handlelist 

(** A selection, together with box type. *)
type selections = selectbox * selection

(** Calculate a set of fresh handles for sizing. *)
val handles_size : unit -> handlelist

(** Ditto for rotating. *)
val handles_rotate : unit -> handlelist

(** The null selection. *)
val null_selection : selections

(** Is a renderobject in a selection? *)
val is_selected : selections -> renderobject -> bool

(** Calculate the renderobject representing a handle. *)
val renderobject_of_handle : selections -> handle -> renderobject

(** Calculate the scene representing the given selections *)
val drawable_of_selection : selections -> scene

(** {2 Views} *)

(** A view (window) on a document. *)
type view =
  {mutable scene : scene;
   mutable pages : scene;
   mutable window : Wxgui.window;
   mutable background : scene;
   mutable selections : selections;
   mutable master_update : Sprite.shape;
   mutable rubberband : (int * int * int * int) option;
   mutable tool : Wxgui.tool}

(** Possible results of a pick operation. *)
type picked =
  | PickedObject of renderobject
  | PickedSelectionHandle of renderobject * renderobject list * handle
  | PickedNone

(** Find out what is at position (x, y) in a view *)
val pick : int -> int -> view -> picked

(** {2 Positioning objects} *)

(** Possible anchor positions for use with [position_anchor] *)
type anchor =
  | Left | TopLeft | Top | TopRight | Right
  | BottomRight | Bottom | BottomLeft | Centre

(** Translate a renderobject so that the given anchor is at the given position *)
val position_anchor : anchor -> (float * float) -> renderobject -> renderobject

(** Translate a renderobject in (x, y) *)
val translate_renderobject : int -> int -> renderobject ->  renderobject

(** Transform a renderobject with a single transform operation *)
val transform_renderobject : Pdftransform.transform_op -> renderobject -> renderobject

val transform_basicshape : Pdftransform.transform -> geometry -> geometry
 
(** Transform a renderobject with a [Transform.transform] *)
val transform_renderobject_many : Pdftransform.transform -> renderobject -> renderobject

(** {2 Building simple shapes} *)

(** Convenience function to build a renderobject for given path, fill,
transform and compositing operator. *)
val mkpoly :
  Pdfgraphics.path -> Fill.fill -> Pdftransform.transform -> compop -> renderobject

(** Same for a rectangle (x, y, w, h) *)
val mkrectangle :
  float -> float -> float -> float -> Fill.fill ->
  Pdftransform.transform -> compop -> renderobject

(** Same, but building a basic shape from (x, y, w, h) *)
val rectangle : float -> float -> float -> float -> basicshape

(** Build a group from a scene *)
val mkgroup : scene -> renderobject

(** Make a renderobject from a primitive and colour *)
val primobj : Colour.colour -> primitive -> renderobject 

(** {2 Dirty regions} *)

(** The dirty region when an object changes *)
val dirty_region :
  renderobject -> renderobject -> (Sprite.shape -> Sprite.shape)

(** The dirty region when an object with fancy fill is transformed. *)
val alldirty : renderobject -> renderobject -> (Sprite.shape -> Sprite.shape)

(** Compose dirty filters of all filters above the LMO. *)
val dirty_filter : Id.idset -> Sprite.shape -> scene -> Sprite.shape

(** {2 Shapes and minshapes} *)

(** The shape of a renderobject (assuming it's a basic shape) *)
val shapeonly_of_basicshape : renderobject -> Sprite.shape

(** Calculate integer bounding box (xmin, xmax, ymin, ymax) for a renderobject.
Not guaranteed minimal. *)
val bounds_of_basicshape : renderobject -> int * int * int * int

(** Calculate the floating point bounds of an object. Again, not guaranteed minimal *) 
val proper_bounds : renderobject -> float * float * float * float

(** {2 Rendering} *)

(** Render a frame, given a LMO, view, shape to render in. [topobjects] allows objects to be put on top the scene. *)
val render_frame :
  ?display_selection:bool ->
  ?topobjects:scene ->
    Id.idset -> view -> Sprite.shape -> Sprite.sprite

(** Render a scene in a shape, with default values for the parameters *)
val render_simple_scene : scene -> Sprite.shape -> Sprite.sprite

val scene_of_graphic : Pdf.t -> Pdfgraphics.t -> scene

val string_of_obj : renderobject -> string


