(** Cross-platform graphical interface *)

(** The platforms *)
type platform = Windows | Mac | Gtk | Unknown

(** Events *)
type internal_event

type window = int
type button = int
type x = int
type y = int

type key =
  | Escape
  | Left
  | Up
  | Right
  | Down
  | A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  | UnknownKey of int

type event =
  | AppStartup
  | AppClose
  | OpenFile of string
  | WindowClosed of window
  | ButtonClicked of window * button
  | KeyDown of window * key
  | KeyUp of window * key
  | LeftDown of window * x * y
  | LeftUp of window * x * y
  | RightDown of window * x * y
  | RightUp of window * x * y
  | LeftDragging of window * x * y
  | PaintRect of window * int * int * int * int
  | BlurSlider of window * int
  | TransSlider of window * int
  | Internal of internal_event
  | NullEvent

val string_of_event : event -> string

type icon = Canvas.canvas

type startup_data =
  {select_icon : icon;
   zoom_icon : icon;
   totop_icon : icon;
   tobottom_icon : icon}

(** Given an event handler, start polling. *)
val startup : startup_data -> unit

(** Close the application from the ML side. *)
val close_application : unit -> unit

(** Exit the WxGui library *)
val shutdown : unit -> unit

(** Poll for events *)
val poll : unit -> event

(** Get the dimensions of the screen *)
val get_screen_size : unit -> int * int

(** Get the platform *)
val get_platform : unit -> platform

val set_status_bar : (window * string) -> unit

(** The null window *)
val nullwindow : window

(** Make a window, given a title string *)
val make_window : string -> int -> int -> int -> int -> int -> int -> bool -> window

(** Close a window *)
val close_window : window -> unit

(** Delete a window which Wxwidgets has closed *)
val delete_window : window -> unit

(** Calling [refresh_window window xmin ymin xmax ymax] prompts the window that an area
requires updating. A canvas representing the area will have already been
placed in the canvas reference. *) 
val refresh_window : window -> (int * int * int * int) -> unit

(** Get the width of a window *)
val get_window_width : window -> int

(** Get the height of a window *)
val get_window_height : window -> int

(** Get both at once *)
val get_window_size : window -> int * int

(** Get dx, dy from window *)
val get_window_scroll : window -> int * int

(** Get up-to-date mouse coordinates *)
val mouse_coords : window -> int * int

(** Capture the mouse on a window *)
val capture_mouse : window -> unit

(** Release the mouse on a window *)
val release_mouse : window -> unit

(** Set the blur slider *)
val set_blurslider : window -> int -> unit

(** Set the transparency slider *)
val set_transslider : window -> int -> unit

(** Set the cache text *)
val set_cachetext : string -> unit

(** Modifier keys. *)
val shift_down : bool ref

val command_down : bool ref

val option_down : bool ref

(** Read the path of the executable. *)
val executable_path : string

val open_cachewindow : unit -> unit

(** {2 The canvas} *)

(** Calling [plot_sprite dx dy (canvas, width) sprite] plots [sprite], offset by
[(dx, dy)] on a [canvas] which is [width] pixels wide. *)
val plot_sprite : window -> int -> int -> Sprite.sprite -> unit

(** Plots a shape in a similar manner given a colour to fill it with. *)
val plot_shape : window -> int -> int -> Colour.colour -> Sprite.shape -> unit

type tool = Select | Zoom

