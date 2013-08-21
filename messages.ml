(* \chaptertitle{Messages}{User Interface Text} *)

(* Command key is the Apple key on OS X and the Control key on other platforms.
*)
let command = "Command"
  (*i match Wxgui.get_platform () with
  | Wxgui.Windows | Wxgui.Gtk -> "Ctrl"
  | Wxgui.Mac -> "Command" i*)

(* Option key is Option key on OS X and Alt key on other platforms. *)
let option =
  "Alt"
  (*i match Wxgui.get_platform () with
  | Wxgui.Windows | Wxgui.Gtk -> "Alt"
  | Wxgui.Mac -> "Option" i*)
  
(* \section{Select tool} *)

(* When no objects are selected. *)
let nonesel =
  "Click to select an object; drag to select many; " ^ command ^
  "-A to select all."

(* When some objects are selected. *)
let objsel = 
  "Drag to move selected objects; Right click adds to selection; " ^
  command ^ "-Click for multiple selections."

(* When the selection is being resized by dragging. *)
let dragsize =
  "Hold " ^ command ^
  " for proportional scaling. Hold shift to scale around centre"

(* Ditto for rotation / shear mode. *)
let dragrotate =
  "Hold " ^ command ^
  " to constrain rotation. Drag crosshair to change centre."

(* When dragging a rubberband. *)
let dragrubberband =
  "Press escape to cancel."

(* \section{Zoom tool} *)

(* When the zoom tool is selected.*)
let zoomhints =
  "Drag to zoom to a rectangle; left and right buttons to zoom in/out"

(* \vspace{10mm}

\noindent\textit{For simplicity, this module has no interface.} *)

