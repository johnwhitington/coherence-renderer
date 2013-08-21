(** GUI Icons *)

(** Given a PDF, make it the right size for an icon. *)
val fit_icon_scene : Render.scene -> Render.scene

(** Render an icon to raw data at 32x32 *)
val render_icon : Render.scene -> Canvas.canvas

