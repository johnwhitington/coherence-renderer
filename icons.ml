(* \chaptertitle{Icons}{Toolbar icons} *)
open Pdfutil
open Render

(* Create a view from just a scene. *)
let view_from_scene scene =
  {scene = scene;
   pages = [];
   window = Wxgui.nullwindow;
   background = [primobj Colour.clear (Rectangle (1., 1., 32., 32.))];
   selections = (Size, ([], []));
   master_update = Sprite.box 1 1 32 32;
   rubberband = None;
   tool = Wxgui.Select}

(* Generate an icon of a certain size. For now, 32 x 32 hardcoded. *)
let fit_icon_scene scene =
  let scaled =
    map
      (transform_renderobject (Pdftransform.Scale ((0., 0.), 0.1, ~-.0.1)))
      scene
  in
    map (position_anchor Centre (16., 16.)) scaled

(* Render a 32x32 icon, given a scene. *)
let render_icon scene =
  let rendered = render_frame (Id.new_ids ()) (view_from_scene scene) (Sprite.box 1 1 32 32) in
    Sprite.flatten_sprite 0 rendered Colour.white

