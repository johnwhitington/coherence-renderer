(* \chaptertitle{Richgraphics}{A graphics environment} *)
open Pdfutil
open Camlpy
open Pdfio

type platform = Windows | Mac | Gtk | Unknown

type internal_event = Camlpy.marshallable

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

let string_of_event = function
  | AppStartup -> "AppStartup"
  | AppClose -> "AppClose"
  | OpenFile _ -> "OpenFile"
  | WindowClosed _ -> "WindowClosed"
  | ButtonClicked _ -> "ButtonClicked"
  | KeyDown _ -> "KeyDown"
  | KeyUp _ -> "KeyUp"
  | LeftDown (w, x, y) -> Printf.sprintf "LeftDown %i %i %i\n" w x y
  | LeftDragging (w, x, y) -> Printf.sprintf "LeftDragging %i %i %i\n" w x y
  | LeftUp (w, x, y) -> Printf.sprintf "LeftUp %i %i %i\n" w x y
  | RightDown (w, x, y) -> Printf.sprintf "RightDown %i %i %i\n" w x y
  | RightUp (w, x, y) -> Printf.sprintf "RightUp %i %i %i\n" w x y
  | PaintRect _ -> "PaintRect"
  | BlurSlider _ -> "BlurSlider"
  | TransSlider _ -> "TransSlider"
  | Internal _ -> "Internal"
  | NullEvent -> "NullEvent"

type icon = Canvas.canvas

type startup_data =
  {select_icon : icon;
   zoom_icon : icon;
   totop_icon : icon;
   tobottom_icon : icon}

(* Modifier keys. Events can update these. The main program just reads the
values out of the references. *)
let shift_down = ref false

let command_down = ref false

let option_down = ref false

(* The event queue *)
let events = Queue.create ()

let pt_send = ref (fun _ -> ())
let pt_poll = ref (fun _ -> Int 0)
let pt_close = ref (fun _ -> ())

let send m = !pt_send m
let poll _ = !pt_poll ()
let close _ = !pt_close ()

let bool_of_int = function
  | 0 -> false
  | _ -> true

(* Returns send, poll, close *)
let startup sdata =
  (*i Printf.printf "ML: Wxgui.startup: There are %i things in Sys.argv\n" (Array.length Sys.argv);
  flush stdout; i*)
  (*i begin match Sys.argv with
  | [|_; portnum|] ->
      Printf.printf "ML: Wxgui.startup: Calling Pytalk.pystarts_establish_connection on port %s\n" portnum;
      let send, poll, close = Pytalk.pystarts_establish_connection (int_of_string portnum) in
        flprint "ML: Done Pytalk.pystarts_establish_connection\n";
        pt_send := send; pt_poll := poll; pt_close := close
  | _ -> i*)
      let send, poll, close = Pytalk.establish_connection "python" "main.py" in
        pt_send := send; pt_poll := poll; pt_close := close;
  (*i end; i*)
    (* Send startup code with data. Main python program with then
    do startup work and begin wx main loop *)
    send
      (Tuple
        [String "Startup";
         String (Canvas.string_of_canvas sdata.select_icon);
         String (Canvas.string_of_canvas_alpha sdata.select_icon);
         String (Canvas.string_of_canvas sdata.zoom_icon);
         String (Canvas.string_of_canvas_alpha sdata.zoom_icon);
         String (Canvas.string_of_canvas sdata.totop_icon);
         String (Canvas.string_of_canvas_alpha sdata.totop_icon);
         String (Canvas.string_of_canvas sdata.tobottom_icon);
         String (Canvas.string_of_canvas_alpha sdata.tobottom_icon)])

(* The path we're running from *)
let executable_path = Sys.getcwd ()

let shutdown () =
  !pt_close ()

let events = Queue.create ()

let key_of_keycode = function
  | 27 -> Escape
  | 314 -> Left
  | 315 -> Up
  | 316 -> Right
  | 317 -> Down
  | 65 -> A | 66 -> B | 67 -> C | 68 -> D | 69 -> E | 70 -> F | 71 -> G
  | 72 -> H | 73 -> I | 74 -> J | 75 -> K | 76 -> L | 77 -> M | 78 -> N
  | 79 -> O | 80 -> P | 81 -> Q | 82 -> R | 83 -> S | 84 -> T | 85 -> U
  | 86 -> V | 87 -> W | 88 -> X | 89 -> Y | 90 -> Z
  | k -> UnknownKey k

(* The poll function called by the main program *)
let rec poll_inner () =
  match poll () with
  | Tuple [String "AppStart"] ->
      flprint "WXGUI: Application startup\n";
      AppStartup
  | Tuple [String "AppClose"] ->
      flprint "WXGUI: Closing application\n";
      AppClose
  | Tuple [String "Command"; Bool b] ->
      command_down := b;
      poll_inner ()
  | Tuple [String "Option"; Bool b] ->
      option_down := b;
      poll_inner ()
  | Tuple [String "Shift"; Bool b] ->
      shift_down := b;
      poll_inner ()
  | Tuple [String "KeyDown"; Int w; Int k] ->
      KeyDown (w, key_of_keycode k)
  | Tuple [String "KeyUp"; Int w; Int k] ->
      KeyUp (w, key_of_keycode k)
  | Tuple [String "LeftDown"; Int w ;Int x; Int y] ->
      LeftDown (w, x, y)
  | Tuple [String "LeftUp"; Int w; Int x; Int y] ->
      LeftUp (w, x, y)
  | Tuple [String "RightDown"; Int w; Int x; Int y] ->
      RightDown (w, x, y)
  | Tuple [String "RightUp"; Int w; Int x; Int y] ->
      RightUp (w, x, y)
  | Tuple [String "LeftDragging"; Int w; Int x; Int y] ->
      LeftDragging (w, x, y)
  | Tuple [String "Button"; Int b; Int w] ->
      ButtonClicked (w, b)
  | Tuple [String "CloseWindow"; Int w] ->
      WindowClosed w
  | Tuple [String "OpenFile"; String filename] ->
      OpenFile filename
  | Tuple [String "PaintRect"; Int win; Int x; Int y; Int w; Int h] ->
      PaintRect (win, x, y, w, h)
  | Tuple [String "BlurSlider"; Int win; Int v] ->
      BlurSlider (win, v)
  | Tuple [String "TransSlider"; Int win; Int v] ->
      TransSlider (win, v)
  | Tuple (String "Internal"::more) ->
      Internal (Tuple more)
  | other ->
      Printf.printf "WXGUI: Unknown object: %s\n" (debug_string_of_marshallable other);
      flush stdout;
      exit 0

(* The main poll function. If there's something in the event queue, return that.
Otherwise actually poll. We should never get an internal event here. *)
let poll () =
  if Queue.is_empty events then
    poll_inner ()
  else
    Queue.take events

let pred_name n = function
  | Internal (Tuple (String n'::_)) when n = n' -> true
  | _ -> false

(* Wait until we get an event matching a predicate, queueing any that don't
match. Return the event when we get it. *)
let rec wait_on_event pred =
  let event = poll_inner () in
    if pred event then event else
      begin
        Queue.add event events;
        wait_on_event pred
      end

(* Keep polling until we get the right response. *)
let synch_call name =
  send (Tuple [String name]);
  match wait_on_event (pred_name name) with
  | Internal (Tuple (String n::more)) when n = name -> more
  | _ -> assert false

(* Same, but there are initial arguments. *)
let synch_call_args name args =
  send (Tuple (String name::args));
  match wait_on_event (pred_name name) with
  | Internal (Tuple (String n::more)) when n = name -> more
  | _ -> assert false

(* Get up-to-date mouse coordinates. *)
let mouse_coords w =
  match synch_call_args "MouseNow" [Int w] with
  | [Int x; Int y] -> x, y
  | _ -> assert false

let get_screen_size () =
  match synch_call "ScreenSize" with
  | [Int w; Int h] -> w, h
  | _ -> assert false

let get_platform () =
  match synch_call "Platform" with
  | [String platform] ->
      begin match platform with
      | "Windows" -> Windows
      | "Mac" -> Mac
      | "Gtk" -> Gtk
      | _ -> Unknown
      end
  | _ -> assert false

let close_application () =
  send (Tuple [String "AppClose"])

type canvas = Pdfio.bytes

let mkcanvas w h =
  Pdfio.mkbytes (w * h * 3)

let canvases = Hashtbl.create 20

let new_blank_canvas window =
  Hashtbl.add canvases window (mkcanvas 1280 1024)

let canvas_exists w =
  try ignore (Hashtbl.find canvases w); true with
   _ -> false

let canvas_of_window =
  Hashtbl.find canvases

let delete_canvas c =
  Hashtbl.remove canvases c

let make_window name sx sy dx dy extentx extenty hastoolbar =
  match
    synch_call_args "MakeWindow"
    [String name; Int sx; Int sy; Int dx; Int dy; Int extentx; Int extenty; Bool hastoolbar]
  with
  | [Int i] -> new_blank_canvas i; i
  | _ -> assert false

let open_cachewindow () =
  send (Tuple [String "OpenCacheWindow"])

(* Delete a window which has been closed from WxPython *)
let delete_window =
  delete_canvas

let close_window w =
  delete_canvas w;
  match synch_call_args "CloseWindow" [Int w] with
  | [] -> ()
  | _ -> assert false

let get_window_width w =
  match synch_call_args "WindowWidth" [Int w] with
  | [Int i] -> i
  | _ -> assert false

let get_window_height w =
  match synch_call_args "WindowHeight" [Int w] with
  | [Int i] -> i
  | _ -> assert false

let get_window_size w =
  get_window_width w, get_window_height w

let get_window_scroll w =
  match synch_call_args "WindowScroll" [Int w] with
  | [Int dx; Int dy] -> dx, dy
  | _ -> assert false

let set_status_bar (window, str) =
  send (Tuple [String "SetStatusBar"; Int window; String str])

let nullwindow = ~-1

let capture_mouse window =
  send (Tuple [String "CaptureMouse"; Int window])

let release_mouse window =
  send (Tuple [String "ReleaseMouse"; Int window])

let set_blurslider window value =
  send (Tuple [String "SetBlurSlider"; Int window; Int value])

let set_transslider window value =
  send (Tuple [String "SetTransSlider"; Int window; Int value])

let set_cachetext text =
  send (Tuple [String "SetCacheText"; String text])

(* Get part of a canvas as a string. Assume 1280 wide for now. *)
let string_of_canvas_portion canvas xmin xmax ymin ymax =
  assert (xmax > xmin && ymax > ymin);
  assert (xmin >= 0 && ymin >= 0 && xmax < 1280 && ymax < 1024);
  let size = (xmax - xmin + 1) * (ymax - ymin + 1) * 3 in
    let s = String.create size in
      let spos = ref 0
      and apos = ref (3 * 1280 * ymin + (xmin * 3)) in
        for row = ymin to ymax do
          for column = xmin to xmax do
            s.[!spos] <- char_of_int (bget canvas (!apos + (column - xmin) * 3));
            s.[!spos + 1] <- char_of_int (bget canvas (!apos + (column - xmin) * 3 + 1));
            s.[!spos + 2] <- char_of_int (bget canvas (!apos + (column - xmin) * 3 + 2));
            spos += 3;
          done;
          apos += (1280 * 3)
        done;
        s

(* Refresh the window. We allow zero-width or zero-height rectangles here - they
just do nothing. *)
let refresh_window window (xmin, ymin, xmax, ymax) =
  (*i Printf.printf "refresh_window xmin, max: %i, %i, ymin, max %i, %i" xmin xmax ymin ymax;
  flprint "\n"; i*)
  if ymin <> ymax && xmin <> xmax && canvas_exists window then
    let canvas = canvas_of_window window in
      let data = string_of_canvas_portion canvas xmin xmax ymin ymax in
        match
          synch_call_args "RefreshWindow"
            [Int window;
             Int xmin; Int ymin; Int (xmax - xmin + 1); Int (ymax - ymin + 1);
             String data]
        with
        | [] -> ()
        | _ -> assert false

(* Set these to offset [plot_sprite]. *)
let dx = ref 0
let dy = ref 0

(* Write a pixel of colour [col] at [(x, y)] to a wx canvas [c] with width
[w].*)
let writepixel x y col (c, w) =
  let o = y * w * 3 + x * 3 in
    bset c o (Colour.red_of_colour col);
    bset c (o + 1) (Colour.green_of_colour col);
    bset c (o + 2) (Colour.blue_of_colour col)

(* Plot a \emph{Sprite.subspan} at [(x, y)]. *)
let rec plot_subspan x y buf = function
  | (l, Fill.Run col) ->
      for xpos = x to x + l - 1 do
        writepixel (xpos + !dx) (y + !dy) col buf
      done
  | (l, Fill.Samples cols) ->
      for xpos = x to x + l - 1 do
        writepixel (xpos + !dx) (y + !dy) cols.(xpos - x) buf
      done
  | (l, Fill.Interval(o, cols)) ->
      for xpos = x to x + l - 1 do
        writepixel (xpos + !dx) (y + !dy) cols.(xpos - x + o - 1) buf
      done

(* Plot a span. *)
let rec plot_span x y buf = function
  | [] -> ()
  | ((l, _) as h)::rest ->
      plot_subspan x y buf h; plot_span (x + l) y buf rest

(* Plot a span line. *)
let plot_spanline y (c, w) =
  iter (fun (s, _, subspans) -> plot_span s y (c, w) subspans)

(* Plot a vspan. *)
let rec plot_vspan buf (s, l, spanlines) =
  match l with
  | 0 -> ()
  | _ -> 
     plot_spanline s buf (hd spanlines);
     plot_vspan buf (s + 1, l - 1, tl spanlines)

(* Plot a sprite. No clipping required, since the renderer has already done it.
If the window is unexpectedly made smaller, Graphics will silently clip anyway.
*)
let plot_sprite w dx' dy' = function
  | Sprite.Sprite (Sprite.Box (minx, miny, maxx, maxy), vspans) ->
      (*i Printf.printf "plot sprite, window %i\n" w; i*)
      if canvas_exists w then
        begin
          dx := dx'; dy := dy'; iter (plot_vspan (canvas_of_window w, 1280)) vspans
        end
  | _ -> ()

(* Plot a shape in a given colour. *)
let plot_shape w dx dy col shp =
  plot_sprite w dx dy (Sprite.fillshape shp (Fill.plain col))

type tool = Select | Zoom

