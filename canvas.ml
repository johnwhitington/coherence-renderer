(* \chaptertitle{Canvas}{Rectangular matrices of colours} *)
open Pdfutil
open Pdfio

(* \intf A canvas is an array of arrays of colours, row-major. Canvases have at least
one row and at least one column. Canvases have coordinates $1..\textit{width}$ and
$1..\textit{height}$ *)
type canvas = Colour.colour array array

(* \intf Return a canvas width or height *)
let canvas_width c = Array.length c.(0)
and canvas_height c = Array.length c

(* \intf Return both dimensions at once *)
let canvas_dimensions c =
  canvas_width c, canvas_height c

(* \intf Make a new canvas. *)
let newcanvas x y =
  assert (x > 0 && y > 0); 
  Array.make_matrix y x Colour.clear

(* \intf Make a new, clear canvas. If we ever change to bigarrays, we'll need to
clear it manually, but for these native OCaml arrays, it's initialized already.
*)
let newcanvasclear = newcanvas

(* \intf Make a new canvas the same as [c]. *)
let copycanvas c =
  let w, h  = canvas_dimensions c in
    let c' = newcanvas w h in
      for yp = 1 to h do
        for xp = 1 to w do
          c'.(yp - 1).(xp - 1) <- c.(yp - 1).(xp - 1)
        done
      done;
      c'

(* \intf Subcanvas copying. Copy from [src] at position [(x, y)] a block of
width [w] and height [h] to [dest]. *)
let subcopy src dest x y w h =
  if not 
    (x > 0 && y > 0 &&
    (x + w - 1) <= canvas_width src &&
    (y + h - 1) <= canvas_height src &&
    w <= canvas_width dest &&
    h <= canvas_height dest &&
    w > 0 && h > 0) then
      begin
        Printf.printf "x, y, w, h = %i, %i, %i, %i\n" x y w h;
      raise (Failure "subcopy")
      end
  else
  for yd = y to y + h - 1 do
    for xd = x to x + w - 1 do
      dest.(yd - y).(xd - x) <- src.(yd - 1).(xd - 1)
    done
  done

(* \intf Make a string of a canvas, RGBRGB (ignores alpha) *)
let string_of_canvas c =
  let w = canvas_width c
  and h = canvas_height c in
    let s = String.create (w * h * 3)
    and spos = ref 0 in
      for yd = 0 to h - 1 do
        for xd = 0 to w - 1 do
          let r, g, b = Colour.unpremul_rgb c.(yd).(xd) in
            s.[!spos] <- char_of_int r;
            s.[!spos + 1] <- char_of_int g;
            s.[!spos + 2] <- char_of_int b;
            spos := !spos + 3
        done
      done;
      Bytes.to_string s

let string_of_canvas_alpha c =
  let w = canvas_width c
  and h = canvas_height c in
    let s = String.create (w * h)
    and spos = ref 0 in
      for yd = 0 to h - 1 do
        for xd = 0 to w - 1 do
          s.[!spos] <-
            char_of_int (Colour.alpha_of_colour c.(yd).(xd));
          incr spos
        done
      done;
      Bytes.to_string s

let bytes_of_canvas c =
  let w = canvas_width c
  and h = canvas_height c in
    let s = mkbytes (w * h * 3)
    and spos = ref 0 in
      for yd = 0 to h - 1 do
        for xd = 0 to w - 1 do
          let r, g, b = Colour.unpremul_rgb c.(yd).(xd) in
            bset s !spos r;
            bset s (!spos + 1) g;
            bset s (!spos + 2) b;
            spos := !spos + 3
        done
      done;
      s
