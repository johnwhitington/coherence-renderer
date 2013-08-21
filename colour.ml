(* \part{Colour Models and Representations} *)

(* \chaptertitle{Colour}{Representation of colours} *)

(* \epigraph{The price one pays for pursuing any profession or calling is an
intimate knowledge of its ugly side.}{\textit{James Baldwin, 1924--1987}} *)

(*This module defines colours (pixel values) and operations on them, taken from
\cite{porterduff,smithcomposit}. We currently represent colours in the 31 bits
available in an OCaml integer. On 64-bit systems, the size is 63 bits, and the
remainder are unused. Values have premultiplied alpha. *)

(* \intf Type for colours. *)
type colour = int 

(* \intf Compositing operators combine two colours to form another. *)
type compositing_operator = colour -> colour -> colour

(* \intf For debugging purposes, we create a compositing function which always
fails. The exception definition\ldots *)
exception Nocover

(* \intf \ldots and associated compositing function. *)
let nocover _ _ = raise Nocover

(* \section{Forming and converting between representations.} *)

(*
Code based on that by Jean-Baptiste Rouquier, with permission.

The volume of the continuous 4D pyramid $(x,y,z,t) | x,y,z \leq t, 0 \leq
x,y,z,t \leq 1$ is $1/4$.  Unfortunately, we are in a discrete space, and the
boundaries are part of the volume.  Faces, edges and vertices cause the total
volume of the discrete pyramid $(r,g,b,a) | r,g,b\leq a, 0\leq r,g,b,a \leq
255$ to be slightly more than $1/4 256^4$.

Assume for now that we just need to encode $(r,g,b,a) | r,g,b < a, 0 \leq
r,g,b,a \leq 255$, that is, $a$ is always the unique maximum of $r,g,b,a$.
Swapping a with another coordinate is an injective function that maps our
pyramid on another one.  Since there are $4$ possible positions for $a$, this
allows us to encode $2$ bits.

The idea is to remove the least significant bit (LSB) from $r,g,b,a$ and
concatenate the 28 remaining bits.  We also concatenate two of the LSB. The two
remaining LSB are encoded with the previous trick (position of $a$).  This uses
30 bits.

As seen before, if we ignore the boundaries of the pyramid, the volume is not
more than $1/4$ the whole hypercube, that's why $30$ bits are enough.

Now, remove the assumption. If a is not the only maximum after cutting the LSB,
we use another scheme.  The previous scheme used only $30$ bits, the last one is
used to flag that we use another scheme.  Since we have either $r=a$ or $g=a$ or
$b=a$, we just need to remember one equality that holds, and encode three of the
four channels.

In this scheme, we concatenate
\begin{enumerate}
\item the flag [mask_equality] to say that we're using this scheme
\item the four LSB
\item three flags to say which equality holds (two flags would be enough)
\item two padding bits
\item three channels
\end{enumerate}*)

let mask_equality  = 0b1000000000000000000000000000000
let mask_r_lsb = 0b0100000000000000000000000000000
let mask_g_lsb = 0b0010000000000000000000000000000
let mask_channel3 = 0b0001111111000000000000000000000
let mask_channel2 = 0b0000000000111111100000000000000
let mask_channel1 = 0b0000000000000000011111110000000
let mask_channel0 = 0b0000000000000000000000001111111

(* Used only in the equality scheme *)
let mask_b_lsb = 0b0001000000000000000000000000000
let mask_a_lsb = 0b0000100000000000000000000000000
let mask_r_eq_a = 0b0000010000000000000000000000000
let mask_g_eq_a = 0b0000001000000000000000000000000
let mask_b_eq_a = 0b0000000100000000000000000000000

(** Concatenate four 7-bits integers *)
let concat r g b a =
  (r lsl 21) lor (g lsl 14) lor (b lsl 7) lor a

(** return the index of the maximum int among four. *)
let index_max4 (a : int) b c d =
  if a > b then
    if c > d then
      if a > c then 0 else 2
    else
      if a > d then 0 else 3
  else
    if c > d then
      if b > c then 1 else 2
    else
      if b > d then 1 else 3

(* Build a colour from components *)
let colour_of_rgba r g b a =
  let r = r lsr 1
  and g = g lsr 1
  and b = b lsr 1
  and a = a lsr 1
  and r_lsb = r land 1 <> 0
  and g_lsb = g land 1 <> 0
  and b_lsb = b land 1 <> 0
  and a_lsb = a land 1 <> 0 in
    if r <> a && g <> a && b <> a then
      (if r_lsb then mask_r_lsb else 0) lor
      (if g_lsb then mask_g_lsb else 0) lor
      (if b_lsb then
         if a_lsb
           then concat r g b a
           else concat r g a b
       else
         if a_lsb
           then concat r a b g
           else concat a g b r)
    else
      mask_equality lor
      (if r_lsb then mask_r_lsb else 0) lor
      (if g_lsb then mask_g_lsb else 0) lor
      (if b_lsb then mask_b_lsb else 0) lor
      (if a_lsb then mask_a_lsb else 0) lor
      (if r = a then mask_r_eq_a else 0) lor
      (if g = a then mask_g_eq_a else 0) lor
      (if b = a then mask_b_eq_a else 0) lor
      (if r = a
         then concat 0 g b a
         else if g = a
           then concat 0 r b a
           else (assert (b = a); concat 0 r g a))

let unsplit i lsb =
  i lsl 1 lor (if lsb then 1 else 0)

(* Extract components from a colour *)
let rgba_of_colour c =
  let r = ref 0 in
  let g = ref 0 in
  let b = ref 0 in
  let a = ref 0 in
  let r_lsb = ref (mask_r_lsb land c <> 0) in
  let g_lsb = ref (mask_g_lsb land c <> 0) in
  let b_lsb = ref false in
  let a_lsb = ref false in
    if c land mask_equality = 0 then
      let channel3 = (c land mask_channel3) lsr 21 in
      let channel2 = (c land mask_channel2) lsr 14 in
      let channel1 = (c land mask_channel1) lsr 7 in
      let channel0 = (c land mask_channel0) in
        (match index_max4 channel3 channel2 channel1 channel0 with
         | 3 -> b_lsb := true; a_lsb := true; r := channel3; g := channel2; b := channel1; a := channel0
         | 2 -> b_lsb := true; a_lsb := false; r := channel3; g := channel2; a := channel1; b := channel0
         | 1 -> b_lsb := false; a_lsb := true; r := channel3; a := channel2; b := channel1; g := channel0
         | n ->
            assert (n = 0);
            b_lsb := false; a_lsb := false; a := channel3; g := channel2; b := channel1; r := channel0)
    else
      (b_lsb := mask_b_lsb land c <> 0;
       a_lsb := mask_a_lsb land c <> 0;
       let channel2 = (c land mask_channel2) lsr 14 in
       let channel1 = (c land mask_channel1) lsr 7 in
       let channel0 = (c land mask_channel0) in
         a := channel0;
         if c land mask_r_eq_a <> 0  then (r := !a; g := channel2; b := channel1)
         else if c land mask_g_eq_a <> 0  then (g := !a; r := channel2; b := channel1)
         else (assert (c land mask_b_eq_a <> 0); (b := !a; r := channel2; g := channel1)));
    unsplit !r !r_lsb,
    unsplit !g !g_lsb,
    unsplit !b !b_lsb,
    unsplit !a !a_lsb

let red_of_colour c =
  let r_lsb = (mask_r_lsb land c <> 0)
  and r =
    if c land mask_equality = 0 then
      let channel3 = (c land mask_channel3) lsr 21 in
      let channel2 = (c land mask_channel2) lsr 14 in
      let channel1 = (c land mask_channel1) lsr 7 in
      let channel0 = (c land mask_channel0) in
        (match index_max4 channel3 channel2 channel1 channel0 with
         | 3 | 2 | 1-> channel3
         | n -> assert (n = 0); channel0)
    else
      (let channel2 = (c land mask_channel2) lsr 14 in
       let channel0 = (c land mask_channel0) in
         if c land mask_r_eq_a <> 0 then channel0 else channel2)
  in
    unsplit r r_lsb

let green_of_colour c =
  let g_lsb = mask_g_lsb land c <> 0 in
    let g =
      if c land mask_equality = 0 then
        let channel3 = (c land mask_channel3) lsr 21 in
        let channel2 = (c land mask_channel2) lsr 14 in
        let channel1 = (c land mask_channel1) lsr 7 in
        let channel0 = (c land mask_channel0) in
          (match index_max4 channel3 channel2 channel1 channel0 with
           | 1 -> channel0
           | _ -> channel2)
      else
        (let channel2 = (c land mask_channel2) lsr 14 in
         let channel1 = (c land mask_channel1) lsr 7 in
         let channel0 = (c land mask_channel0) in
           if c land mask_r_eq_a <> 0 then channel2
           else if c land mask_g_eq_a <> 0 then channel0
           else (assert (c land mask_b_eq_a <> 0); channel1))
    in
      unsplit g g_lsb

let blue_of_colour c =
  if c land mask_equality = 0 then
    let channel3 = (c land mask_channel3) lsr 21 in
    let channel2 = (c land mask_channel2) lsr 14 in
    let channel1 = (c land mask_channel1) lsr 7 in
    let channel0 = (c land mask_channel0) in
      (match index_max4 channel3 channel2 channel1 channel0 with
       | 3 -> unsplit channel1 true
       | 2 -> unsplit channel0 true
       | 1 -> unsplit channel1 false
       | n -> assert (n = 0); unsplit channel1 false)
  else
    (let b_lsb = mask_b_lsb land c <> 0 in
     let channel1 = (c land mask_channel1) lsr 7 in
     let channel0 = (c land mask_channel0) in
       if c land mask_r_eq_a <> 0  then unsplit channel1 b_lsb
       else if c land mask_g_eq_a <> 0  then unsplit channel1 b_lsb
       else (assert (c land mask_b_eq_a <> 0); unsplit channel0 b_lsb))

let alpha_of_colour c =
  if c land mask_equality = 0 then
    let channel3 = (c land mask_channel3) lsr 21 in
    let channel2 = (c land mask_channel2) lsr 14 in
    let channel1 = (c land mask_channel1) lsr 7 in
    let channel0 = (c land mask_channel0) in
      (match index_max4 channel3 channel2 channel1 channel0 with
       | 3 -> unsplit channel0 true
       | 2 -> unsplit channel1 false
       | 1 -> unsplit channel2 true
       | n -> assert (n = 0); unsplit channel3 false)
  else
    unsplit (c land mask_channel0) (mask_a_lsb land c <> 0)

(* \intf Floating point version. *)
let colour_of_rgba_float r g b a =
  assert (r >= 0. && g >= 0. && b >= 0. && a >= 0.);
  assert (r <= 1. && g <= 1. && b <= 1. && a <= 1.);
  let conv x = int_of_float (x *. 255.) in
    colour_of_rgba (conv r) (conv g) (conv b) (conv a)

(* \intf The same for when we have a tuple (it is important to minimise tuple
creations and destructions in code calling these routines, since they form the
core of the compositing code). *)
let colour_of_rgba_tuple (r, g, b, a) = colour_of_rgba r g b a

(* \intf Make a colour up from a single channel value. *)
let colour_of_channel a = colour_of_rgba a a a a

(* \intf The clear colour. (Here we see an attendant advantage of premultiplied
alpha: there is only one value for clear. *)
let clear = colour_of_rgba 0 0 0 0

(* \intf Remove all but one colour channel from a colour, retaining the alpha. *)
let red_channel c =
  colour_of_rgba (red_of_colour c) 0 0 (alpha_of_colour c)

let green_channel c =
  colour_of_rgba 0 (green_of_colour c) 0 (alpha_of_colour c)

let blue_channel c =
  colour_of_rgba 0 0 (blue_of_colour c) (alpha_of_colour c)

(* \intf Average r, g, b and replace them with that coverage.  Alpha unaltered.
*)
let monochrome c =
  let r, g, b, a = rgba_of_colour c in
    let av = (r + g + b) / 3 in
      colour_of_rgba av av av a

(* \section{Compositing functions} *)

(* Divide a number between 0 and 65534 inclusive by 255. \SPEED{Is this
actually faster than a divide instruction when compiled with caml in native
code?} *)
let div255 i = (i + (i lsr 8) + 1) lsr 8

(* \intf Porter/Duff dissolve operator. Dissolve [col] by [delta]. We use a
label for the delta to make calls easier to read. *)
let dissolve col ~delta =
  assert (delta >=0 && delta <= 255);
  if delta = 0 then clear
  else if delta = 255 then col else
    let delta = if delta > 255 || delta < 0 then 255 else delta in
      let r = red_of_colour col
      and g = green_of_colour col
      and b = blue_of_colour col
      and a = alpha_of_colour col in
        let r' = div255 (r * delta)
        and g' = div255 (g * delta)
        and b' = div255 (b * delta)
        and a' = div255 (a * delta) in
            colour_of_rgba r' g' b' a'

(* The following implementation of the Porter/Duff `over' operator is courtesy
of Alvy Ray Smith at Microsoft. *)

(* Linear interpolation from [p] to [q] by [a]. All on $0..255$. *)
let prelerp p q a =
  p + q - let t = a * p + 128 in ((t lsr 8) + t) lsr 8
  
(* \intf The compositing function itself. *)
let over a b =
  let aa = alpha_of_colour a in
    if aa = 0 then b else if aa = 255 then a else
    let ra = red_of_colour a
    and ga = green_of_colour a
    and ba = blue_of_colour a in
      let ab = alpha_of_colour b
      and rb = red_of_colour b
      and gb = green_of_colour b
      and bb = blue_of_colour b in
        let r' = prelerp rb ra aa
        and g' = prelerp gb ga aa
        and b' = prelerp bb ba aa
        and a' = prelerp ab aa aa in
          colour_of_rgba r' g' b' a'

(* \intf Over for just the alpha channel --- used, for instance, in stamping
brushstrokes. *)
let alpha_over a b =
  let aa = alpha_of_colour a in
    if aa = 0 then b else if aa = 255 then a else
      let ab = alpha_of_colour b in
        colour_of_rgba 0 0 0 (prelerp ab aa aa)

(* \intf Porter/Duff `plus` operator. *)
let pd_plus a b =
  let ar = red_of_colour a
  and ag = green_of_colour a
  and ab = blue_of_colour a
  and aa = alpha_of_colour a
  and br = red_of_colour b
  and bg = green_of_colour b
  and bb = blue_of_colour b
  and ba = alpha_of_colour b in
    assert (ar + br <= 255);
    assert (ag + bg <= 255);
    assert (ab + bb <= 255);
    assert (aa + ba <= 255);
    colour_of_rgba (ar + br) (ag + bg) (ab + bb) (aa + ba)

(* \intf Dissolve between [a] and [b] by [alpha] *)
let dissolve_between ~a b ~alpha =
  assert (alpha >= 0 && alpha <= 255);
  if alpha = 0 then b
  else if alpha = 255 then a else
    let a' = dissolve a alpha
    and b' = dissolve b (255 - alpha) in
      pd_plus a' b'

(* \intf Predicate to determine if a colour is opaque *)
let opaque col =
  alpha_of_colour col = 255

let transparent col =
  alpha_of_colour col = 0

(* Multiplication. If [a, b] on 0\ldots 255, returns [a] multiplied by
[b] considered to be on 0\ldots 1. *)
let int_mult a b =
  if b = 255 then a else
    let t = a * b in
      ((t lsl 8) + t) lsr 16

(* \intf Unpremultiply a colour, returning the components. *)
let unpremul_rgb c =
  let r, g, b, a = rgba_of_colour c in
    if a = 0 then 0, 0, 0 else
    if a = 255 then r, g, b else
      let r = (r * 255) / a
      and g = (g * 255) / a
      and b = (b * 255) / a in
        r, g, b

let unpremul_components c =
  let r, g, b, a = rgba_of_colour c in
    if a = 0 then 0, 0, 0, 0 else
    if a = 255 then r, g, b, 255 else
      let r = (r * 255) / a
      and g = (g * 255) / a
      and b = (b * 255) / a in
        r, g, b, a

(* \intf Premultiply a colour held in unpremultiplied form. *)
let premul r g b a =
  int_mult r a, int_mult g a, int_mult b a, a

(* \section{Standard colours} *)

(* Make an opaque colour. *)
let mkcol r g b =
  colour_of_rgba_tuple (premul r g b 255)

let white = mkcol 255 255 255

(* These all appear in the interface. *)
let aliceblue = mkcol 240 248 255
let antiquewhite = mkcol 250 235 215 
let aqua = mkcol 0 255 255 
let aquamarine = mkcol 127 255 212 
let azure = mkcol 240 255 255 
let beige = mkcol 245 245 220 
let bisque = mkcol 255 228 196 
let black = mkcol 0 0 0 
let blanchedalmond = mkcol 255 235 205 
let blue = mkcol 0 0 255 
let blueviolet = mkcol 138 43 226 
let brown = mkcol 165 42 42 
let burlywood = mkcol 222 184 135 
let cadetblue = mkcol 95 158 160 
let chartreuse = mkcol 127 255 0 
let chocolate = mkcol 210 105 30 
let coral = mkcol 255 127 80 
let cornflowerblue = mkcol 100 149 237 
let cornsilk = mkcol 255 248 220 
let crimson = mkcol 220 20 60 
let cyan = mkcol 0 255 255 
let darkblue = mkcol 0 0 139 
let darkcyan = mkcol 0 139 139 
let darkgoldenrod = mkcol 184 134 11 
let darkgray = mkcol 169 169 169 
let darkgreen = mkcol 0 100 0 
let darkgrey = mkcol 169 169 169 
let darkkhaki = mkcol 189 183 107 
let darkmagenta = mkcol 139 0 139 
let darkolivegreen = mkcol 85 107 47 
let darkorange = mkcol 255 140 0 
let darkorchid = mkcol 153 50 204 
let darkred = mkcol 139 0 0 
let darksalmon = mkcol 233 150 122 
let darkseagreen = mkcol 143 188 143 
let darkslateblue = mkcol 72 61 139 
let darkslategray = mkcol 47 79 79 
let darkslategrey = mkcol 47 79 79 
let darkturquoise = mkcol 0 206 209 
let darkviolet = mkcol 148 0 211 
let deeppink = mkcol 255 20 147 
let deepskyblue = mkcol 0 191 255 
let dimgray = mkcol 105 105 105 
let dimgrey = mkcol 105 105 105 
let dodgerblue = mkcol 30 144 255 
let firebrick = mkcol 178 34 34 
let floralwhite = mkcol 255 250 240 
let forestgreen = mkcol 34 139 34 
let fuchsia = mkcol 255 0 255 
let gainsboro = mkcol 220 220 220 
let ghostwhite = mkcol 248 248 255 
let gold = mkcol 255 215 0 
let goldenrod = mkcol 218 165 32 
let gray = mkcol 128 128 128 
let grey = mkcol 128 128 128 
let green = mkcol 0 128 0 
let greenyellow = mkcol 173 255 47 
let honeydew = mkcol 240 255 240 
let hotpink = mkcol 255 105 180 
let indianred = mkcol 205 92 92 
let indigo = mkcol 75 0 130 
let ivory = mkcol 255 255 240 
let khaki = mkcol 240 230 140 
let lavender = mkcol 230 230 250 
let lavenderblush = mkcol 255 240 245 
let lawngreen = mkcol 124 252 0 
let lemonchiffon = mkcol 255 250 205 
let lightblue = mkcol 173 216 230 
let lightcoral = mkcol 240 128 128 
let lightcyan = mkcol 224 255 255 
let lightgoldenrodyellow = mkcol 250 250 210 
let lightgray = mkcol 211 211 211 
let lightgreen = mkcol 144 238 144 
let lightgrey = mkcol 211 211 211 
let lightpink = mkcol 255 182 193 
let lightsalmon = mkcol 255 160 122 
let lightseagreen = mkcol 32 178 170 
let lightskyblue = mkcol 135 206 250 
let lightslategray = mkcol 119 136 153 
let lightslategrey = mkcol 119 136 153 
let lightsteelblue = mkcol 176 196 222 
let lightyellow = mkcol 255 255 224 
let lime = mkcol 0 255 0 
let limegreen = mkcol 50 205 50 
let linen = mkcol 250 240 230 
let magenta = mkcol 255 0 255 
let maroon = mkcol 128 0 0 
let mediumaquamarine = mkcol 102 205 170 
let mediumblue = mkcol 0 0 205 
let mediumorchid = mkcol 186 85 211 
let mediumpurple = mkcol 147 112 219 
let mediumseagreen = mkcol 60 179 113 
let mediumslateblue = mkcol 123 104 238 
let mediumspringgreen = mkcol 0 250 154 
let mediumturquoise = mkcol 72 209 204 
let mediumvioletred = mkcol 199 21 133 
let midnightblue = mkcol 25 25 112 
let mintcream = mkcol 245 255 250 
let mistyrose = mkcol 255 228 225 
let moccasin = mkcol 255 228 181 
let navajowhite = mkcol 255 222 173 
let navy = mkcol 0 0 128 
let oldlace = mkcol 253 245 230 
let olive = mkcol 128 128 0 
let olivedrab = mkcol 107 142 35 
let orange = mkcol 255 165 0 
let orangered = mkcol 255 69 0 
let orchid = mkcol 218 112 214 
let palegoldenrod = mkcol 238 232 170 
let palegreen = mkcol 152 251 152 
let paleturquoise = mkcol 175 238 238 
let palevioletred = mkcol 219 112 147 
let papayawhip = mkcol 255 239 213 
let peachpuff = mkcol 255 218 185 
let peru = mkcol 205 133 63 
let pink = mkcol 255 192 203 
let plum = mkcol 221 160 221 
let powderblue = mkcol 176 224 230 
let purple = mkcol 128 0 128 
let red = mkcol 255 0 0 
let rosybrown = mkcol 188 143 143 
let royalblue = mkcol 65 105 225 
let saddlebrown = mkcol 139 69 19 
let salmon = mkcol 250 128 114 
let sandybrown = mkcol 244 164 96 
let seagreen = mkcol 46 139 87 
let seashell = mkcol 255 245 238 
let sienna = mkcol 160 82 45 
let silver = mkcol 192 192 192 
let skyblue = mkcol 135 206 235 
let slateblue = mkcol 106 90 205 
let slategray = mkcol 112 128 144 
let slategrey = mkcol 112 128 144 
let snow = mkcol 255 250 250 
let springgreen = mkcol 0 255 127 
let steelblue = mkcol 70 130 180 
let tan = mkcol 210 180 140 
let teal = mkcol 0 128 128 
let thistle = mkcol 216 191 216 
let tomato = mkcol 255 99 71 
let turquoise = mkcol 64 224 208 
let violet = mkcol 238 130 238 
let wheat = mkcol 245 222 179 
let whitesmoke = mkcol 245 245 245 
let yellow = mkcol 255 255 0 
let yellowgreen = mkcol 154 205 50

