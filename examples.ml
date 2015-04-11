(* Demo Examples *)
open Pdfutil
open Render

let path = ref ""

let circlezero =
  Path (Shapes.circle 0. 0. 100.)

let circleone =
  Path (Shapes.circle 100. 100. 100.)

let circletwo =
  Path (Shapes.circle 200. 100. 90.)

let object_of_geometry g =
  Obj (Id.new_ids (), g, Pdftransform.i, Over)

let object_of_geometry_compop c g =
  Obj (Id.new_ids (), g, Pdftransform.i, c)

(* Extract the first path in a graphic. *)
let rec path_of_graphic ({Pdfgraphics.elements = elements} as graphic) =
  match elements with
  | [] -> raise (Failure "no path in graphic")
  | Pdfgraphics.Path (p, _)::_ -> p
  | Pdfgraphics.MCSection (_, g)::_
  | Pdfgraphics.MCSectionProperties (_, _, g)::_ ->
      path_of_graphic {graphic with Pdfgraphics.elements = g}
  | _::t -> path_of_graphic {graphic with Pdfgraphics.elements = t}

(* Load the paths from a PDF file at path/file *)
let table = null_hash () 

let pdf_graphic_from_file file =
  try Hashtbl.find table file with
  | Not_found ->
      let pdf = Pdfread.pdf_of_file None None file in
        match Pdfpage.pages_of_pagetree pdf with
        | [] -> failwith ("No pages in PDF file")
        | page::_ ->
            flprint ("new pdf_graphic_from_file: " ^ file ^ "\n");
            let r = Pdfgraphics.graphic_of_page pdf page, pdf, page.Pdfpage.resources in
              Hashtbl.add table file r;
              r

let pdf_pathsinfile file =
  let graphic, _, _ = pdf_graphic_from_file (!path ^ "/" ^ file) in
    path_of_graphic graphic
 
(* Common objects *)
let redblob =
  object_of_geometry_compop (PreTrans (1.0, Over)) (Basic (Fill.plain Colour.red, circleone))

let blueblob =
  object_of_geometry (Basic (Fill.plain Colour.blue, circleone))

(* Objects for Filters demo *)
let cpg_example =
  Obj
    (Id.new_ids (),
     Basic (
       Fill.gradient
         (60., 100.) (220., 150.) true true
         (Colour.dissolve ~delta:128 Colour.cornflowerblue) Colour.yellow,
       CPG (ExclusiveOr, circleone, circletwo)),
     Pdftransform.i, Over)

let affinefilter =
  object_of_geometry
    (Filters.affine
      ([Pdftransform.Scale ((200., 250.), 1., ~-.0.5); Pdftransform.ShearX ((200., 250.), ~-.0.3)])
      (Basic (Fill.gradient (200., 250.) (200., 270.) true true Colour.white (Colour.dissolve ~delta:0 Colour.white), rectangle 200.  200. 600. 100.)))

let blurfilter =
  object_of_geometry
    (Filters.blur
       (Basic (Fill.plain Colour.white, (Path (Shapes.circle 100. 150. 40.))))
       (Convolve.mkgaussian 5))

let rgbfilter =
  object_of_geometry
    (Filters.rgb
     (Basic (Fill.plain Colour.white, Path (Shapes.circle 0. 0. 40.)))
     ((fun c -> [Pdftransform.Translate (10.,10.)]),
      (fun c -> [Pdftransform.Translate (~-.10.,0.)]),
      (fun c -> [Pdftransform.Translate (0.,0.)]))
     Render.FilterPlain)


(* Five objects *)
let brush () =
  let pth = pdf_pathsinfile "brushcurve.pdf" in
    (object_of_geometry
       (Basic
         (Fill.plain (Colour.dissolve ~delta:185 Colour.darkorange),
         Brushstroke (Brush.mkround 15. 0.5, pth))))

(* CPG of brush and circle, in purple. *)
let brushcircle () =
  let brush_basic =
    match brush () with
    | Obj (_, Basic (_, Brushstroke b), _, _) ->
       begin match Brush.bounds_brushstroke b with
       | minx, maxx, miny, maxy ->
            Brushstroke (Brush.transform_brushstroke [Pdftransform.Translate (~-.115., ~-.690.)] b)
       end
    | _ -> assert false
  in
    object_of_geometry
      (Basic (Fill.plain Colour.purple, CPG (ExclusiveOr, brush_basic, circlezero)))

let brushblue () =
  let pth = pdf_pathsinfile "brushcurve.pdf" in
    (object_of_geometry
       (Basic
         (Fill.plain (Colour.dissolve ~delta:185 Colour.blue),
         Brushstroke (Brush.mkround 15. 0.5, pth))))

let minusfilter = 
  object_of_geometry
     (Filters.minus (Basic (Fill.plain Colour.white, rectangle 200. 200. 100.  100.)))

let monofilter =
  object_of_geometry
    (Filters.monochrome
       (Basic (Fill.plain Colour.white, (Path (Shapes.circle 100. 150. 40.)))))

let q_shape () =
  mkpoly
    (pdf_pathsinfile "q.pdf")
    (Fill.plain Colour.darkgreen)
    Pdftransform.i
    Over

let q_shape_2 () =
  mkpoly
    (pdf_pathsinfile "q.pdf")
    (Fill.plain (Colour.dissolve Colour.cornflowerblue 200))
    Pdftransform.i
    Over

let logo () =
  mkpoly
    (pdf_pathsinfile "logo.pdf")
    (Fill.plain (Colour.dissolve Colour.darkred 255))
    Pdftransform.i
    Over

let swaptoptwo objs' =
   position_anchor Centre (200., 200.)
   (object_of_geometry
     (Filters.swapdepth
        (idset_in (hd objs')) (idset_in (hd (tl (objs'))))
        (Basic (Fill.plain Colour.white, rectangle 100. 100. 200.
        200.))))

let load_text name =
  let graphic, pdf, resources = pdf_graphic_from_file (!path ^ "/" ^ name) in
    let objs = Render.scene_of_graphic pdf graphic in
      if length objs = 0 then failwith "renderobjects_of_graphic produced no content" else
        Obj (Id.new_ids (), Group (rev objs), Pdftransform.i, Over)

let aatext () = load_text "aatext.pdf"
let mintext1 () = load_text "mintext1.pdf"
let mintext2 () = load_text "mintext2.pdf"
let filtertext1 () = load_text "filtertext1.pdf"
let filtertext2 () = load_text "filtertext2.pdf"
let lionfilter1 () = load_text "lionfilter1.pdf"
let lionfilter2 () = load_text "lionfilter2.pdf"
let examplei () = load_text "i.pdf"

let smalllion () =
  let graphic, pdf, resources = pdf_graphic_from_file (!path ^ "/" ^ "lion.pdf") in
    (*flprint "\n";
    flprint (Pdfgraphics.string_of_graphic graphic);
    flprint "\n";*)
    let objs = Render.scene_of_graphic pdf graphic in
      Obj (Id.new_ids (), Group (rev objs), Pdftransform.i, Over)

let radial_filled =
  let fill =
    Fill.radial
      (60., 100.) (60., 100.) (200., 150.) true true
      Colour.lightsteelblue Colour.darkslateblue
  in
    position_anchor Centre (500., 200.)
      (Obj (Id.new_ids (), Basic (fill, circleone), Pdftransform.i, Over))

let wirecircle =
  position_anchor Centre (250., 250.)
    (object_of_geometry
      (Basic
        (Fill.plain Colour.black,
         StrokedPath (Shapes.circle 100. 100. 100.,
           {Shapes.startcap = Shapes.ButtCap;
            Shapes.endcap = Shapes.ButtCap;
            Shapes.join = Shapes.RoundJoin;
            Shapes.mitrelimit = 5.;
            Shapes.linewidth = 1.;}))))

let hole = 
  position_anchor Centre (250., 300.)
    (object_of_geometry
        (Filters.hole (Basic (Fill.plain Colour.white, rectangle 200. 200. 100.  100.))))

let wireframe =
  position_anchor Centre (200., 200.)
  (object_of_geometry
    (Filters.wireframe
    (Basic (Fill.plain Colour.white, Path (Shapes.circle 0. 0. 40.)))
    {Shapes.startcap = Shapes.ButtCap;
     Shapes.join = Shapes.BevelJoin;
     Shapes.endcap = Shapes.ButtCap;
     Shapes.linewidth = 0.5;
     Shapes.mitrelimit = root2}
    (Fill.plain (Colour.dissolve Colour.black ~delta:200))))

let p6_curve () =
  (let pth = pdf_pathsinfile "brushcurve.pdf" in
    position_anchor Centre (300., 300.)
      (Obj (Id.new_ids (),
       (Basic ((Fill.plain (Colour.dissolve Colour.slateblue 255)),
         (StrokedPath (pth,
  {Shapes.startcap = Shapes.RoundCap;
   Shapes.join = Shapes.RoundJoin;
   Shapes.endcap = Shapes.RoundCap;
   Shapes.linewidth = 2.;
   Shapes.mitrelimit = root2}
           )))),
       Pdftransform.i,
       Over)))

let p6_curve2 () =
  position_anchor Centre (310., 310.)
    (Obj(Id.new_ids (),
     (Basic((Fill.plain (Colour.dissolve Colour.slateblue 128)),
       (StrokedPath(pdf_pathsinfile "brushcurve.pdf",
    {Shapes.startcap = Shapes.RoundCap;
     Shapes.join = Shapes.RoundJoin;
     Shapes.endcap = Shapes.RoundCap;
     Shapes.linewidth = 4.;
     Shapes.mitrelimit = root2}
         )))),
     Pdftransform.i,
     Over))

let p6_curve3 () =
  position_anchor Centre (320., 320.)
    (Obj(Id.new_ids (),
     (Basic((Fill.plain (Colour.dissolve Colour.slateblue 192)),
       (StrokedPath(pdf_pathsinfile "brushcurve.pdf",
    {Shapes.startcap = Shapes.RoundCap;
     Shapes.join = Shapes.RoundJoin;
     Shapes.endcap = Shapes.RoundCap;
     Shapes.linewidth = 4.;
     Shapes.mitrelimit = root2}
         )))),
     Pdftransform.i,
     Over))

let curves () =
  position_anchor Centre (120., 300.)
    (mkgroup [p6_curve (); p6_curve2 (); p6_curve3 ()])

let smear () =
  let pth = pdf_pathsinfile "brushcurve.pdf" in
    position_anchor Centre (250., 350.)
    (Obj (Id.new_ids (),
          (Filters.smear (Brush.mkround 15. 1., pth)),
          Pdftransform.i,
          Over))

let wirebrush () =
  let pth = pdf_pathsinfile "brushcurve.pdf" in
    position_anchor Centre (250., 350.)
    (Obj (Id.new_ids (),
      (Filters.wireframe
        (Basic (Fill.plain Colour.white, Brushstroke (Brush.mkround 15. 1., pth)))
    {Shapes.startcap = Shapes.ButtCap;
     Shapes.join = Shapes.BevelJoin;
     Shapes.endcap = Shapes.ButtCap;
     Shapes.linewidth = 0.5;
     Shapes.mitrelimit = root2}
    (Fill.plain Colour.black))
          ,
          Pdftransform.i,
          Over))

let monobrush () =
  let pth = pdf_pathsinfile "brushcurve.pdf" in
    position_anchor Centre (250., 350.)
    (Obj (Id.new_ids (),
      (Filters.monochrome
        (Basic (Fill.plain Colour.white, Brushstroke (Brush.mkround 15. 1., pth)))),
          Pdftransform.i,
          Over))

let curve () =
 position_anchor Centre (500.0, 200.0)
   (Obj(Id.new_ids (),
     (Basic((Fill.plain Colour.gold),
       Brushstroke(Brush.mkround 20. 0.6,
         pdf_pathsinfile "brushcurve.pdf"))),
     Pdftransform.i,
     Over))

