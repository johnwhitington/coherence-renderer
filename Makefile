SOURCES = pdfgraphics.ml pdfgraphics.mli camlpy.ml camlpy.mli pytalk.ml pytalk.mli id.ml id.mli\
	  colour.ml colour.mli coord.ml coord.mli\
	  canvas.ml canvas.mli fill.ml fill.mli\
	  sprite.ml sprite.mli\
	  wxgui.ml wxgui.mli convolve.ml convolve.mli\
	  polygon.ml polygon.mli brush.ml brush.mli\
     	  shapes.ml shapes.mli cache.ml cache.mli render.ml render.mli\
	  filters.ml filters.mli\
	  messages.ml messages.mli undo.ml undo.mli\
	  icons.ml icons.mli examples.ml engine.ml

RESULT = engine

LIBS = unix
OCAMLNCFLAGS = -g -w -3
OCAMLBCFLAGS = -g -w -3
OCAMLLDFLAGS = -g

PACKS = camlgpc camlpdf

all : native-code htdoc

clean ::
	rm -rf doc foo foo2 *.pyc

-include OCamlMakefile

