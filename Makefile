RESULT     = automap
SOURCES    = vector.ml quadTree.ml e3D.ml automap.ml
LIBS       = GL.cmxa Glu.cmxa bigarray.cmxa VBO.cmxa vertArray.cmxa genimg_loader.cmxa png_loader.cmxa jpeg_loader.cmxa sdl.cmxa sdlloader.cmxa str.cmxa unix.cmxa lablgtk.cmxa
INCDIRS    = -I +sdl -I +glMLite -I +lablgtk2

all: $(RESULT)

$(RESULT): $(SOURCES)
	@echo "Compiling $(RESULT)..."
	@ocamlopt $(INCDIRS) -o $(RESULT) $(LIBS) $(SOURCES)
	@echo "Done !"

clean:
	rm -f ${SOURCES:ml=cm?} *.o *~ automap temp.bmp border_tmp.bmp temp_grid.bmp canny.bmp mark.bmp cfg.txt test.obj resultat.bmp temp_qt.bmp \#*\#
