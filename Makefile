RESULT     = automap
SOURCES    = e3d.ml automap.ml
LIBS       = lablgl.cmxa bigarray.cmxa sdl.cmxa sdlloader.cmxa str.cmxa lablgtk.cmxa
INCDIRS    = -I +sdl -I +lablGL -I +lablgtk2

all: $(RESULT)

$(RESULT): $(SOURCES)
	@echo "Compiling $(RESULT)..."
       #@ocamlopt $(INCDIRS) -c $(SOURCES) $(LIBS)#
	@ocamlopt $(INCDIRS) -o $(RESULT) $(LIBS) $(SOURCES)
	@echo "Done !"

clean:
	rm -f $(RESULT).cm? *.cm* *.o *~ automap temp.bmp temp_grid.bmp canny.bmp cfg.txt test.obj resultat.bmp 