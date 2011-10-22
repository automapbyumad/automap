RESULT     = automap
SOURCES    = automap.ml
LIBS       = lablgl.cma bigarray.cma sdl.cma sdlloader.cma str.cma lablgtk.cma
INCDIRS    = -I +sdl -I +lablGL -I +lablgtk2

all: $(RESULT)

$(RESULT): $(SOURCES)
	@echo "Compiling $(RESULT)..."
	@ocamlc $(INCDIRS) -o $(RESULT) $(LIBS) $(SOURCES)
	@echo "Done !"

clean:
	rm -f $(RESULT).cm? *.o *~ automap temp.bmp temp_grid.bmp canny.bmp cfg.txt
