# Makefile generique.
# Nom du programme
PRG= automap

# Fichier C du programme
SRC= generate.c
# Fichiers ML du programme
ML= vector.ml quadTree.ml e3D.ml automap.ml

# Flags
CFLAGS= -W -Wall -Werror -ansi -pedantic -lSDL -lSDL_image
LIBS= GL.cmxa Glu.cmxa bigarray.cmxa VBO.cmxa vertArray.cmxa genimg_loader.cmxa sdl.cmxa sdlloader.cmxa str.cmxa unix.cmxa lablgtk.cmxa
INCDIRS= -I +sdl -I +glMLite -I +lablgtk2

# Les listes de fichiers à produire
OBJ=${SRC:.c=.o}
MLI=${ML:.ml=.mli}
CMO=${ML:.ml=.cmo}
CMX=${ML:.ml=.cmx}
CMI=${MLI:.mli=.cmi}

# Les compilateurs
CC=gcc
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
OCAMLDEP=ocamldep

${PRG}: ${OBJ} ${CMX}
	${OCAMLOPT} $(INCDIRS) -o $@ $(LIBS) ${OBJ} ${CMX}

.SUFFIXES: .c .o .ml .mli .cmo .cmx .cmi

.c.o:
	${CC} ${CFLAGS} -c $<

.ml.cmx:
	${OCAMLOPT} $(INCDIRS) -c $< $(LIBS)

.ml.cmo:
	${OCAMLOPT} ${INCDIRS} -c $< $(LIBS)

.mli.cmi:
	${OCAMLOPT} $(INCDIRS) -c $< $(LIBS)

clean::
	rm -f *.cm? *~ *.o
	rm -f ${PRG} ${PRG}.byte
	rm -f ${SOURCES:ml=cm?} ${ML:ml=o} *~ automap temp.bmp border_tmp.bmp temp_grid.bmp canny.bmp mark.bmp cfg.txt test.obj resultat.bmp temp_qt.bmp generate_texture.bmp \#*\#


fullclean:: clean
	rm -f .depend

# Creation du fichier de dependances
# Il faut appeller make depend avant de recompiler votre programme
# Si vous utilisez le GNU make (standard sous linux) il faudra inclure
# le fichier. BSD make (bmake sur vos rack) inclut automatiquement le
# fichier .depend s'il existe
depend: .depend
.depend: ${ML} ${MLI}
	rm -f .depend
	${OCAMLDEP} ${ML} ${MLI} > .depend

# Inclusion du fichier de dependance (GNU Make)
# GNU Make "reconstruira" le fichier .depend s'il n'existe pas.
include .depend

# Il faut toujours un (au moins) saut de ligne en fin de fichier,
# c'est pour ca que l'on finit toujours les Makefile par un
# commentaire.
# END
