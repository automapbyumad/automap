# Makefile generique.
# Nom du programme
PRG= automap
# Fichiers ML du programme
ML= vector.ml quadTree.ml e3D.ml automap.ml
MLI= ${ML:.ml=.mli}
LIBS= GL.cmxa Glu.cmxa bigarray.cmxa VBO.cmxa vertArray.cmxa genimg_loader.cmxa sdl.cmxa sdlloader.cmxa str.cmxa unix.cmxa lablgtk.cmxa
INCDIRS= -I +sdl -I +glMLite -I +lablgtk2

# Les listes de fichiers à produire
CMO=${ML:.ml=.cmo}
CMX=${ML:.ml=.cmx}
CMI=${MLI:.mli=.cmi}
# Les compilateurs
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
OCAMLDEP=ocamldep
${PRG}: ${CMX}
	${OCAMLOPT} $(INCDIRS) -o $@ $(LIBS) main.o ${CMX}

.SUFFIXES: .ml .mli .cmo .cmx .cmi

.ml.cmx:
	${OCAMLOPT} $(INCDIRS) -c $< $(LIBS)

.ml.cmo:
	${OCAMLOPT} ${INCDIRS} -c $< $(LIBS)

.mli.cmi:
	${OCAMLOPT} $(INCDIRS) -c $< $(LIBS)

clean::
	rm -f *.cm? *~
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
