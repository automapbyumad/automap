# {{{ COPYING 
#
# +-----------------------------------------------------------------------+
# | This file contains compile rules to build glMLite, an OCaml binding   |
# | to the OpenGL API.                                                    |
# +-----------------------------------------------------------------------+
# | Copyright (C) 2006 - 2010 Florent Monnier <fmonnier@linux-nantes.org> |
# +-----------------------------------------------------------------------+
# | This program is free software: you can redistribute it and/or         |
# | modify it under the terms of the GNU General Public License           |
# | as published by the Free Software Foundation, either version 3        |
# | of the License, or (at your option) any later version.                |
# |                                                                       |
# | This program is distributed in the hope that it will be useful,       |
# | but WITHOUT ANY WARRANTY; without even the implied warranty of        |
# | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         |
# | GNU General Public License for more details.                          |
# |                                                                       |
# | You should have received a copy of the GNU General Public License     |
# | along with this program.  If not, see <http://www.gnu.org/licenses/>  |
# +-----------------------------------------------------------------------+
#
# }}}

include ./Makefile.depend

all:
	(cd $(SRC); $(MAKE))

install:
	(cd $(SRC); $(MAKE) install)

uninstall:
	(cd $(SRC); $(MAKE) uninstall)

install_findlib:
	(cd $(SRC); $(MAKE) install_findlib)

uninstall_findlib:
	(cd $(SRC); $(MAKE) uninstall_findlib)

# {{{ partial builds 

core: core_byte core_opt
core_byte:
	(cd $(SRC); $(MAKE) core_byte)
core_opt:
	(cd $(SRC); $(MAKE) core_opt)
install_core:
	(cd $(SRC); $(MAKE) install_core)

.PHONY: core core_byte core_opt install_core


gl:
	(cd $(SRC); $(MAKE) gl gl_opt)
glu:
	(cd $(SRC); $(MAKE) glu glu_opt)
glut:
	(cd $(SRC); $(MAKE) glut glut_opt)
va varray:
	(cd $(SRC); $(MAKE) varray varray_opt)
# TODO: the installs for these
.PHONY: gl glu glut va varray


fun:
	(cd $(SRC); $(MAKE) fungl)
	(cd $(SRC); $(MAKE) funglut)
install_fun:
	(cd $(SRC); $(MAKE) install_fun)
.PHONY: fun install_fun

jpeg jpeg_opt jpeg_byte:
	(cd $(SRC); $(MAKE) $@)
install_jpeg:
	(cd $(SRC); $(MAKE) install_jpeg)

.PHONY: jpeg install_jpeg 

gle: gle_byte gle_opt
gle_byte:
	(cd $(SRC); $(MAKE) gle_byte -f Makefile.GLE)
gle_opt:
	(cd $(SRC); $(MAKE) gle_opt -f Makefile.GLE)
install_gle install_GLE:
	(cd $(SRC); $(MAKE) install_gle -f Makefile.GLE)

.PHONY: gle gle_byte gle_opt install_gle

gen genimg:
	(cd $(SRC); $(MAKE) all -f Makefile.IM)
genimg_byte genimg_opt:
	(cd $(SRC); $(MAKE) all -f Makefile.IM $@)
install_genimg:
	(cd $(SRC); $(MAKE) install -f Makefile.IM)

.PHONY: genimg install_genimg genimg_byte genimg_opt

svg rsvg: rsvg_byte rsvg_opt
svg_byte: rsvg_byte
svg_opt: rsvg_opt

rsvg_byte rsvg_opt:
	(cd $(SRC); $(MAKE) all -f Makefile.rsvg $@)
install_svg install_rsvg:
	(cd $(SRC); $(MAKE) install -f Makefile.rsvg)

.PHONY: svg rsvg install_svg install_rsvg 
.PHONY: svg_byte rsvg_byte svg_opt rsvg_opt
 
png: png_byte png_opt
png_byte:
	(cd $(SRC); $(MAKE) -f Makefile.png png_byte)
png_opt:
	(cd $(SRC); $(MAKE) -f Makefile.png png_opt)
install_png:
	(cd $(SRC); $(MAKE) -f Makefile.png install)

.PHONY: png png_byte png

ftgl: ftgl_byte ftgl_opt
ftgl_byte:
	(cd $(SRC); $(MAKE) -f Makefile.ftgl ftgl_byte)
ftgl_opt:
	(cd $(SRC); $(MAKE) -f Makefile.ftgl ftgl_opt)
install_ftgl:
	(cd $(SRC); $(MAKE) -f Makefile.ftgl install)

.PHONY: ftgl ftgl_byte ftgl_opt install_ftgl

vbo: vbo_byte vbo_opt
vbo_byte:
	(cd $(SRC); $(MAKE) vbo_byte -f Makefile.VBO)
vbo_opt:
	(cd $(SRC); $(MAKE) vbo_opt -f Makefile.VBO)
install_vbo:
	(cd $(SRC); $(MAKE) install -f Makefile.VBO)
clean_vbo:
	(cd $(SRC); $(MAKE) vbo_clean -f Makefile.VBO)

.PHONY: vbo vbo_byte vbo_opt install_vbo

everything: core jpeg fun genimg svg png gle ftgl vbo doc
install_everything: install_core install_jpeg install_fun install_genimg install_svg install_png install_gle install_ftgl install_vbo

clean_everything:\
  clean_core  clean_jpeg  clean_fun  clean_genimg  clean_svg  clean_png  clean_vbo

# }}}

doc:
	(cd $(SRC); $(MAKE) doc)
	@echo ' Read the documentation with:'
	@echo ' $$BROWSER SRC/doc/index.html'
install_doc:
	(cd $(SRC); $(MAKE) install_doc)

clean:
	(cd $(SRC); $(MAKE) clean)

clean_all clean-all cleanall: clean  clean-pack
	(cd $(SRC); $(MAKE) clean-all)
	rm -f *~

.PHONY: all install doc clean cleanall clean_all

# {{{ tarball 

VERSION=XX
PACK=$(DIST_NAME)-$(VERSION)

pack: $(PACK).tgz

upload-pack: $(PACK).tgz
	chmod a+r $(PACK).tgz
	scp $< \
	  fmonnier@tux.linux-nantes.fr.eu.org:~/public_html/OCaml/GL/
	chmod o-r $(PACK).tgz


SRC_FILES=\
	$(SRC)/META.in              \
	$(SRC)/meta.ml              \
	$(SRC)/GL.ml.pp             \
	$(SRC)/Glu.ml.pp            \
	$(SRC)/Glut.ml.pp           \
	$(SRC)/Glut.mli.pp          \
	$(SRC)/Makefile             \
	$(SRC)/Makefile.enums       \
	$(SRC)/Makefile.mlpp        \
	$(SRC)/configure.c          \
	$(SRC)/configure_boot.ml    \
	$(SRC)/mlpp.ml              \
	$(SRC)/struct_to_sig.ml     \
	$(SRC)/enums-gen.ml         \
	$(SRC)/enums_xmlm.ml        \
	$(SRC)/xmlm.ml              \
	$(SRC)/xmlm.mli             \
	$(SRC)/enums.list.xml       \
	$(SRC)/gl.wrap.c            \
	$(SRC)/gl.wrap.h            \
	$(SRC)/glu.wrap.c           \
	$(SRC)/glut.wrap.c          \
	\
	$(SRC)/GLE.ml.pp            \
	$(SRC)/gle.wrap.c           \
	$(SRC)/Makefile.GLE         \
	\
	$(SRC)/jpeg_loader.ml       \
	$(SRC)/loader-libjpeg.c     \
	$(SRC)/loader-libjpeg-mem.c \
	$(SRC)/loader-texure.h      \
	\
	$(SRC)/png_loader.ml        \
	$(SRC)/loader-libpng.c      \
	$(SRC)/Makefile.png         \
	\
	$(SRC)/genimg_loader.ml     \
	$(SRC)/loader-libmagick.c   \
	$(SRC)/Makefile.IM          \
	$(SRC)/genimg_loader.README.txt \
	\
	$(SRC)/svg_loader.ml        \
	$(SRC)/loader-librsvg.c     \
	$(SRC)/Makefile.rsvg        \
	\
	$(SRC)/vertArray.ml.pp      \
	$(SRC)/varray.wrap.c        \
	$(SRC)/Makefile.VArray      \
	\
	$(SRC)/VBO.ml.pp            \
	$(SRC)/vbo.wrap.c           \
	$(SRC)/Makefile.VBO         \
	\
	$(SRC)/FunGL.ml.pp          \
	$(SRC)/FunGL.README.txt     \
	$(SRC)/FunGL.Interoperability.txt \
	$(SRC)/fungl.wrap.c         \
	$(SRC)/FunGlut.ml           \
	\
	$(SRC)/ftgl.ml              \
	$(SRC)/ftgl.wrap.c          \
	$(SRC)/Makefile.ftgl        \
	\
	$(SRC)/OSMesa.ml            \
	$(SRC)/osmesa.wrap.c        \
	$(SRC)/osdemo.ml            \
	$(SRC)/Makefile.OSMesa      \
	$(SRC)/make_osmesa.sh       \
	$(SRC)/ml_osdemo.sh         \
	$(SRC)/.style.css           \
	$(SRC)/put_version.ml       \
	\
	$(SRC)/gl3_deprecations.c   \
	$(SRC)/gl3_deprecations.h   \
	#EOL


QUATERN=toolbox/quaternions/
QUATERN_FILES=\
	$(QUATERN)/Makefile         \
	$(QUATERN)/quaternions.ml   \
	$(QUATERN)/quaternions.mli  \
	#EOL


ROOT_FILES=\
	LICENSE_GPL.txt             \
	README.txt                  \
	README.enums.txt            \
	README.GL3.txt              \
	Makefile                    \
	Makefile.depend             \
	TODO                        \
	#EOL

LICENSE_GPL.txt:
	wget http://www.gnu.org/licenses/gpl-3.0.txt
	mv  gpl-3.0.txt  $@

$(PACK):  $(ROOT_FILES)
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp $^ $@/


$(PACK)/$(QUATERN): $(QUATERN_FILES)
	if [ ! -d $(PACK) ]; then mkdir $(PACK) ; fi
	if [ ! -d $@ ]; then mkdir -p $@ ; fi
	cp $^ $@/


$(PACK)/$(INTRF):  $(INTRF_FILES)
	if [ ! -d $(PACK) ]; then mkdir $(PACK) ; fi
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp $^ $@/


$(PACK)/$(INTRF_SMPL):  $(INTRF_SMPL_FILES)
	if [ ! -d $(PACK) ]; then mkdir $(PACK) ; fi
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp $^ $@/


$(PACK)/$(SMPL):  $(SMPL_FILES)
	if [ ! -d $(PACK) ]; then mkdir $(PACK) ; fi
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp $^ $@/


$(PACK)/$(GLED):  $(GLED_FILES)
	if [ ! -d $(PACK) ]; then mkdir $(PACK) ; fi
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp $^ $@/


$(PACK)/$(NEHE):  $(NEHE_FILES)
	if [ ! -d $@ ]; then mkdir -p $@ ; fi
	cp $^ $@/
	cp -R $(NEHE_SUBS) $@/
	for f in `tree -fida --noreport $(PACK)/$(NEHE_SUBS) | grep svn`; do rm -rf $$f; done


$(PACK)/$(SRC):  $(SRC_FILES)
	if [ ! -d $(PACK) ]; then mkdir $(PACK) ; fi
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp $^ $@/

	sed -i -e "s:VERSION:$(VERSION):g" $@/META.in

	ocaml SRC/put_version.ml $@/GL.ml.pp $(VERSION)

	sed -i  \
	  -e 's|^DOC_INSTALL_PATH=.*$$||'  \
	  -e 's|#\(DOC_INSTALL_PATH=.*\)$$|\1|'  \
	  $(PACK)/$(SRC)/Makefile


pack-dir:    $(PACK)               \
             $(PACK)/$(SRC)        \
             $(PACK)/$(QUATERN)    \
             $(PACK)/$(INTRF)      \
             $(PACK)/$(INTRF_SMPL) \
             $(PACK)/$(SMPL)       \
             $(PACK)/$(GLED)       \
             $(PACK)/$(NEHE)

$(PACK).tar: $(PACK)               \
             $(PACK)/$(SRC)        \
             $(PACK)/$(QUATERN)    \
             $(PACK)/$(INTRF)      \
             $(PACK)/$(INTRF_SMPL) \
             $(PACK)/$(SMPL)       \
             $(PACK)/$(GLED)       \
             $(PACK)/$(NEHE)
	tar cf $@ $</

$(PACK).tar.lzma:  $(PACK).tar
	lzma --best $<

$(PACK).tar.gz:  $(PACK).tar
	gzip --best $<

$(PACK).tlz:  $(PACK).tar.lzma
	mv $< $@
	ls -lh --color $@

$(PACK).tgz:  $(PACK).tar.gz
	mv $< $@
	ls -lh --color $@
	md5sum $@

clean-pack:
	rm -f  $(PACK).tgz  LICENSE_GPL.txt
	rm -rf $(PACK)/
# }}}

# vim: fdm=marker
