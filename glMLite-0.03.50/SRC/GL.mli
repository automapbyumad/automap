(* {{{ COPYING *(

  +-----------------------------------------------------------------------+
  |  This file belongs to glMLite, an OCaml binding to the OpenGL API.    |
  +-----------------------------------------------------------------------+
  |  Copyright (C) 2006, 2007, 2008, 2009  Florent Monnier                |
  |  Contact:  <fmonnier@linux-nantes.org>                                |
  +-----------------------------------------------------------------------+
  |  This program is free software: you can redistribute it and/or        |
  |  modify it under the terms of the GNU General Public License          |
  |  as published by the Free Software Foundation, either version 3       |
  |  of the License, or (at your option) any later version.               |
  |                                                                       |
  |  This program is distributed in the hope that it will be useful,      |
  |  but WITHOUT ANY WARRANTY; without even the implied warranty of       |
  |  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        |
  |  GNU General Public License for more details.                         |
  |                                                                       |
  |  You should have received a copy of the GNU General Public License    |
  |  along with this program.  If not, see <http://www.gnu.org/licenses/> |
  +-----------------------------------------------------------------------+

)* }}} *)


(* http://camltastic.blogspot.com/2008/08/tip-calling-c-functions-directly-with.html
 *)

(** OpenGL functions *)

(**
{{:http://www.opengl.org/documentation/specs/version1.1/glspec1.1/}OpenGL Specifications}

{{:http://www.opengl.org/resources/faq/technical/}OpenGL FAQ}
*)

(*
http://www.opengl.org/code/
*)

(** {3 Drawing Functions} *)

# 1 "enums/primitive.inc.ml" 1
type primitive =
  | GL_POINTS
  | GL_LINES
  | GL_LINE_LOOP
  | GL_LINE_STRIP
  | GL_TRIANGLES
  | GL_TRIANGLE_STRIP
  | GL_TRIANGLE_FAN
  | GL_QUADS  (** deprecated in core OpenGL 3. *)
  | GL_QUAD_STRIP  (** deprecated in core OpenGL 3. *)
  | GL_POLYGON  (** deprecated in core OpenGL 3. *)
# 44 "GL.ml.pp" 2

external glBegin: primitive:primitive -> unit = "ml_glbegin" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glBegin.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glEnd: unit -> unit = "ml_glend" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEnd.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glVertex2: x:float -> y:float -> unit = "ml_glvertex2" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glVertex3: x:float -> y:float -> z:float -> unit = "ml_glvertex3" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glVertex4: x:float -> y:float -> z:float -> w:float -> unit = "ml_glvertex4" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glVertex2v: float * float -> unit = "ml_glvertex2v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glVertex3v: float * float * float -> unit = "ml_glvertex3v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glVertex4v: float * float * float * float -> unit = "ml_glvertex4v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glNormal3: nx:float -> ny:float -> nz:float -> unit = "ml_glnormal3" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glNormal3v: v:float * float * float -> unit = "ml_glnormal3v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glIndex: c:float -> unit = "ml_glindexd" "noalloc"
external glIndexi: c:int -> unit = "ml_glindexi" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glColor3: r:float -> g:float -> b:float -> unit = "ml_glcolor3" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glColor4: r:float -> g:float -> b:float -> a:float -> unit = "ml_glcolor4" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glColor3v: v:float * float * float -> unit = "ml_glcolor3v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glColor4v: v:float * float * float * float -> unit = "ml_glcolor4v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glColor3c: r:char -> g:char -> b:char -> unit = "ml_glcolor3c" "noalloc"
external glColor4c: r:char -> g:char -> b:char -> a:char -> unit = "ml_glcolor4c" "noalloc"
(** not clamped to range [\[0.0 - 1.0\]] but [\['\000' - '\255'\]] *)

external glColor3cv: v:char * char * char -> unit = "ml_glcolor3cv" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glColor4cv: v:char * char * char * char -> unit = "ml_glcolor4cv" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glEdgeFlag: flag:bool -> unit = "ml_gledgeflag" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlag.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glRasterPos2: x:float -> y:float -> unit = "ml_glrasterpos2d" "noalloc"
external glRasterPos3: x:float -> y:float -> z:float -> unit = "ml_glrasterpos3d" "noalloc"
external glRasterPos4: x:float -> y:float -> z:float -> w:float -> unit = "ml_glrasterpos4d" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glRasterPos2i: x:int -> y:int -> unit = "ml_glrasterpos2i" "noalloc"
external glRasterPos3i: x:int -> y:int -> z:int -> unit = "ml_glrasterpos3i" "noalloc"
external glRasterPos4i: x:int -> y:int -> z:int -> w:int -> unit = "ml_glrasterpos4i" "noalloc"

external glRasterPos2v: v:float * float -> unit = "ml_glrasterpos2dv" "noalloc"
external glRasterPos3v: v:float * float * float -> unit = "ml_glrasterpos3dv" "noalloc"
external glRasterPos4v: v:float * float * float * float -> unit = "ml_glrasterpos4dv" "noalloc"
external glRasterPos2iv: v:int * int -> unit = "ml_glrasterpos2iv" "noalloc"
external glRasterPos3iv: v:int * int * int -> unit = "ml_glrasterpos3iv" "noalloc"
external glRasterPos4iv: v:int * int * int * int -> unit = "ml_glrasterpos4iv" "noalloc"

(** All glRasterPos* functions are deprecated in core OpenGL 3. *)

external glRecti: x1:int -> y1:int -> x2:int -> y2:int -> unit = "ml_glrecti" "noalloc"
external glRect: x1:float -> y1:float -> x2:float -> y2:float -> unit = "ml_glrect" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


(** {3 Transformations} *)

# 1 "enums/matrix_mode.inc.ml" 1
type matrix_mode =
  | GL_MODELVIEW
  | GL_PROJECTION
  | GL_TEXTURE
# 170 "GL.ml.pp" 2
external glMatrixMode: mode:matrix_mode -> unit = "ml_glmatrixmode" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glMatrixMode.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glViewport: x:int -> y:int -> width:int -> height:int -> unit = "ml_glviewport" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glViewport.xml}
    manual page on opengl.org} *)

external glOrtho: left:float -> right:float -> bottom:float ->
                  top:float -> near:float -> far:float -> unit
         = "ml_glortho_bytecode"
           "ml_glortho_native"
           "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glFrustum: left:float -> right:float -> bottom:float ->
                    top:float -> near:float -> far:float -> unit
         = "ml_glfrustum_bytecode"
           "ml_glfrustum_native"
           "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glFrustum.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glPushMatrix: unit -> unit = "ml_glpushmatrix" "noalloc"
external glPopMatrix: unit -> unit = "ml_glpopmatrix" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPushMatrix.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glLoadIdentity: unit -> unit = "ml_glloadidentity" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLoadIdentity.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


external glRotatev: angle:float -> vec:float * float * float -> unit = "ml_glrotatev" "noalloc"
external glRotate: angle:float -> x:float -> y:float -> z:float -> unit = "ml_glrotate" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glTranslatev: float * float * float -> unit = "ml_gltranslatev" "noalloc"
external glTranslate: x:float -> y:float -> z:float -> unit = "ml_gltranslate" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTranslate.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glScalev: float * float * float -> unit = "ml_glscalev" "noalloc"
external glScale: x:float -> y:float -> z:float -> unit = "ml_glscale" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glScale.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

(* TODO
glMultTransposeMatrix
glLoadTransposeMatrix
*)

(* {{{ glMultMatrix *)
# 234 "GL.ml.pp"

val glMultMatrix: mat:float array array -> unit
(** checks the matrix given is 4x4 with assertions

    {{:http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

# 265 "GL.ml.pp"
(* }}} *)

external glMultMatrixFlat: float array -> unit = "ml_glmultmatrixd_flat" "noalloc"
(** same than [glMultMatrix] but with an array of length 16

    {{:http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glMultMatrixFlat_unsafe: float array -> unit = "ml_glmultmatrixd_flat_unsafe"
(** same than [glMultMatrixFlat] but doesn't make any checks.
    @deprecated in core OpenGL 3. *)


(* {{{ glLoadMatrix *)
# 281 "GL.ml.pp"

val glLoadMatrix: mat:float array array -> unit
(** checks the matrix given is 4x4 with assertions

    {{:http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

# 312 "GL.ml.pp"
(* }}} *)

external glLoadMatrixFlat: float array -> unit = "ml_glloadmatrixd_flat" "noalloc"
(** as [glLoadMatrix] but with an array of length 16

    {{:http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glLoadMatrixFlat_unsafe: float array -> unit = "ml_glloadmatrixd_flat_unsafe" "noalloc"
(** same than [glLoadMatrixFlat] but doesn't make any checks
    @deprecated in core OpenGL 3. *)



(** {3 Miscellaneous} *)

external glFlush: unit -> unit = "ml_glflush" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glFlush.xml}
    manual page on opengl.org} *)

# 1 "enums/orientation.inc.ml" 1
type orientation =
  | GL_CW
  | GL_CCW
# 334 "GL.ml.pp" 2
external glFrontFace: orientation:orientation -> unit = "ml_glfrontface" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glFrontFace.xml}
    manual page on opengl.org} *)

external glScissor: x:int -> y:int -> width:int -> height:int -> unit = "ml_glscissor" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml}
    manual page on opengl.org} *)

external glFinish: unit -> unit = "ml_glfinish" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glFinish.xml}
    manual page on opengl.org} *)


external glClearColor: r:float -> g:float -> b:float -> a:float -> unit = "ml_glclearcolor" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClearColor.xml}
    manual page on opengl.org} *)

external glClearIndex: float -> unit = "ml_glclearindex" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClearIndex.xml}
    manual page on opengl.org} *)

external glColorMask: r:bool -> g:bool -> b:bool -> a:bool -> unit = "ml_glcolormask" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColorMask.xml}
    manual page on opengl.org} *)


module Attrib : sig (* PACK_ENUM *)
# 1 "enums/attrib_bit.inc.ml" 1
type attrib_bit =
  | GL_ACCUM_BUFFER_BIT  (** deprecated in core OpenGL 3. *)
  | GL_COLOR_BUFFER_BIT
  | GL_CURRENT_BIT
  | GL_DEPTH_BUFFER_BIT
  | GL_ENABLE_BIT
  | GL_EVAL_BIT
  | GL_FOG_BIT
  | GL_HINT_BIT
  | GL_LIGHTING_BIT
  | GL_LINE_BIT
  | GL_LIST_BIT
  | GL_MULTISAMPLE_BIT
  | GL_PIXEL_MODE_BIT
  | GL_POINT_BIT
  | GL_POLYGON_BIT
  | GL_POLYGON_STIPPLE_BIT
  | GL_SCISSOR_BIT
  | GL_STENCIL_BUFFER_BIT
  | GL_TEXTURE_BIT
  | GL_TRANSFORM_BIT
  | GL_VIEWPORT_BIT
# 362 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glPushAttrib: attrib:Attrib.attrib_bit list -> unit = "ml_glpushattrib" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPushAttrib.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glPopAttrib: unit -> unit = "ml_glpopattrib" "noalloc"
(** @deprecated in core OpenGL 3. *)


# 1 "enums/face_mode.inc.ml" 1
type face_mode =
  | GL_FRONT
  | GL_BACK
  | GL_FRONT_AND_BACK
# 373 "GL.ml.pp" 2
# 1 "enums/polygon_mode.inc.ml" 1
type polygon_mode =
  | GL_POINT
  | GL_LINE
  | GL_FILL
# 374 "GL.ml.pp" 2
external glPolygonMode: face:face_mode -> mode:polygon_mode -> unit = "ml_glpolygonmode" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPolygonMode.xml}
    manual page on opengl.org} *)

external glGetPolygonMode: unit -> polygon_mode * polygon_mode = "ml_glgetpolygonmode"
(** glGet with argument GL_POLYGON_MODE
    {{:http://www.opengl.org/sdk/docs/man/xhtml/glGet.xml}
    manual page on opengl.org} *)

# 1 "enums/clear_mask.inc.ml" 1
type clear_mask =
  | GL_COLOR_BUFFER_BIT
  | GL_DEPTH_BUFFER_BIT
  | GL_ACCUM_BUFFER_BIT  (** deprecated in core OpenGL 3. *)
  | GL_STENCIL_BUFFER_BIT
# 384 "GL.ml.pp" 2
external glClear: mask:clear_mask list -> unit = "ml_glclear" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClear.xml}
    manual page on opengl.org} *)


external glLineWidth: width:float -> unit = "ml_gllinewidth" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLineWidth.xml}
    manual page on opengl.org}.
    In OpenGL 3, this function does not support values greater than 1.0 anymore. *)

external glPointSize: size:float -> unit = "ml_glpointsize" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPointSize.xml}
    manual page on opengl.org} *)

(* {{{ glPointParameter *)

type sprite_coord_origin = GL_LOWER_LEFT | GL_UPPER_LEFT

type point_parameter =
  | GL_POINT_SIZE_MIN of float
  | GL_POINT_SIZE_MAX of float
  | GL_POINT_FADE_THRESHOLD_SIZE of float
  | GL_POINT_DISTANCE_ATTENUATION of float * float * float
  | GL_POINT_SPRITE_COORD_ORIGIN of sprite_coord_origin

# 410 "GL.ml.pp"

(* glext *)

val glPointParameter: point_parameter -> unit
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml}
    manual page on opengl.org} *)

# 433 "GL.ml.pp"
(* }}} *)


# 1 "enums/gl_func.inc.ml" 1
type gl_func =
  | GL_NEVER
  | GL_LESS
  | GL_EQUAL
  | GL_LEQUAL
  | GL_GREATER
  | GL_NOTEQUAL
  | GL_GEQUAL
  | GL_ALWAYS
# 437 "GL.ml.pp" 2

external glAlphaFunc: func:gl_func -> ref:float -> unit = "ml_glalphafunc" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glAlphaFunc.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

# 1 "enums/hint_target.inc.ml" 1
type hint_target =
  | GL_FOG_HINT  (** deprecated in core OpenGL 3. *)
  | GL_LINE_SMOOTH_HINT
  | GL_PERSPECTIVE_CORRECTION_HINT  (** deprecated in core OpenGL 3. *)
  | GL_POINT_SMOOTH_HINT  (** deprecated in core OpenGL 3. *)
  | GL_POLYGON_SMOOTH_HINT
  | GL_TEXTURE_COMPRESSION_HINT
  | GL_GENERATE_MIPMAP_HINT  (** deprecated in core OpenGL 3. *)
  | GL_FRAGMENT_SHADER_DERIVATIVE_HINT
# 444 "GL.ml.pp" 2
# 1 "enums/hint_mode.inc.ml" 1
type hint_mode =
  | GL_FASTEST
  | GL_NICEST
  | GL_DONT_CARE
# 445 "GL.ml.pp" 2
external glHint: target:hint_target -> mode:hint_mode -> unit = "ml_glhint" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glHint.xml}
    manual page on opengl.org} *)

external glCullFace: mode:face_mode -> unit = "ml_glcullface" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCullFace.xml}
    manual page on opengl.org} *)

(* TODO
test this function:
*)
external glGetCullFaceMode: unit -> face_mode = "ml_glgetcullfacemode" "noalloc"
(** associated get for {!glCullFace} *)

external glLineStipple: factor:int -> pattern:int -> unit = "ml_gllinestipple" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLineStipple.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

type polygon_stipple_mask = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external glPolygonStipple: mask:polygon_stipple_mask -> unit = "ml_glpolygonstipple" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPolygonStipple.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glPolygonStipple_unsafe: mask:polygon_stipple_mask -> unit = "ml_glpolygonstipple_unsafe" "noalloc"
(** Same than [glPolygonStipple] but does not check the size of the big array.
    @deprecated in core OpenGL 3. *)

module DrawBuffer : sig (* PACK_ENUM *)
# 1 "enums/draw_buffer_mode.inc.ml" 1
type draw_buffer_mode =
  | GL_NONE
  | GL_FRONT_LEFT
  | GL_FRONT_RIGHT
  | GL_BACK_LEFT
  | GL_BACK_RIGHT
  | GL_FRONT
  | GL_BACK
  | GL_LEFT
  | GL_RIGHT
  | GL_FRONT_AND_BACK
  | GL_AUX0
  | GL_AUX1
  | GL_AUX2
  | GL_AUX3
# 476 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glDrawBuffer: mode:DrawBuffer.draw_buffer_mode -> unit = "ml_gldrawbuffer" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffer.xml}
    manual page on opengl.org} *)

module ReadBuffer : sig (* PACK_ENUM *)
# 1 "enums/read_buffer_mode.inc.ml" 1
type read_buffer_mode =
  | GL_FRONT_LEFT
  | GL_FRONT_RIGHT
  | GL_BACK_LEFT
  | GL_BACK_RIGHT
  | GL_FRONT
  | GL_BACK
  | GL_LEFT
  | GL_RIGHT
  | GL_AUX0
  | GL_AUX1
  | GL_AUX2
  | GL_AUX3
# 483 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glReadBuffer: mode:ReadBuffer.read_buffer_mode -> unit = "ml_glreadbuffer" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glReadBuffer.xml}
    manual page on opengl.org} *)


module Sfactor : sig (* PACK_ENUM *)
# 1 "enums/blend_sfactor.inc.ml" 1
type blend_sfactor =
  | GL_ZERO
  | GL_ONE
  | GL_DST_COLOR
  | GL_ONE_MINUS_DST_COLOR
  | GL_SRC_ALPHA
  | GL_ONE_MINUS_SRC_ALPHA
  | GL_DST_ALPHA
  | GL_ONE_MINUS_DST_ALPHA
  | GL_SRC_ALPHA_SATURATE
# 491 "GL.ml.pp" 2
end (* PACK_ENUM *)
module Dfactor : sig (* PACK_ENUM *)
# 1 "enums/blend_dfactor.inc.ml" 1
type blend_dfactor =
  | GL_ZERO
  | GL_ONE
  | GL_SRC_COLOR
  | GL_ONE_MINUS_SRC_COLOR
  | GL_SRC_ALPHA
  | GL_ONE_MINUS_SRC_ALPHA
  | GL_DST_ALPHA
  | GL_ONE_MINUS_DST_ALPHA
# 494 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glBlendFunc: sfactor:Sfactor.blend_sfactor -> dfactor:Dfactor.blend_dfactor -> unit = "ml_glblendfunc" "noalloc"
(**
  {{:http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunc.xml}
  manual page on opengl.org} ;
  {{:http://www.opengl.org/resources/faq/technical/transparency.htm}
  Transparency, Translucency, and Blending Chapter}
*)

(* TODO
glBlendFuncSeparate
http://www.opengl.org/sdk/docs/man/xhtml/glBlendFuncSeparate.xml
*)

# 1 "enums/blend_mode.inc.ml" 1
type blend_mode =
  | GL_FUNC_ADD
  | GL_FUNC_SUBTRACT
  | GL_FUNC_REVERSE_SUBTRACT
  | GL_MIN
  | GL_MAX
# 509 "GL.ml.pp" 2
external glBlendEquation: mode:blend_mode -> unit = "ml_glblendequation" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquation.xml}
    manual page on opengl.org} *)

(*
# 1 "enums/blend_mode_ext.inc.ml" 1
type blend_mode_ext =
  | GL_FUNC_ADD_EXT
  | GL_FUNC_SUBTRACT_EXT
  | GL_FUNC_REVERSE_SUBTRACT_EXT
  | GL_MIN_EXT
  | GL_MAX_EXT
  | GL_ALPHA_MIN_SGIX
  | GL_ALPHA_MAX_SGIX
  | GL_LOGIC_OP
# 515 "GL.ml.pp" 2
external glBlendEquationEXT: mode:blend_mode_ext -> unit = "ml_glblendequationext"
(** {{:http://techpubs.sgi.com/library/tpl/cgi-bin/getdoc.cgi?db=man&fname=/usr/share/catman/g_man/cat3/OpenGL/glblendequationext.z}
    man page on sgi.com} *)
*)

module Op : sig (* PACK_ENUM *)
# 1 "enums/op_code.inc.ml" 1
type op_code =
  | GL_CLEAR
  | GL_SET
  | GL_COPY
  | GL_COPY_INVERTED
  | GL_NOOP
  | GL_INVERT
  | GL_AND
  | GL_NAND
  | GL_OR
  | GL_NOR
  | GL_XOR
  | GL_EQUIV
  | GL_AND_REVERSE
  | GL_AND_INVERTED
  | GL_OR_REVERSE
  | GL_OR_INVERTED
# 522 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glLogicOp: opcode:Op.op_code -> unit = "ml_gllogicop" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLogicOp.xml}
    manual page on opengl.org} *)


(* TODO
glIndexMask
*)

external glPolygonOffset: factor:float -> units:float -> unit = "ml_glpolygonoffset" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml}
    manual page on opengl.org} *)




(** {3 Lighting} *)

# 1 "enums/shade_mode.inc.ml" 1
type shade_mode =
  | GL_FLAT
  | GL_SMOOTH
# 542 "GL.ml.pp" 2
external glShadeModel: shade_mode -> unit = "ml_glshademodel" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glShadeModel.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

(* TODO
glGet with argument GL_SHADE_MODEL
*)

(* {{{ glLight *)

module Light : sig (* _PACK_ENUM *)
type light_pname =
  | GL_SPOT_EXPONENT of float
  | GL_SPOT_CUTOFF of float
  | GL_CONSTANT_ATTENUATION of float
  | GL_LINEAR_ATTENUATION of float
  | GL_QUADRATIC_ATTENUATION of float
  | GL_SPOT_DIRECTION of (float * float * float)
  | GL_AMBIENT of (float * float * float * float)
  | GL_DIFFUSE of (float * float * float * float)
  | GL_SPECULAR of (float * float * float * float)
  | GL_POSITION of (float * float * float * float)
end (* _PACK_ENUM *)

type gl_light = GL_LIGHT of int

# 570 "GL.ml.pp"

val glLight: light:gl_light -> pname:Light.light_pname -> unit
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

# 606 "GL.ml.pp"
(* }}} *)
(* {{{ glLightModel *)

type color_control =
  | GL_SEPARATE_SPECULAR_COLOR
  | GL_SINGLE_COLOR

type light_model =
  | GL_LIGHT_MODEL_AMBIENT of (float * float * float * float)
  | GL_LIGHT_MODEL_COLOR_CONTROL of color_control
  | GL_LIGHT_MODEL_LOCAL_VIEWER of bool
  | GL_LIGHT_MODEL_TWO_SIDE of bool

# 620 "GL.ml.pp"

val glLightModel: light_model:light_model -> unit
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

(** See [glGetLight] for associated get. *)

# 645 "GL.ml.pp"
(* }}} *)


(* {{{ glMaterial *)

module Material : sig (* _PACK_ENUM *)
type material_mode =
  | GL_AMBIENT of (float * float * float * float)
  | GL_DIFFUSE of (float * float * float * float)
  | GL_SPECULAR of (float * float * float * float)
  | GL_EMISSION of (float * float * float * float)
  | GL_SHININESS of float
  | GL_AMBIENT_AND_DIFFUSE of (float * float * float * float)
  | GL_COLOR_INDEXES of (int * int * int)  (* TODO: check if these should be floats *)
end (* _PACK_ENUM *)

# 662 "GL.ml.pp"

val glMaterial: face:face_mode -> mode:Material.material_mode -> unit
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

# 690 "GL.ml.pp"

(* }}} *)


module GetMat : sig (* _PACK_ENUM *)
type face_mode = GL_FRONT | GL_BACK
type get_material_4f =
  | GL_AMBIENT
  | GL_DIFFUSE
  | GL_SPECULAR
  | GL_EMISSION
type get_material_1f =
  | GL_SHININESS
type get_material_3i =
  | GL_COLOR_INDEXES  (* TODO: check if these should be floats *)
end (* _PACK_ENUM *)

external glGetMaterial4f: GetMat.face_mode -> mode:GetMat.get_material_4f -> float * float * float * float = "ml_glgetmaterial4f"
external glGetMaterial1f: GetMat.face_mode -> mode:GetMat.get_material_1f -> float = "ml_glgetmaterial1f"
external glGetMaterial3i: GetMat.face_mode -> mode:GetMat.get_material_3i -> int * int * int = "ml_glgetmaterial3i"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetMaterial.xml}
    manual page on opengl.org} *)


# 1 "enums/color_material_mode.inc.ml" 1
type color_material_mode =
  | GL_EMISSION
  | GL_AMBIENT
  | GL_DIFFUSE
  | GL_SPECULAR
  | GL_AMBIENT_AND_DIFFUSE
# 715 "GL.ml.pp" 2
external glColorMaterial: face:face_mode -> mode:color_material_mode -> unit = "ml_glcolormaterial" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glColorMaterial.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


external glSecondaryColor3: red:float -> green:float -> blue:float -> unit = "ml_glsecondarycolor3d" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)
(* TODO
glSecondaryColor3c
glSecondaryColor3v
glSecondaryColor3cv
*)



(** {3 Stenciling} *)

external glStencilFunc: func:gl_func -> ref:int -> mask:int -> unit = "ml_glstencilfunc" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glStencilFunc.xml}
    manual page on opengl.org} *)

external glStencilFuncn: func:gl_func -> ref:int -> mask:nativeint -> unit = "ml_glstencilfuncn" "noalloc"
(** OCaml standard ints have 1 bit missing from nativeint. *)

external glStencilMask: mask:int -> unit = "ml_glstencilmask" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glStencilMask.xml}
    manual page on opengl.org} *)

(* TODO: get the full range of the GLuint
void glStencilMask( GLuint mask );
*)

# 1 "enums/stencil_op.inc.ml" 1
type stencil_op =
  | GL_KEEP
  | GL_ZERO
  | GL_REPLACE
  | GL_INCR
  | GL_INCR_WRAP
  | GL_DECR
  | GL_DECR_WRAP
  | GL_INVERT
# 751 "GL.ml.pp" 2
external glStencilOp: sfail:stencil_op -> dpfail:stencil_op -> dppass:stencil_op -> unit = "ml_glstencilop" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glStencilOp.xml}
    manual page on opengl.org} *)

external glClearStencil: s:int -> unit = "ml_glclearstencil" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClearStencil.xml}
    manual page on opengl.org} *)





(** {3 Depth Buffer} *)

external glDepthRange: near:float -> far:float -> unit = "ml_gldepthrange" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDepthRange.xml}
    manual page on opengl.org} *)

external glClearDepth: depth:float -> unit = "ml_glcleardepth" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClearDepth.xml}
    manual page on opengl.org} *)

external glDepthFunc: func:gl_func -> unit = "ml_gldepthfunc" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDepthFunc.xml}
    manual page on opengl.org} *)

external glDepthMask: bool -> unit = "ml_gldepthmask" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDepthMask.xml}
    manual page on opengl.org} *)



(** {3 Accumulation Buffer} *)

# 1 "enums/accum_op.inc.ml" 1
type accum_op =
  | GL_ACCUM
  | GL_LOAD
  | GL_ADD
  | GL_MULT
  | GL_RETURN
# 786 "GL.ml.pp" 2
external glAccum: op:accum_op -> value:float -> unit = "ml_glaccum" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glAccum.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glClearAccum: r:float -> g:float -> b:float -> a:float -> unit = "ml_glclearaccum" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClearAccum.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)



(** {3 GL Capabilities} *)

# 1 "enums/gl_capability.inc.ml" 1
type gl_capability =
  | GL_ALPHA_TEST  (** deprecated in core OpenGL 3. *)
  | GL_AUTO_NORMAL
  | GL_BLEND
  | GL_CLIP_PLANE0
  | GL_CLIP_PLANE1
  | GL_CLIP_PLANE2
  | GL_CLIP_PLANE3
  | GL_CLIP_PLANE4
  | GL_CLIP_PLANE5
  | GL_COLOR_LOGIC_OP
  | GL_COLOR_MATERIAL  (** deprecated in core OpenGL 3. *)
  | GL_COLOR_TABLE
  | GL_CONVOLUTION_1D
  | GL_CONVOLUTION_2D
  | GL_CULL_FACE
  | GL_DEPTH_TEST
  | GL_DITHER
  | GL_FOG
  | GL_HISTOGRAM
  | GL_INDEX_LOGIC_OP
  | GL_LIGHT0  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT1  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT2  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT3  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT4  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT5  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT6  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT7  (** deprecated in core OpenGL 3. *)
  | GL_LIGHTING  (** deprecated in core OpenGL 3. *)
  | GL_LINE_SMOOTH
  | GL_LINE_STIPPLE
  | GL_MAP1_COLOR_4
  | GL_MAP1_INDEX
  | GL_MAP1_NORMAL
  | GL_MAP1_TEXTURE_COORD_1
  | GL_MAP1_TEXTURE_COORD_2
  | GL_MAP1_TEXTURE_COORD_3
  | GL_MAP1_TEXTURE_COORD_4
  | GL_MAP1_VERTEX_3
  | GL_MAP1_VERTEX_4
  | GL_MAP2_COLOR_4
  | GL_MAP2_INDEX
  | GL_MAP2_NORMAL
  | GL_MAP2_TEXTURE_COORD_1
  | GL_MAP2_TEXTURE_COORD_2
  | GL_MAP2_TEXTURE_COORD_3
  | GL_MAP2_TEXTURE_COORD_4
  | GL_MAP2_VERTEX_3
  | GL_MAP2_VERTEX_4
  | GL_MINMAX
  | GL_MULTISAMPLE
  | GL_NORMALIZE
  | GL_POINT_SMOOTH  (** deprecated in core OpenGL 3. *)
  | GL_POINT_SPRITE  (** deprecated in core OpenGL 3. *)
  | GL_POLYGON_OFFSET_FILL
  | GL_POLYGON_OFFSET_LINE
  | GL_POLYGON_OFFSET_POINT
  | GL_POLYGON_SMOOTH
  | GL_POLYGON_STIPPLE
  | GL_POST_COLOR_MATRIX_COLOR_TABLE
  | GL_POST_CONVOLUTION_COLOR_TABLE
  | GL_RESCALE_NORMAL  (** deprecated in core OpenGL 3. *)
  | GL_SAMPLE_ALPHA_TO_COVERAGE
  | GL_SAMPLE_ALPHA_TO_ONE
  | GL_SAMPLE_COVERAGE
  | GL_SEPARABLE_2D
  | GL_SCISSOR_TEST
  | GL_STENCIL_TEST
  | GL_TEXTURE_1D
  | GL_TEXTURE_2D
  | GL_TEXTURE_3D
  | GL_TEXTURE_CUBE_MAP
  | GL_TEXTURE_GEN_Q  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_R  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_S  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_T  (** deprecated in core OpenGL 3. *)
  | GL_VERTEX_PROGRAM_POINT_SIZE
  | GL_VERTEX_PROGRAM_TWO_SIDE  (** deprecated in core OpenGL 3. *)
# 801 "GL.ml.pp" 2
external glEnable: cap:gl_capability -> unit = "ml_glenable" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEnable.xml}
    manual page on opengl.org} *)
external glDisable: cap:gl_capability -> unit = "ml_gldisable" "noalloc"


module Enabled : sig (* PACK_ENUM *)
# 1 "enums/enabled_cap.inc.ml" 1
type enabled_cap =
  | GL_ALPHA_TEST  (** deprecated in core OpenGL 3. *)
  | GL_AUTO_NORMAL
  | GL_BLEND
  | GL_CLIP_PLANE0
  | GL_CLIP_PLANE1
  | GL_CLIP_PLANE2
  | GL_CLIP_PLANE3
  | GL_CLIP_PLANE4
  | GL_CLIP_PLANE5
  | GL_COLOR_ARRAY
  | GL_COLOR_LOGIC_OP
  | GL_COLOR_MATERIAL  (** deprecated in core OpenGL 3. *)
  | GL_COLOR_SUM
  | GL_COLOR_TABLE
  | GL_CONVOLUTION_1D
  | GL_CONVOLUTION_2D
  | GL_CULL_FACE
  | GL_DEPTH_TEST
  | GL_DITHER
  | GL_EDGE_FLAG_ARRAY
  | GL_FOG
  | GL_FOG_COORD_ARRAY
  | GL_HISTOGRAM
  | GL_INDEX_ARRAY
  | GL_INDEX_LOGIC_OP
  | GL_LIGHT0  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT1  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT2  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT3  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT4  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT5  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT6  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT7  (** deprecated in core OpenGL 3. *)
  | GL_LIGHTING  (** deprecated in core OpenGL 3. *)
  | GL_LINE_SMOOTH
  | GL_LINE_STIPPLE
  | GL_MAP1_COLOR_4
  | GL_MAP1_INDEX
  | GL_MAP1_NORMAL
  | GL_MAP1_TEXTURE_COORD_1
  | GL_MAP1_TEXTURE_COORD_2
  | GL_MAP1_TEXTURE_COORD_3
  | GL_MAP1_TEXTURE_COORD_4
  | GL_MAP2_COLOR_4
  | GL_MAP2_INDEX
  | GL_MAP2_NORMAL
  | GL_MAP2_TEXTURE_COORD_1
  | GL_MAP2_TEXTURE_COORD_2
  | GL_MAP2_TEXTURE_COORD_3
  | GL_MAP2_TEXTURE_COORD_4
  | GL_MAP2_VERTEX_3
  | GL_MAP2_VERTEX_4
  | GL_MINMAX
  | GL_MULTISAMPLE
  | GL_NORMAL_ARRAY
  | GL_NORMALIZE
  | GL_POINT_SMOOTH  (** deprecated in core OpenGL 3. *)
  | GL_POINT_SPRITE  (** deprecated in core OpenGL 3. *)
  | GL_POLYGON_SMOOTH
  | GL_POLYGON_OFFSET_FILL
  | GL_POLYGON_OFFSET_LINE
  | GL_POLYGON_OFFSET_POINT
  | GL_POLYGON_STIPPLE
  | GL_POST_COLOR_MATRIX_COLOR_TABLE
  | GL_POST_CONVOLUTION_COLOR_TABLE
  | GL_RESCALE_NORMAL  (** deprecated in core OpenGL 3. *)
  | GL_SAMPLE_ALPHA_TO_COVERAGE
  | GL_SAMPLE_ALPHA_TO_ONE
  | GL_SAMPLE_COVERAGE
  | GL_SCISSOR_TEST
  | GL_SECONDARY_COLOR_ARRAY
  | GL_SEPARABLE_2D
  | GL_STENCIL_TEST
  | GL_TEXTURE_1D
  | GL_TEXTURE_2D
  | GL_TEXTURE_3D
  | GL_TEXTURE_COORD_ARRAY
  | GL_TEXTURE_CUBE_MAP
  | GL_TEXTURE_GEN_Q  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_R  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_S  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_T  (** deprecated in core OpenGL 3. *)
  | GL_VERTEX_ARRAY
  | GL_VERTEX_PROGRAM_POINT_SIZE
  | GL_VERTEX_PROGRAM_TWO_SIDE  (** deprecated in core OpenGL 3. *)
# 809 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glIsEnabled: Enabled.enabled_cap -> bool = "ml_glisenabled" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabled.xml}
    manual page on opengl.org} *)



(** {3 Texture mapping} *)

type texture_id = private int
external glGenTextures: n:int -> texture_id array = "ml_glgentextures" (* DOES ALLOC *)
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGenTextures.xml}
    manual page on opengl.org} *)

external glGenTexture: unit -> texture_id = "ml_glgentexture" "noalloc"

module BindTex : sig (* PACK_ENUM *)
# 1 "enums/texture_binding.inc.ml" 1
type texture_binding =
  | GL_TEXTURE_1D
  | GL_TEXTURE_2D
  | GL_TEXTURE_3D
  | GL_TEXTURE_CUBE_MAP
# 827 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glBindTexture: target:BindTex.texture_binding -> texture:texture_id -> unit = "ml_glbindtexture" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glBindTexture.xml}
    manual page on opengl.org} *)

external glUnbindTexture: target:BindTex.texture_binding -> unit = "ml_glunbindtexture" "noalloc"
(* deactive the previous active texture
   (is equivalent to the C call glBindTexture(target, 0)) *)

external glBindTexture2D: texture:texture_id -> unit = "ml_glbindtexture2d" "noalloc"
(** equivalent to [glBindTexture] with parameter [GL_TEXTURE_2D] *)

external glUnbindTexture2D: unit -> unit = "ml_glunbindtexture2d" "noalloc"
(* deactive the previous active texture
   (is equivalent to the C call glBindTexture(GL_TEXTURE_2D, 0)) *)

external glDeleteTextures: textures:texture_id array -> unit = "ml_gldeletetextures" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDeleteTextures.xml}
    manual page on opengl.org} *)

external glDeleteTexture: texture:texture_id -> unit = "ml_gldeletetexture" "noalloc"

external glIsTexture: texture:texture_id -> bool = "ml_glistexture" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glIsTexture.xml}
    manual page on opengl.org} *)

external glPrioritizeTextures: textures:texture_id array -> priority:float array -> unit = "ml_glprioritizetextures" "noalloc"
external glPrioritizeTexture: texture:texture_id -> priority:float -> unit = "ml_glprioritizetexture" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPrioritizeTextures.xml}
    manual page on opengl.org} *)

external glPrioritizeTexturesp: prioritized_textures:(texture_id * float) array -> unit = "ml_glprioritizetexturesp" "noalloc"

(* TODO
type target = GL_TEXTURE_ENV | GL_TEXTURE_FILTER_CONTROL | GL_POINT_SPRITE
type pname =
    GL_TEXTURE_ENV_MODE
  | GL_TEXTURE_LOD_BIAS
  | GL_COMBINE_RGB
  | GL_COMBINE_ALPHA
  | GL_SRC0_RGB
  | GL_SRC1_RGB
  | GL_SRC2_RGB
  | GL_SRC0_ALPHA
  | GL_SRC1_ALPHA
  | GL_SRC2_ALPHA
  | GL_OPERAND0_RGB
  | GL_OPERAND1_RGB
  | GL_OPERAND2_RGB
  | GL_OPERAND0_ALPHA
  | GL_OPERAND1_ALPHA
  | GL_OPERAND2_ALPHA
  | GL_RGB_SCALE
  | GL_ALPHA_SCALE
  | GL_COORD_REPLACE
type param =
    GL_ADD
  | GL_ADD_SIGNED
  | GL_INTERPOLATE
  | GL_MODULATE
  | GL_DECAL
  | GL_BLEND
  | GL_REPLACE
  | GL_SUBTRACT
  | GL_COMBINE
  | GL_TEXTURE
  | GL_CONSTANT
  | GL_PRIMARY_COLOR
  | GL_PREVIOUS
  | GL_SRC_COLOR
  | GL_ONE_MINUS_SRC_COLOR
  | GL_SRC_ALPHA
  | GL_ONE_MINUS_SRC_ALPHA
  (* a single boolean value for the point sprite texture coordinate replacement,
     a single floating-point value for the texture level-of-detail bias, or 1.0,
     2.0, or 4.0 when specifying the GL_RGB_SCALE or GL_ALPHA_SCALE. *)

void glTexEnvfv( GLenum target, GLenum pname, const GLfloat * params);
void glTexEnviv( GLenum target, GLenum pname, const GLint * params);

void glTexEnvf( GLenum target, GLenum pname, GLfloat  param);
void glTexEnvi( GLenum target, GLenum pname, GLint  param);


type target = GL_TEXTURE_ENV | GL_TEXTURE_FILTER_CONTROL
type pname = GL_TEXTURE_ENV_MODE | GL_TEXTURE_ENV_COLOR of rgba | GL_TEXTURE_LOD_BIAS
type params = (* Specifies a pointer to a parameter array that contains either
     a single symbolic constant,
     single floating-point number,
     or an RGBA color. *)
*)
module TexEnv : sig (* PACK_ENUM *)
# 1 "enums/texenv_target.inc.ml" 1
type texenv_target =
  | GL_TEXTURE_ENV
  | GL_TEXTURE_FILTER_CONTROL
  | GL_POINT_SPRITE  (** deprecated in core OpenGL 3. *)
# 920 "GL.ml.pp" 2
# 1 "enums/texenv_pname.inc.ml" 1
type texenv_pname =
  | GL_TEXTURE_ENV_MODE
  | GL_TEXTURE_LOD_BIAS
  | GL_COMBINE_RGB
  | GL_COMBINE_ALPHA
  | GL_SRC0_RGB
  | GL_SRC1_RGB
  | GL_SRC2_RGB
  | GL_SRC0_ALPHA
  | GL_SRC1_ALPHA
  | GL_SRC2_ALPHA
  | GL_OPERAND0_RGB
  | GL_OPERAND1_RGB
  | GL_OPERAND2_RGB
  | GL_OPERAND0_ALPHA
  | GL_OPERAND1_ALPHA
  | GL_OPERAND2_ALPHA
  | GL_RGB_SCALE
  | GL_ALPHA_SCALE
  | GL_COORD_REPLACE
# 921 "GL.ml.pp" 2
# 1 "enums/texenv_param.inc.ml" 1
type texenv_param =
  | GL_ADD
  | GL_ADD_SIGNED
  | GL_INTERPOLATE
  | GL_MODULATE
  | GL_DECAL
  | GL_BLEND
  | GL_REPLACE
  | GL_SUBTRACT
  | GL_COMBINE
  | GL_TEXTURE
  | GL_CONSTANT
  | GL_PRIMARY_COLOR
  | GL_PREVIOUS
  | GL_SRC_COLOR
  | GL_ONE_MINUS_SRC_COLOR
  | GL_SRC_ALPHA
  | GL_ONE_MINUS_SRC_ALPHA
  | GL_TRUE
  | GL_FALSE
# 922 "GL.ml.pp" 2
end (* PACK_ENUM *)
(*
type texenv_scale = SCALE_1 | SCALE_2 | SCALE_4
  | GL_RGB_SCALE of texenv_scale
  | GL_ALPHA_SCALE of texenv_scale
*)
external glTexEnv: TexEnv.texenv_target -> TexEnv.texenv_pname -> TexEnv.texenv_param -> unit = "ml_gltexenv" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexEnv.xml}
    manual page on opengl.org} *)

# 1 "enums/tex_coord.inc.ml" 1
type tex_coord =
  | GL_S
  | GL_T
  | GL_R
  | GL_Q
# 933 "GL.ml.pp" 2
# 1 "enums/tex_coord_gen_func.inc.ml" 1
type tex_coord_gen_func =
  | GL_TEXTURE_GEN_MODE
# 934 "GL.ml.pp" 2
# 1 "enums/tex_gen_param.inc.ml" 1
type tex_gen_param =
  | GL_OBJECT_LINEAR
  | GL_EYE_LINEAR
  | GL_SPHERE_MAP
  | GL_NORMAL_MAP
  | GL_REFLECTION_MAP
# 935 "GL.ml.pp" 2
external glTexGen: tex_coord -> tex_coord_gen_func -> tex_gen_param -> unit = "ml_gltexgen" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

# 1 "enums/tex_coord_fun_params.inc.ml" 1
type tex_coord_fun_params =
  | GL_OBJECT_PLANE
  | GL_EYE_PLANE
# 941 "GL.ml.pp" 2

external glTexGenv: tex_coord -> tex_coord_fun_params -> float * float * float * float -> unit = "ml_gltexgenv"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glTexGenva: tex_coord -> tex_coord_fun_params -> float array -> unit = "ml_gltexgenva"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


module TexTarget : sig (* _PACK_ENUM *)
# 1 "enums/target_2d.inc.ml" 1
type target_2d =
  | GL_TEXTURE_2D
  | GL_PROXY_TEXTURE_2D
  | GL_TEXTURE_CUBE_MAP_POSITIVE_X
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  | GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  | GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
  | GL_PROXY_TEXTURE_CUBE_MAP
# 955 "GL.ml.pp" 2

type target_1d = GL_TEXTURE_1D | GL_PROXY_TEXTURE_1D
type target_3d = GL_TEXTURE_3D | GL_PROXY_TEXTURE_3D
end (* _PACK_ENUM *)

module InternalFormat : sig (* PACK_ENUM *)
# 1 "enums/internal_format.inc.ml" 1
type internal_format =
  | GL_ALPHA
  | GL_ALPHA4
  | GL_ALPHA8
  | GL_ALPHA12
  | GL_ALPHA16
  | GL_COMPRESSED_ALPHA
  | GL_COMPRESSED_LUMINANCE
  | GL_COMPRESSED_LUMINANCE_ALPHA
  | GL_COMPRESSED_INTENSITY
  | GL_COMPRESSED_RGB
  | GL_COMPRESSED_RGBA
  | GL_DEPTH_COMPONENT
  | GL_DEPTH_COMPONENT16
  | GL_DEPTH_COMPONENT24
  | GL_DEPTH_COMPONENT32
  | GL_LUMINANCE
  | GL_LUMINANCE4
  | GL_LUMINANCE8
  | GL_LUMINANCE12
  | GL_LUMINANCE16
  | GL_LUMINANCE_ALPHA
  | GL_LUMINANCE4_ALPHA4
  | GL_LUMINANCE6_ALPHA2
  | GL_LUMINANCE8_ALPHA8
  | GL_LUMINANCE12_ALPHA4
  | GL_LUMINANCE12_ALPHA12
  | GL_LUMINANCE16_ALPHA16
  | GL_INTENSITY
  | GL_INTENSITY4
  | GL_INTENSITY8
  | GL_INTENSITY12
  | GL_INTENSITY16
  | GL_R3_G3_B2
  | GL_RGB
  | GL_RGB4
  | GL_RGB5
  | GL_RGB8
  | GL_RGB10
  | GL_RGB12
  | GL_RGB16
  | GL_RGBA
  | GL_RGBA2
  | GL_RGBA4
  | GL_RGB5_A1
  | GL_RGBA8
  | GL_RGB10_A2
  | GL_RGBA12
  | GL_RGBA16
  | GL_SLUMINANCE
  | GL_SLUMINANCE8
  | GL_SLUMINANCE_ALPHA
  | GL_SLUMINANCE8_ALPHA8
  | GL_SRGB
  | GL_SRGB8
  | GL_SRGB_ALPHA
  | GL_SRGB8_ALPHA8
# 962 "GL.ml.pp" 2
end (* PACK_ENUM *)
# 1 "enums/pixel_data_format.inc.ml" 1
type pixel_data_format =
  | GL_COLOR_INDEX
  | GL_RED
  | GL_GREEN
  | GL_BLUE
  | GL_ALPHA
  | GL_RGB
  | GL_RGBA
  | GL_LUMINANCE
  | GL_LUMINANCE_ALPHA
# 964 "GL.ml.pp" 2
# 1 "enums/pixel_data_type.inc.ml" 1
type pixel_data_type =
  | GL_UNSIGNED_BYTE
  | GL_BYTE
  | GL_BITMAP
  | GL_UNSIGNED_SHORT
  | GL_SHORT
  | GL_UNSIGNED_INT
  | GL_INT
  | GL_FLOAT
  | GL_UNSIGNED_BYTE_3_3_2
  | GL_UNSIGNED_BYTE_2_3_3_REV
  | GL_UNSIGNED_SHORT_5_6_5
  | GL_UNSIGNED_SHORT_5_6_5_REV
  | GL_UNSIGNED_SHORT_4_4_4_4
  | GL_UNSIGNED_SHORT_4_4_4_4_REV
  | GL_UNSIGNED_SHORT_5_5_5_1
  | GL_UNSIGNED_SHORT_1_5_5_5_REV
  | GL_UNSIGNED_INT_8_8_8_8
  | GL_UNSIGNED_INT_8_8_8_8_REV
  | GL_UNSIGNED_INT_10_10_10_2
  | GL_UNSIGNED_INT_2_10_10_10_REV
# 965 "GL.ml.pp" 2

(** input type to provide the textures *)
type img_input =
  | Filename of string  (** provide the filename of a texture *)
  | Buffer of string    (** provide the image data as a buffer *)

(*
type image_data
*)
type image_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.(*Array3*)Genarray.t


# 979 "GL.ml.pp"
val assert_size : width:int -> height:int -> unit
(** utility function to check if the dimensions of the image are compatible with OpenGL textures. *)
# 990 "GL.ml.pp"
 

(* TODO
 add all new type for recent GL versions
*)
external glTexImage2D:
    target:TexTarget.target_2d -> level:int ->
    internal_format:InternalFormat.internal_format ->
    width:int -> height:int ->
    format_:pixel_data_format ->
    type_:pixel_data_type ->
    pixels:image_data -> unit
    = "ml_glteximage2d_bytecode"
      "ml_glteximage2d_native"
      "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexImage2D.xml}
    manual page on opengl.org} *)

external glTexImage2D_str:
    target:TexTarget.target_2d -> level:int ->
    internal_format:InternalFormat.internal_format ->
    width:int -> height:int ->
    format_:pixel_data_format ->
    type_:pixel_data_type ->
    pixels:string -> unit
    = "ml_glteximage2d_str_bytecode"
      "ml_glteximage2d_str_native"
      "noalloc"

external glTexImage1D: target:TexTarget.target_1d -> level:int ->
    internal_format:InternalFormat.internal_format ->
    width:int ->
    format_:pixel_data_format ->
    type_:pixel_data_type ->
    pixels:image_data -> unit
    = "ml_glteximage1d_bytecode"
      "ml_glteximage1d_native"
      "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexImage1D.xml}
    manual page on opengl.org} *)

external glTexImage3D: target:TexTarget.target_3d -> level:int ->
    internal_format:InternalFormat.internal_format ->
    width:int -> height:int -> depth:int ->
    format_:pixel_data_format ->
    type_:pixel_data_type ->
    pixels:image_data -> unit
    = "ml_glteximage3d_bytecode"
      "ml_glteximage3d_native"
      "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexImage3D.xml}
    manual page on opengl.org} *)

external glTexCoord1: s:float -> unit = "ml_gltexcoord1" "noalloc"
external glTexCoord2: s:float -> t:float -> unit = "ml_gltexcoord2" "noalloc"
external glTexCoord3: s:float -> t:float -> r:float -> unit = "ml_gltexcoord3" "noalloc"
external glTexCoord4: s:float -> t:float -> r:float -> q:float -> unit = "ml_gltexcoord4" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glTexCoord2v: v:float * float -> unit = "ml_gltexcoord2v" "noalloc"
external glTexCoord3v: v:float * float * float -> unit = "ml_gltexcoord3v" "noalloc"
external glTexCoord4v: v:float * float * float * float -> unit = "ml_gltexcoord4v" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


(* {{{ glTexParameter *)

module Min : sig (* PACK_ENUM *)
# 1 "enums/min_filter.inc.ml" 1
type min_filter =
  | GL_NEAREST
  | GL_LINEAR
  | GL_NEAREST_MIPMAP_NEAREST
  | GL_LINEAR_MIPMAP_NEAREST
  | GL_NEAREST_MIPMAP_LINEAR
  | GL_LINEAR_MIPMAP_LINEAR
# 1063 "GL.ml.pp" 2
end (* PACK_ENUM *)
module Mag : sig (* PACK_ENUM *)
# 1 "enums/mag_filter.inc.ml" 1
type mag_filter =
  | GL_NEAREST
  | GL_LINEAR
# 1066 "GL.ml.pp" 2
end (* PACK_ENUM *)

# 1 "enums/wrap_param.inc.ml" 1
type wrap_param =
  | GL_CLAMP  (** deprecated in core OpenGL 3. *)
  | GL_CLAMP_TO_BORDER
  | GL_CLAMP_TO_EDGE
  | GL_MIRRORED_REPEAT
  | GL_REPEAT
# 1069 "GL.ml.pp" 2

module TexParam : sig (* _PACK_ENUM *)

# 1 "enums/tex_param_target.inc.ml" 1
type tex_param_target =
  | GL_TEXTURE_1D
  | GL_TEXTURE_2D
  | GL_TEXTURE_3D
  | GL_TEXTURE_CUBE_MAP
# 1073 "GL.ml.pp" 2

type texture_compare_mode =
  (*
  | GL_VERSION_1_4::GL_COMPARE_R_TO_TEXTURE
  *)
  | GL_NONE

type depth_texture_mode =
  | GL_LUMINANCE
  | GL_INTENSITY
  | GL_ALPHA

(** parameter for [glTexParameter] *)
type tex_param =
  | GL_TEXTURE_MIN_FILTER of Min.min_filter
  | GL_TEXTURE_MAG_FILTER of Mag.mag_filter
  | GL_TEXTURE_MIN_LOD of float
  | GL_TEXTURE_MAX_LOD of float
  | GL_TEXTURE_BASE_LEVEL of int
  | GL_TEXTURE_MAX_LEVEL of int
  | GL_TEXTURE_WRAP_S of wrap_param
  | GL_TEXTURE_WRAP_T of wrap_param
  | GL_TEXTURE_WRAP_R of wrap_param
  | GL_TEXTURE_BORDER_COLOR of (float * float * float * float)
  | GL_TEXTURE_PRIORITY of float

  | GL_TEXTURE_COMPARE_MODE of texture_compare_mode  (** only in GL >= 1.4 *)
  | GL_TEXTURE_COMPARE_FUNC of gl_func               (** only in GL >= 1.4 *)
  | GL_DEPTH_TEXTURE_MODE of depth_texture_mode      (** only in GL >= 1.4 *)
  | GL_GENERATE_MIPMAP of bool                       (** only in GL >= 1.4 *)  (* GL_TRUE / GL_FALSE *)

end (* _PACK_ENUM *)

# 1107 "GL.ml.pp"
val glTexParameter:
    target:TexParam.tex_param_target ->
    param:TexParam.tex_param -> unit
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml}
    manual page on opengl.org} *)
# 1144 "GL.ml.pp"

(* }}} *)

(* TODO
http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml
*)

module CopyTex : sig (* PACK_ENUM *)
# 1 "enums/copy_tex_target.inc.ml" 1
type copy_tex_target =
  | GL_TEXTURE_2D
  | GL_TEXTURE_CUBE_MAP_POSITIVE_X
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  | GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  | GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
# 1153 "GL.ml.pp" 2
end (* PACK_ENUM *)

external glCopyTexImage2D:
    target:CopyTex.copy_tex_target ->
    level:int ->
    internal_format:InternalFormat.internal_format ->
    x:int ->
    y:int ->
    width:int ->
    height:int ->
    border:int -> unit
    = "ml_glcopyteximage2d_bytecode"
      "ml_glcopyteximage2d_native"
      "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage2D.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glSampleCoverage: value:float -> invert:bool -> unit = "ml_glsamplecoverage" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glSampleCoverage.xml}
    manual page on opengl.org} *)


(** {3 Raster functions} *)

# 1 "enums/pixel_packing_b.inc.ml" 1
type pixel_packing_b =
  | GL_PACK_SWAP_BYTES
  | GL_PACK_LSB_FIRST
  | GL_UNPACK_SWAP_BYTES
  | GL_UNPACK_LSB_FIRST
# 1179 "GL.ml.pp" 2
external glPixelStoreb: pixel_packing:pixel_packing_b -> param:bool -> unit = "ml_glpixelstoreb" "noalloc"

# 1 "enums/pixel_packing_i.inc.ml" 1
type pixel_packing_i =
  | GL_PACK_ROW_LENGTH
  | GL_PACK_IMAGE_HEIGHT
  | GL_PACK_SKIP_PIXELS
  | GL_PACK_SKIP_ROWS
  | GL_PACK_SKIP_IMAGES
  | GL_PACK_ALIGNMENT
  | GL_UNPACK_ROW_LENGTH
  | GL_UNPACK_IMAGE_HEIGHT
  | GL_UNPACK_SKIP_PIXELS
  | GL_UNPACK_SKIP_ROWS
  | GL_UNPACK_SKIP_IMAGES
  | GL_UNPACK_ALIGNMENT
# 1182 "GL.ml.pp" 2
external glPixelStorei: pixel_packing:pixel_packing_i -> param:int -> unit = "ml_glpixelstorei" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml}
    manual page on opengl.org} *)

external glPixelZoom: xfactor:float -> yfactor:float -> unit = "ml_glpixelzoom" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPixelZoom.xml}
    manual page on opengl.org} *)



# 1 "enums/pixel_map.inc.ml" 1
type pixel_map =
  | GL_PIXEL_MAP_I_TO_I
  | GL_PIXEL_MAP_S_TO_S
  | GL_PIXEL_MAP_I_TO_R
  | GL_PIXEL_MAP_I_TO_G
  | GL_PIXEL_MAP_I_TO_B
  | GL_PIXEL_MAP_I_TO_A
  | GL_PIXEL_MAP_R_TO_R
  | GL_PIXEL_MAP_G_TO_G
  | GL_PIXEL_MAP_B_TO_B
  | GL_PIXEL_MAP_A_TO_A
# 1193 "GL.ml.pp" 2
external glPixelMapfv: map:pixel_map -> v:float array -> unit = "ml_glpixelmapfv"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml}
    manual page on opengl.org} *)


(*
void glPixelMapfv( GLenum map, GLsizei mapsize, const GLfloat *values );
void glPixelMapuiv( GLenum map, GLsizei mapsize, const GLuint *values );
void glPixelMapusv( GLenum map, GLsizei mapsize, const GLushort *values );
*)

(* TODO
void glGetPixelMapfv( GLenum map, GLfloat *values );
void glGetPixelMapuiv( GLenum map, GLuint *values );
void glGetPixelMapusv( GLenum map, GLushort *values );
*)

external glBitmap: width:int -> height:int -> xorig:float -> yorig:float ->
    xmove:float -> ymove:float -> bitmap:('a, 'b, 'c) Bigarray.Array1.t -> unit
    = "ml_glbitmap_bytecode"
      "ml_glbitmap_native"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glBitmap.xml}
    manual page on opengl.org} *)


# 1 "enums/pixel_type.inc.ml" 1
type pixel_type =
  | GL_COLOR
  | GL_DEPTH
  | GL_STENCIL
# 1219 "GL.ml.pp" 2
external glCopyPixels: x:int -> y:int -> width:int -> height:int -> pixel_type:pixel_type -> unit = "ml_glcopypixels" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCopyPixels.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

(* TODO
void glPixelTransferf( GLenum pname, GLfloat param);
void glPixelTransferi( GLenum pname, GLint param);
http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/pixeltransfer.html
*)


# 1 "enums/pixel_transfer_i.inc.ml" 1
type pixel_transfer_i =
  | GL_INDEX_SHIFT
  | GL_INDEX_OFFSET
# 1233 "GL.ml.pp" 2
# 1 "enums/pixel_transfer_f.inc.ml" 1
type pixel_transfer_f =
  | GL_RED_SCALE
  | GL_RED_BIAS
  | GL_GREEN_SCALE
  | GL_GREEN_BIAS
  | GL_BLUE_SCALE
  | GL_BLUE_BIAS
  | GL_ALPHA_SCALE
  | GL_ALPHA_BIAS
  | GL_DEPTH_SCALE
  | GL_DEPTH_BIAS
# 1234 "GL.ml.pp" 2
# 1 "enums/pixel_transfer_b.inc.ml" 1
type pixel_transfer_b =
  | GL_MAP_COLOR
  | GL_MAP_STENCIL
# 1235 "GL.ml.pp" 2

(** if the [ARB_imaging] extension is supported, these symbolic names are accepted *)
# 1 "enums/pixel_transfer_f_ARB.inc.ml" 1
type pixel_transfer_f_ARB =
  | GL_POST_COLOR_MATRIX_RED_SCALE
  | GL_POST_COLOR_MATRIX_GREEN_SCALE
  | GL_POST_COLOR_MATRIX_BLUE_SCALE
  | GL_POST_COLOR_MATRIX_ALPHA_SCALE
  | GL_POST_COLOR_MATRIX_RED_BIAS
  | GL_POST_COLOR_MATRIX_GREEN_BIAS
  | GL_POST_COLOR_MATRIX_BLUE_BIAS
  | GL_POST_COLOR_MATRIX_ALPHA_BIAS
  | GL_POST_CONVOLUTION_RED_SCALE
  | GL_POST_CONVOLUTION_GREEN_SCALE
  | GL_POST_CONVOLUTION_BLUE_SCALE
  | GL_POST_CONVOLUTION_ALPHA_SCALE
  | GL_POST_CONVOLUTION_RED_BIAS
  | GL_POST_CONVOLUTION_GREEN_BIAS
  | GL_POST_CONVOLUTION_BLUE_BIAS
  | GL_POST_CONVOLUTION_ALPHA_BIAS
# 1238 "GL.ml.pp" 2

external glPixelTransferi: pname:pixel_transfer_i -> param:int -> unit = "ml_glpixeltransferi"
external glPixelTransferf: pname:pixel_transfer_f -> param:float -> unit = "ml_glpixeltransferf"
external glPixelTransferb: pname:pixel_transfer_b -> param:bool -> unit = "ml_glpixeltransferb"
external glPixelTransferfARB: pname:pixel_transfer_f_ARB -> param:float -> unit = "ml_glpixeltransferfARB"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml}
    manual page on opengl.org} *)



module Framebuffer : sig (* PACK_ENUM *)
# 1 "enums/pixel_buffer_format.inc.ml" 1
type pixel_buffer_format =
  | GL_COLOR_INDEX
  | GL_STENCIL_INDEX
  | GL_DEPTH_COMPONENT
  | GL_RED
  | GL_GREEN
  | GL_BLUE
  | GL_ALPHA
  | GL_RGB
  | GL_BGR
  | GL_RGBA
  | GL_BGRA
  | GL_LUMINANCE
  | GL_LUMINANCE_ALPHA
# 1250 "GL.ml.pp" 2
# 1 "enums/pixel_buffer_type.inc.ml" 1
type pixel_buffer_type =
  | GL_UNSIGNED_BYTE
  | GL_BYTE
  | GL_BITMAP
  | GL_UNSIGNED_SHORT
  | GL_SHORT
  | GL_UNSIGNED_INT
  | GL_INT
  | GL_FLOAT
  | GL_UNSIGNED_BYTE_3_3_2
  | GL_UNSIGNED_BYTE_2_3_3_REV
  | GL_UNSIGNED_SHORT_5_6_5
  | GL_UNSIGNED_SHORT_5_6_5_REV
  | GL_UNSIGNED_SHORT_4_4_4_4
  | GL_UNSIGNED_SHORT_4_4_4_4_REV
  | GL_UNSIGNED_SHORT_5_5_5_1
  | GL_UNSIGNED_SHORT_1_5_5_5_REV
  | GL_UNSIGNED_INT_8_8_8_8
  | GL_UNSIGNED_INT_8_8_8_8_REV
  | GL_UNSIGNED_INT_10_10_10_2
  | GL_UNSIGNED_INT_2_10_10_10_REV
# 1251 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glReadPixelsBA_unsafe: x:int -> y:int -> width:int -> height:int ->
    Framebuffer.pixel_buffer_format -> Framebuffer.pixel_buffer_type -> image_data -> unit
    = "ml_glreadpixels_ba_unsafe_bytecode"
      "ml_glreadpixels_ba_unsafe_native"
      "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glReadPixels.xml}
    manual page on opengl.org} *)

external glReadPixelsBA: x:int -> y:int -> width:int -> height:int ->
    Framebuffer.pixel_buffer_format -> Framebuffer.pixel_buffer_type -> image_data -> unit
    = "ml_glreadpixels_ba_bytecode"
      "ml_glreadpixels_ba_native"
      "noalloc"
(** same than [glReadPixelsBA_unsafe] but checks the size of the big-array *)

(* TODO
void glDrawPixels( GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels );
*)


(** {3 Clipping} *)

module Plane : sig (* PACK_ENUM *)
# 1 "enums/clip_plane.inc.ml" 1
type clip_plane =
  | GL_CLIP_PLANE0
  | GL_CLIP_PLANE1
  | GL_CLIP_PLANE2
  | GL_CLIP_PLANE3
  | GL_CLIP_PLANE4
  | GL_CLIP_PLANE5
# 1276 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glClipPlane: plane:Plane.clip_plane -> equation:float array -> unit = "ml_glclipplane"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClipPlane.xml}
    manual page on opengl.org} *)

external glClipPlane_unsafe: plane:Plane.clip_plane -> equation:float array -> unit = "ml_glclipplane_unsafe"
(** same than [glClipPlane] but doesn't check that [equation] contains 4 items. *)

type clip_plane_i = GL_CLIP_PLANE of int

# 1287 "GL.ml.pp"

val glClipPlanei: plane:clip_plane_i -> equation:float array -> unit
val glClipPlanei_unsafe: plane:clip_plane_i -> equation:float array -> unit

# 1306 "GL.ml.pp"

(* TODO
http://www.opengl.org/sdk/docs/man/xhtml/glGetClipPlane.xml
*)

(** {3 Evaluators} *)

module Map1 : sig (* PACK_ENUM *)
# 1 "enums/map1_target.inc.ml" 1
type map1_target =
  | GL_MAP1_VERTEX_3
  | GL_MAP1_VERTEX_4
  | GL_MAP1_INDEX
  | GL_MAP1_COLOR_4
  | GL_MAP1_NORMAL
  | GL_MAP1_TEXTURE_COORD_1
  | GL_MAP1_TEXTURE_COORD_2
  | GL_MAP1_TEXTURE_COORD_3
  | GL_MAP1_TEXTURE_COORD_4
# 1315 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glMap1: target:Map1.map1_target -> u1:float -> u2:float -> stride:int -> order:int -> points:float array -> unit
         = "ml_glmap1d_bytecode"
           "ml_glmap1d_native"
           "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glMap1.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


module Map2 : sig (* PACK_ENUM *)
# 1 "enums/map2_target.inc.ml" 1
type map2_target =
  | GL_MAP2_VERTEX_3
  | GL_MAP2_VERTEX_4
  | GL_MAP2_INDEX
  | GL_MAP2_COLOR_4
  | GL_MAP2_NORMAL
  | GL_MAP2_TEXTURE_COORD_1
  | GL_MAP2_TEXTURE_COORD_2
  | GL_MAP2_TEXTURE_COORD_3
  | GL_MAP2_TEXTURE_COORD_4
# 1327 "GL.ml.pp" 2
end (* PACK_ENUM *)
external glMap2: target:Map2.map2_target ->
                 u1:float -> u2:float -> ustride:int -> uorder:int ->
                 v1:float -> v2:float -> vstride:int -> vorder:int ->
                 points:float array array array -> unit
         = "ml_glmap2d_bytecode"
           "ml_glmap2d_native"
           "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glMap2.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glEvalCoord1: u:float -> unit = "ml_glevalcoord1d" "noalloc"
external glEvalCoord2: u:float -> v:float -> unit = "ml_glevalcoord2d" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

module EvalMesh1 : sig
type eval_mesh_1 = GL_POINT | GL_LINE
end

module EvalMesh2 : sig
type eval_mesh_2 = GL_POINT | GL_LINE | GL_FILL
end

external glEvalMesh1: mode:EvalMesh1.eval_mesh_1 -> i1:int -> i2:int -> unit = "ml_glevalmesh1" "noalloc"
external glEvalMesh2: mode:EvalMesh2.eval_mesh_2 -> i1:int -> i2:int -> j1:int -> j2:int -> unit = "ml_glevalmesh2" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEvalMesh.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glEvalPoint1: i:int -> unit = "ml_glevalpoint1" "noalloc"
external glEvalPoint2: i:int -> j:int -> unit = "ml_glevalpoint2" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEvalPoint.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


external glMapGrid1: un:int -> u1:float -> u2:float -> unit = "ml_glmapgrid1d" "noalloc"
external glMapGrid2: un:int -> u1:float -> u2:float -> vn:int -> v1:float -> v2:float -> unit
    = "ml_glmapgrid2d_bytecode"
      "ml_glmapgrid2d_native"
      "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)



(** {3 Display Lists} *)

# 1 "enums/list_mode.inc.ml" 1
type list_mode =
  | GL_COMPILE
  | GL_COMPILE_AND_EXECUTE
# 1380 "GL.ml.pp" 2
external glNewList: gl_list:int -> mode:list_mode -> unit = "ml_glnewlist" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glNewList.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glEndList: unit -> unit = "ml_glendlist" "noalloc"
(** @deprecated in core OpenGL 3. *)

external glGenList: unit -> int = "ml_glgenlist" "noalloc"
external glGenLists: range:int -> int = "ml_glgenlists" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGenLists.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glCallList: gl_list:int -> unit = "ml_glcalllist" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCallList.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glCallLists: gl_lists:int array -> unit = "ml_glcalllists" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCallLists.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glDeleteLists: gl_list:int -> range:int -> unit = "ml_gldeletelists" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDeleteLists.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glListBase: base:int -> unit = "ml_gllistbase" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glListBase.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glIsList: gl_list:int -> bool = "ml_glislist" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glIsList.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glGetListMode: unit -> list_mode = "ml_glgetlistmode" "noalloc"
(** @deprecated in core OpenGL 3. *)
 


(** {3 Picking} *)

# 1 "enums/render_mode.inc.ml" 1
type render_mode =
  | GL_RENDER
  | GL_SELECT
  | GL_FEEDBACK
# 1427 "GL.ml.pp" 2
external glRenderMode: mode:render_mode -> int = "ml_glrendermode" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glRenderMode.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glInitNames: unit -> unit = "ml_glinitnames" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glInitNames.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glLoadName: name:int -> unit = "ml_glloadname" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLoadName.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glPushName: name:int -> unit = "ml_glpushname" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPushName.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glPopName: unit -> unit = "ml_glpopname" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glPopName.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)


(* WIP *)

type select_buffer
external new_select_buffer: buffer_size:int -> select_buffer = "ml_alloc_select_buffer" "noalloc"
external free_select_buffer: select_buffer:select_buffer -> unit = "ml_free_select_buffer" "noalloc"
external select_buffer_get: select_buffer:select_buffer -> index:int -> int = "ml_select_buffer_get" "noalloc"

external glSelectBuffer: buffer_size:int -> select_buffer:select_buffer -> unit = "ml_glselectbuffer" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glSelectBuffer.xml}
    manual page on opengl.org}
    @deprecated in core OpenGL 3. *)

external glSelectBufferBA: 
    (*
    ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
    *)
    (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t
    -> unit
    = "ml_glselectbuffer_ba" "noalloc"

(* TODO?
void glFeedbackBuffer( GLsizei size, GLenum type, GLfloat *buffer );  !!! deprecated in core OpenGL 3.2
void glPassThrough( GLfloat token );  !!! deprecated in core OpenGL 3.2
*)


(** {3 Fog} *)

(* {{{ glFog *)

type fog_mode =
  | GL_LINEAR
  | GL_EXP
  | GL_EXP2

type fog_coord_src =
  | GL_FOG_COORD
  | GL_FRAGMENT_DEPTH

type fog_param =
  | GL_FOG_MODE of fog_mode
  | GL_FOG_DENSITY of float
  | GL_FOG_START of float
  | GL_FOG_END of float
  | GL_FOG_INDEX of float
  | GL_FOG_COLOR of (float * float * float * float)
  | GL_FOG_COORD_SRC of fog_coord_src
;;

# 1503 "GL.ml.pp"

val glFog: pname:fog_param -> unit
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml}
    manual page on opengl.org} *)

# 1530 "GL.ml.pp"

(* }}} *)

(* glFogCoord() is deprecated in core OpenGL 3.2 *)


(** {3 GLSL Shaders} *)

type shader_object
type shader_program
type shader_type = GL_VERTEX_SHADER | GL_FRAGMENT_SHADER

external glCreateShader: shader_type:shader_type -> shader_object = "ml_glcreateshader"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCreateShader.xml}
    manual page on opengl.org} *)

external glDeleteShader: shader:shader_object -> unit = "ml_gldeleteshader"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDeleteShader.xml}
    manual page on opengl.org} *)

external glIsShader: shader:shader_object -> bool = "ml_glisshader"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glIsShader.xml}
    manual page on opengl.org} *)

external glShaderSource: shader:shader_object  -> string -> unit = "ml_glshadersource"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glShaderSource.xml}
    manual page on opengl.org} *)

external glCompileShader: shader:shader_object -> unit = "ml_glcompileshader"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCompileShader.xml}
    manual page on opengl.org} *)

external glCreateProgram: unit -> shader_program = "ml_glcreateprogram"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgram.xml}
    manual page on opengl.org} *)

external glDeleteProgram: program:shader_program -> unit = "ml_gldeleteprogram"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgram.xml}
    manual page on opengl.org} *)

external glAttachShader: program:shader_program -> shader:shader_object  -> unit = "ml_glattachshader"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glAttachShader.xml}
    manual page on opengl.org} *)

external glDetachShader: program:shader_program -> shader:shader_object -> unit = "ml_gldetachshader"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDetachShader.xml}
    manual page on opengl.org} *)

external glLinkProgram: program:shader_program -> unit = "ml_gllinkprogram"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgram.xml}
    manual page on opengl.org} *)

external glUseProgram: program:shader_program -> unit = "ml_gluseprogram"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glUseProgram.xml}
    manual page on opengl.org} *)

external glUnuseProgram: unit -> unit = "ml_glunuseprogram"
(** equivalent to the C call glUseProgram(0) that desactivates the program *)

external glGetShaderCompileStatus: shader:shader_object -> bool = "ml_glgetshadercompilestatus"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetShader.xml}
    manual page on opengl.org} *)

external glGetShaderCompileStatus_exn: shader:shader_object -> unit = "ml_glgetshadercompilestatus_exn"
(** same than [glGetShaderCompileStatus] but raises an exception instead of returning false *)

external glGetUniformLocation: program:shader_program -> name:string -> int = "ml_glgetuniformlocation"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocation.xml}
    manual page on opengl.org} *)


type get_program_bool =
  | GL_DELETE_STATUS
  | GL_LINK_STATUS
  | GL_VALIDATE_STATUS

type get_program_int =
  | GL_INFO_LOG_LENGTH
  | GL_ATTACHED_SHADERS
  | GL_ACTIVE_ATTRIBUTES
  | GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
  | GL_ACTIVE_UNIFORMS
  | GL_ACTIVE_UNIFORM_MAX_LENGTH

external glGetProgrami: program:shader_program -> pname:get_program_int -> int = "ml_glgetprogram_int"
external glGetProgramb: program:shader_program -> pname:get_program_bool -> bool = "ml_glgetprogram_bool"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetProgram.xml}
    manual page on opengl.org} *)


external glUniform1f: location:int -> v0:float -> unit = "ml_gluniform1f" "noalloc"
external glUniform2f: location:int -> v0:float -> v1:float -> unit = "ml_gluniform2f" "noalloc"
external glUniform3f: location:int -> v0:float -> v1:float -> v2:float -> unit = "ml_gluniform3f" "noalloc"
external glUniform4f: location:int -> v0:float -> v1:float -> v2:float -> v3:float -> unit = "ml_gluniform4f" "noalloc"
external glUniform1i: location:int -> v0:int -> unit = "ml_gluniform1i" "noalloc"
external glUniform2i: location:int -> v0:int -> v1:int -> unit = "ml_gluniform2i" "noalloc"
external glUniform3i: location:int -> v0:int -> v1:int -> v2:int -> unit = "ml_gluniform3i" "noalloc"
external glUniform4i: location:int -> v0:int -> v1:int -> v2:int -> v3:int -> unit = "ml_gluniform4i" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml}
    manual page on opengl.org} *)

external glUniform1fv: location:int -> value:float array -> unit = "ml_gluniform1fv"
external glUniform2fv: location:int -> count:int -> value:float array -> unit = "ml_gluniform2fv"
external glUniform3fv: location:int -> count:int -> value:float array -> unit = "ml_gluniform3fv"
external glUniform4fv: location:int -> count:int -> value:float array -> unit = "ml_gluniform4fv"
external glUniform1iv: location:int -> value:int array -> unit = "ml_gluniform1iv"
external glUniform2iv: location:int -> count:int -> value:int array -> unit = "ml_gluniform2iv"
external glUniform3iv: location:int -> count:int -> value:int array -> unit = "ml_gluniform3iv"
external glUniform4iv: location:int -> count:int -> value:int array -> unit = "ml_gluniform4iv"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml}
    manual page on opengl.org} *)


external glUniformMatrix2f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix2f"
external glUniformMatrix3f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix3f"
external glUniformMatrix4f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix4f"

external glUniformMatrix2x3f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix2x3f"
external glUniformMatrix3x2f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix3x2f"

external glUniformMatrix2x4f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix2x4f"
external glUniformMatrix4x2f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix4x2f"

external glUniformMatrix3x4f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix3x4f"
external glUniformMatrix4x3f: location:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix4x3f"


external glUniformMatrix2fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix2fv"
external glUniformMatrix3fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix3fv"
external glUniformMatrix4fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix4fv"

external glUniformMatrix2x3fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix2x3fv"
external glUniformMatrix3x2fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix3x2fv"

external glUniformMatrix2x4fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix2x4fv"
external glUniformMatrix4x2fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix4x2fv"

external glUniformMatrix3x4fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix3x4fv"
external glUniformMatrix4x3fv: location:int -> count:int -> transpose:bool -> value:float array -> unit = "ml_gluniformmatrix4x3fv"



external glGetAttribLocation: program:shader_program -> name:string -> int = "ml_glgetattriblocation" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocation.xml}
    manual page on opengl.org} *)

external glBindAttribLocation: program:shader_program -> index:int -> name:string -> unit = "ml_glbindattriblocation" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocation.xml}
    manual page on opengl.org} *)


external glVertexAttrib1s: index:int -> v:int -> unit = "ml_glvertexattrib1s" "noalloc"
external glVertexAttrib1d: index:int -> v:float -> unit = "ml_glvertexattrib1d" "noalloc"

external glVertexAttrib2s: index:int -> v0:int -> v1:int -> unit = "ml_glvertexattrib2s" "noalloc"
external glVertexAttrib2d: index:int -> v0:float -> v1:float -> unit = "ml_glvertexattrib2d" "noalloc"

external glVertexAttrib3s: index:int -> v0:int -> v1:int -> v2:int -> unit = "ml_glvertexattrib3s" "noalloc"
external glVertexAttrib3d: index:int -> v0:float -> v1:float -> v2:float -> unit = "ml_glvertexattrib3d" "noalloc"

external glVertexAttrib4s: index:int -> v0:int -> v1:int -> v2:int -> v3:int -> unit = "ml_glvertexattrib4s" "noalloc"
external glVertexAttrib4d: index:int -> v0:float -> v1:float -> v2:float -> v3:float -> unit = "ml_glvertexattrib4d" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml}
    manual page on opengl.org} *)

external glGetShaderInfoLog: shader:shader_object -> string = "ml_glgetshaderinfolog" (* DOES ALLOC *)
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderInfoLog.xml}
    manual page on opengl.org} *)

external glGetProgramInfoLog: program:shader_program -> string = "ml_glgetprograminfolog" (* DOES ALLOC *)
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramInfoLog.xml}
    manual page on opengl.org} *)

external glEnableVertexAttribArray: index:int -> unit = "ml_glenablevertexattribarray" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml}
    manual page on opengl.org} *)

external glDisableVertexAttribArray: index:int -> unit = "ml_gldisablevertexattribarray" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glDisableVertexAttribArray.xml}
    manual page on opengl.org} *)


(** {3 Gets} *)

module Get : sig (* _PACK_ENUM *)
# 1 "enums/get_boolean_1.inc.ml" 1
type get_boolean_1 =
  | GL_ALPHA_TEST  (** deprecated in core OpenGL 3. *)
  | GL_AUTO_NORMAL
  | GL_BLEND
  | GL_CLIP_PLANE0
  | GL_CLIP_PLANE1
  | GL_CLIP_PLANE2
  | GL_CLIP_PLANE3
  | GL_CLIP_PLANE4
  | GL_CLIP_PLANE5
  | GL_COLOR_ARRAY
  | GL_COLOR_LOGIC_OP
  | GL_COLOR_MATERIAL  (** deprecated in core OpenGL 3. *)
  | GL_COLOR_SUM
  | GL_COLOR_TABLE
  | GL_CONVOLUTION_1D
  | GL_CONVOLUTION_2D
  | GL_CULL_FACE
  | GL_CURRENT_RASTER_POSITION_VALID
  | GL_DEPTH_TEST
  | GL_DEPTH_WRITEMASK
  | GL_DITHER
  | GL_DOUBLEBUFFER
  | GL_EDGE_FLAG
  | GL_EDGE_FLAG_ARRAY
  | GL_FOG
  | GL_FOG_COORD_ARRAY
  | GL_HISTOGRAM
  | GL_INDEX_ARRAY
  | GL_INDEX_LOGIC_OP
  | GL_INDEX_MODE
  | GL_LIGHT0  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT1  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT2  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT3  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT4  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT5  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT6  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT7  (** deprecated in core OpenGL 3. *)
  | GL_LIGHTING  (** deprecated in core OpenGL 3. *)
  | GL_LIGHT_MODEL_LOCAL_VIEWER
  | GL_LIGHT_MODEL_TWO_SIDE
  | GL_LINE_SMOOTH
  | GL_LINE_STIPPLE
  | GL_MAP1_INDEX
  | GL_MAP1_NORMAL
  | GL_MAP1_TEXTURE_COORD_1
  | GL_MAP1_TEXTURE_COORD_2
  | GL_MAP1_TEXTURE_COORD_3
  | GL_MAP1_TEXTURE_COORD_4
  | GL_MAP1_VERTEX_3
  | GL_MAP1_VERTEX_4
  | GL_MAP2_COLOR_4
  | GL_MAP2_INDEX
  | GL_MAP2_NORMAL
  | GL_MAP2_TEXTURE_COORD_1
  | GL_MAP2_TEXTURE_COORD_2
  | GL_MAP2_TEXTURE_COORD_3
  | GL_MAP2_TEXTURE_COORD_4
  | GL_MAP2_VERTEX_3
  | GL_MAP2_VERTEX_4
  | GL_MAP_COLOR
  | GL_MAP_STENCIL
  | GL_MINMAX
  | GL_NORMAL_ARRAY
  | GL_NORMALIZE
  | GL_PACK_SWAP_BYTES
  | GL_POINT_SMOOTH  (** deprecated in core OpenGL 3. *)
  | GL_POINT_SPRITE  (** deprecated in core OpenGL 3. *)
  | GL_POLYGON_OFFSET_FILL
  | GL_POLYGON_OFFSET_LINE
  | GL_POLYGON_OFFSET_POINT
  | GL_POLYGON_SMOOTH
  | GL_POLYGON_STIPPLE
  | GL_POST_COLOR_MATRIX_COLOR_TABLE
  | GL_PACK_LSB_FIRST
  | GL_POST_CONVOLUTION_COLOR_TABLE
  | GL_RESCALE_NORMAL  (** deprecated in core OpenGL 3. *)
  | GL_RGBA_MODE
  | GL_SAMPLE_COVERAGE_INVERT
  | GL_SCISSOR_TEST
  | GL_SECONDARY_COLOR_ARRAY
  | GL_SEPARABLE_2D
  | GL_STENCIL_TEST
  | GL_STEREO
  | GL_TEXTURE_1D
  | GL_TEXTURE_2D
  | GL_TEXTURE_3D
  | GL_TEXTURE_COORD_ARRAY
  | GL_TEXTURE_CUBE_MAP
  | GL_TEXTURE_GEN_Q  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_R  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_S  (** deprecated in core OpenGL 3. *)
  | GL_TEXTURE_GEN_T  (** deprecated in core OpenGL 3. *)
  | GL_UNPACK_LSB_FIRST
  | GL_UNPACK_SWAP_BYTES
  | GL_VERTEX_ARRAY
  | GL_VERTEX_PROGRAM_POINT_SIZE
  | GL_VERTEX_PROGRAM_TWO_SIDE  (** deprecated in core OpenGL 3. *)
  | GL_PACK_ALIGNMENT
  | GL_PACK_IMAGE_HEIGHT
  | GL_PACK_ROW_LENGTH
  | GL_PACK_SKIP_IMAGES
  | GL_PACK_SKIP_PIXELS
  | GL_PACK_SKIP_ROWS
  | GL_UNPACK_IMAGE_HEIGHT
  | GL_UNPACK_ROW_LENGTH
  | GL_UNPACK_SKIP_IMAGES
  | GL_UNPACK_SKIP_PIXELS
  | GL_UNPACK_SKIP_ROWS
# 1716 "GL.ml.pp" 2
# 1 "enums/get_boolean_4.inc.ml" 1
type get_boolean_4 =
  | GL_COLOR_WRITEMASK
# 1717 "GL.ml.pp" 2
# 1 "enums/get_integer_4.inc.ml" 1
type get_integer_4 =
  | GL_VIEWPORT
  | GL_SCISSOR_BOX
# 1718 "GL.ml.pp" 2
# 1 "enums/get_integer_2.inc.ml" 1
type get_integer_2 =
  | GL_MAX_VIEWPORT_DIMS
# 1719 "GL.ml.pp" 2
# 1 "enums/get_integer_1.inc.ml" 1
type get_integer_1 =
  | GL_ACCUM_RED_BITS  (** deprecated in core OpenGL 3. *)
  | GL_ACCUM_GREEN_BITS  (** deprecated in core OpenGL 3. *)
  | GL_ACCUM_BLUE_BITS  (** deprecated in core OpenGL 3. *)
  | GL_ACCUM_ALPHA_BITS  (** deprecated in core OpenGL 3. *)
  | GL_CURRENT_RASTER_INDEX
  | GL_LIST_BASE
  | GL_LIST_INDEX
  | GL_MAX_LIGHTS
  | GL_MAX_LIST_NESTING
  | GL_MAX_TEXTURE_SIZE
  | GL_MAX_TEXTURE_COORDS
  | GL_ACTIVE_TEXTURE
  | GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
  | GL_MAX_TEXTURE_UNITS
  | GL_MAX_ELEMENTS_INDICES
  | GL_MAX_ELEMENTS_VERTICES
  | GL_SUBPIXEL_BITS
  | GL_DEPTH_BITS
  | GL_STENCIL_BITS
  | GL_NAME_STACK_DEPTH
  | GL_COLOR_MATRIX_STACK_DEPTH
  | GL_MODELVIEW_STACK_DEPTH
  | GL_PROJECTION_STACK_DEPTH
  | GL_TEXTURE_STACK_DEPTH
  | GL_MAX_MODELVIEW_STACK_DEPTH
  | GL_MAX_PROJECTION_STACK_DEPTH
  | GL_MAX_TEXTURE_STACK_DEPTH
  | GL_SAMPLE_BUFFERS
  | GL_SAMPLES
  | GL_MAX_CLIP_PLANES
  | GL_AUX_BUFFERS
  | GL_RED_BITS
  | GL_GREEN_BITS
  | GL_BLUE_BITS
  | GL_ALPHA_BITS
  | GL_MAX_3D_TEXTURE_SIZE
  | GL_MAX_CLIENT_ATTRIB_STACK_DEPTH
  | GL_MAX_ATTRIB_STACK_DEPTH
  | GL_MAX_COLOR_MATRIX_STACK_DEPTH
  | GL_MAX_CUBE_MAP_TEXTURE_SIZE
  | GL_MAX_DRAW_BUFFERS
  | GL_MAX_NAME_STACK_DEPTH
  | GL_NORMAL_ARRAY_STRIDE
  | GL_INDEX_ARRAY_STRIDE
  | GL_COLOR_ARRAY_STRIDE
  | GL_EDGE_FLAG_ARRAY_STRIDE
  | GL_TEXTURE_COORD_ARRAY_STRIDE
  | GL_VERTEX_ARRAY_STRIDE
  | GL_SECONDARY_COLOR_ARRAY_STRIDE
  | GL_FOG_COORD_ARRAY_STRIDE
  | GL_SECONDARY_COLOR_ARRAY_SIZE
  | GL_SELECTION_BUFFER_SIZE
  | GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
  | GL_MAX_VERTEX_UNIFORM_COMPONENTS
  | GL_MAX_VERTEX_ATTRIBS
# 1720 "GL.ml.pp" 2
# 1 "enums/get_float_3.inc.ml" 1
type get_float_3 =
  | GL_CURRENT_NORMAL
  | GL_POINT_DISTANCE_ATTENUATION
# 1721 "GL.ml.pp" 2
# 1 "enums/get_float_4.inc.ml" 1
type get_float_4 =
  | GL_ACCUM_CLEAR_VALUE
  | GL_BLEND_COLOR
  | GL_COLOR_CLEAR_VALUE
  | GL_CURRENT_COLOR
  | GL_CURRENT_RASTER_COLOR
  | GL_CURRENT_RASTER_POSITION
  | GL_CURRENT_RASTER_SECONDARY_COLOR
  | GL_CURRENT_RASTER_TEXTURE_COORDS
  | GL_CURRENT_SECONDARY_COLOR
  | GL_CURRENT_TEXTURE_COORDS
  | GL_FOG_COLOR
  | GL_LIGHT_MODEL_AMBIENT
  | GL_MAP2_GRID_DOMAIN
# 1722 "GL.ml.pp" 2
# 1 "enums/get_matrix.inc.ml" 1
type get_matrix =
  | GL_COLOR_MATRIX
  | GL_MODELVIEW_MATRIX
  | GL_PROJECTION_MATRIX
  | GL_TEXTURE_MATRIX
  | GL_TRANSPOSE_COLOR_MATRIX
  | GL_TRANSPOSE_MODELVIEW_MATRIX
  | GL_TRANSPOSE_PROJECTION_MATRIX
  | GL_TRANSPOSE_TEXTURE_MATRIX
# 1723 "GL.ml.pp" 2
# 1 "enums/get_float_1.inc.ml" 1
type get_float_1 =
  | GL_CURRENT_RASTER_DISTANCE
  | GL_FOG_DENSITY
  | GL_FOG_END
  | GL_FOG_INDEX
  | GL_FOG_START
  | GL_INDEX_CLEAR_VALUE
  | GL_LINE_WIDTH
  | GL_LINE_WIDTH_GRANULARITY
  | GL_POINT_SIZE
  | GL_STENCIL_CLEAR_VALUE
  | GL_DEPTH_CLEAR_VALUE
  | GL_SAMPLE_COVERAGE_VALUE
  | GL_ZOOM_X
  | GL_ZOOM_Y
  | GL_ALPHA_SCALE
  | GL_BLUE_SCALE
  | GL_DEPTH_SCALE
  | GL_GREEN_SCALE
  | GL_RED_SCALE
  | GL_POINT_SIZE_GRANULARITY
  | GL_POINT_SIZE_MAX
  | GL_POINT_SIZE_MIN
  | GL_POINT_FADE_THRESHOLD_SIZE
  | GL_CURRENT_INDEX
  | GL_POLYGON_OFFSET_FACTOR
  | GL_POLYGON_OFFSET_UNITS
# 1724 "GL.ml.pp" 2
# 1 "enums/get_float_2.inc.ml" 1
type get_float_2 =
  | GL_LINE_WIDTH_RANGE
  | GL_POINT_SIZE_RANGE
  | GL_DEPTH_RANGE
# 1725 "GL.ml.pp" 2
# 1 "enums/get_texture_binding.inc.ml" 1
type get_texture_binding =
  | GL_TEXTURE_BINDING_1D
  | GL_TEXTURE_BINDING_2D
  | GL_TEXTURE_BINDING_3D
  | GL_TEXTURE_BINDING_CUBE_MAP
# 1726 "GL.ml.pp" 2

type get_light =
  | GL_SPOT_EXPONENT
  | GL_SPOT_CUTOFF
  | GL_CONSTANT_ATTENUATION
  | GL_LINEAR_ATTENUATION
  | GL_QUADRATIC_ATTENUATION
  | GL_SPOT_DIRECTION
  | GL_AMBIENT
  | GL_DIFFUSE
  | GL_SPECULAR
  | GL_POSITION
  | GL_LIGHT_MODEL_COLOR_CONTROL
end (* _PACK_ENUM *)

(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGet.xml}
    manual page on opengl.org} *)

external glGetBoolean1: Get.get_boolean_1 -> bool = "ml_glgetboolean1" "noalloc"
external glGetBoolean4: Get.get_boolean_4 -> bool * bool * bool * bool = "ml_glgetboolean4" (* DOES ALLOC *)
external glGetInteger4: Get.get_integer_4 -> int * int * int * int = "ml_glgetinteger4" (* DOES ALLOC *)
external glGetInteger1: Get.get_integer_1 -> int = "ml_glgetinteger1" "noalloc"
external glGetInteger2: Get.get_integer_2 -> int * int = "ml_glgetinteger2" (* DOES ALLOC *)
external glGetFloat4: Get.get_float_4 -> float * float * float * float = "ml_glgetfloat4" (* DOES ALLOC *)
external glGetFloat3: Get.get_float_3 -> float * float * float = "ml_glgetfloat3" (* DOES ALLOC *)
external glGetFloat2: Get.get_float_2 -> float * float = "ml_glgetfloat2" (* DOES ALLOC *)
external glGetFloat1: Get.get_float_1 -> float = "ml_glgetfloat1" (* DOES ALLOC *)
external glGetMatrix: Get.get_matrix -> float array array = "ml_glgetmatrix" (* DOES ALLOC *)
external glGetMatrixFlat: Get.get_matrix -> float array = "ml_glgetmatrix_flat" (* DOES ALLOC *)
external glGetTextureBinding: Get.get_texture_binding -> texture_id = "ml_glgettexturebinding" "noalloc"

# 1 "enums/get_string.inc.ml" 1
type get_string =
  | GL_VENDOR
  | GL_RENDERER
  | GL_VERSION
  | GL_SHADING_LANGUAGE_VERSION
  | GL_EXTENSIONS
# 1758 "GL.ml.pp" 2
external glGetString: name:get_string -> string = "ml_glgetstring" (* DOES ALLOC *)
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetString.xml}
    manual page on opengl.org} *)

# 1 "enums/gl_error.inc.ml" 1
type gl_error =
  | GL_NO_ERROR
  | GL_INVALID_ENUM
  | GL_INVALID_VALUE
  | GL_INVALID_OPERATION
  | GL_STACK_OVERFLOW
  | GL_STACK_UNDERFLOW
  | GL_OUT_OF_MEMORY
  | GL_TABLE_TOO_LARGE
# 1763 "GL.ml.pp" 2
external glGetError: unit -> gl_error = "ml_glgeterror" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetError.xml}
    manual page on opengl.org} *)


(* perhaps remove tuple_params ? *)

(* {{{ glGetLight *)

type tuple_params =
  | P1 of float
  | P3 of float * float * float
  | P4 of float * float * float * float
  | PCC of color_control

# 1779 "GL.ml.pp"

val glGetLight: light:gl_light -> pname:Get.get_light -> tuple_params
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glGetLight.xml}
    manual page on opengl.org} *)

# 1816 "GL.ml.pp"
(* }}} *)


(* {{{ Multitexture *)
(** {3 Multitexture} *)

(** {{:http://www.opengl.org/resources/code/samples/sig99/advanced99/notes/node60.html}
    Multitexture Node},
    {{:http://www.opengl.org/wiki/GL_ARB_multitexture}
    ARB multitexture wiki}
*)

# 1 "enums/texture_i.inc.ml" 1
type texture_i =
  | GL_TEXTURE0
  | GL_TEXTURE1
  | GL_TEXTURE2
  | GL_TEXTURE3
  | GL_TEXTURE4
  | GL_TEXTURE5
  | GL_TEXTURE6
  | GL_TEXTURE7
  | GL_TEXTURE8
  | GL_TEXTURE9
  | GL_TEXTURE10
  | GL_TEXTURE11
  | GL_TEXTURE12
  | GL_TEXTURE13
  | GL_TEXTURE14
  | GL_TEXTURE15
  | GL_TEXTURE16
  | GL_TEXTURE17
  | GL_TEXTURE18
  | GL_TEXTURE19
  | GL_TEXTURE20
  | GL_TEXTURE21
  | GL_TEXTURE22
  | GL_TEXTURE23
  | GL_TEXTURE24
  | GL_TEXTURE25
  | GL_TEXTURE26
  | GL_TEXTURE27
  | GL_TEXTURE28
  | GL_TEXTURE29
  | GL_TEXTURE30
  | GL_TEXTURE31
# 1829 "GL.ml.pp" 2
external glActiveTexture: texture:texture_i -> unit = "ml_glactivetexture" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glActiveTexture.xml}
    manual page on opengl.org} *)

external glActiveTexturei: texture:int -> unit = "ml_glactivetexture_i" "noalloc"
(** [glActiveTexturei i] is equivalent to [glActiveTexture GL_TEXTUREi] *)

external glClientActiveTexture: texture:texture_i -> unit = "ml_glclientactivetexture" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glClientActiveTexture.xml}
    manual page on opengl.org} *)

external glMultiTexCoord2: texture:texture_i -> s:float -> t:float -> unit = "ml_glmultitexcoord2d" "noalloc"
(** {{:http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml}
    manual page on opengl.org} *)

external glMultiTexCoord2i: texture:int -> s:float -> t:float -> unit = "ml_glmultitexcoord2f_i"

(* }}} *)
(* TODO glMultiTexCoord(3|4) *)

(** {3 Library Version} *)

# 1852 "GL.ml.pp"
val glmlite_version : int * int * int
# 1857 "GL.ml.pp"

(* vim: sw=2 sts=2 ts=2 et fdm=marker filetype=ocaml nowrap
 *)
