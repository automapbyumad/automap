(* {{{ COPYING *(

  +-----------------------------------------------------------------------+
  |  This file belongs to glMLite, an OCaml binding to the OpenGL API.    |
  +-----------------------------------------------------------------------+
  |  Copyright (C) 2006, 2007, 2008  Florent Monnier                      |
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

(** An {i experimental} attempt at a functional interface to OpenGL. *)

(** This module tries to be a functional wrapper around OpenGL,
    so you don't have to think about the side effects and restoring
    the previous gl state when using these functions.
    You can set parameters for a local effect.
*)

(** {3 Types} *)

type vertex2 = float * float  (** (x,y) *)
type vertex3 = float * float * float  (** (x,y,z) *)
type vertex4 = float * float * float * float  (** (x,y,z,w) *)
type vector = float * float * float
type rgb = float * float * float
type rgba = float * float * float * float
type uv = float * float
type matrix4x4 = float array


(** {3 Replacement Functions} *)

# 48 "FunGL.ml.pp"

val draw_translated: vector -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glTranslate} *)

val draw_rotated: float -> vector -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glRotate} *)

val draw_scaled: vector -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glScale} *)

val draw_as_identity: (unit -> unit) -> unit
(** use this function as replacement of {!GL.glLoadIdentity} *)

val draw_with_matrix: matrix4x4 -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glMultMatrix}/Flat *)

# 112 "FunGL.ml.pp"



# 116 "FunGL.ml.pp"

val draw_with_rgb: rgb -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glColor3} *)

val draw_with_rgba: rgba -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glColor4} *)

# 145 "FunGL.ml.pp"



# 149 "FunGL.ml.pp"
val draw_with_material : face:GL.face_mode -> mode:GL.Material.material_mode -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glMaterial} *)
# 162 "FunGL.ml.pp"



# 166 "FunGL.ml.pp"
val draw_with_lightModel : light_model:GL.light_model -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glLightModel} *)
# 179 "FunGL.ml.pp"



# 183 "FunGL.ml.pp"
val draw_with_shadeModel : shade_mode:GL.shade_mode -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glShadeModel} *)
# 196 "FunGL.ml.pp"


(*
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/colormaterial.html
  glIsEnabled with argument GL_COLOR_MATERIAL
  glGet with argument GL_COLOR_MATERIAL_PARAMETER
  glGet with argument GL_COLOR_MATERIAL_FACE
  if (!glIsEnabled( GL_COLOR_MATERIAL )) {
      ptr = NULL;
    }
  if (ptr != NULL) {
      restore(face, material_param);
    }
*)



# 214 "FunGL.ml.pp"
val draw_with_frontFace : orientation:GL.orientation -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glFrontFace} *)
# 227 "FunGL.ml.pp"




# 232 "FunGL.ml.pp"
val draw_with_cullFace : mode:GL.face_mode -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glCullFace} *)
# 245 "FunGL.ml.pp"



(*
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/frustum.html

   associated gets:
        glGet with argument GL_MATRIX_MODE (
          glGet with argument GL_MODELVIEW_MATRIX
          glGet with argument GL_PROJECTION_MATRIX
          glGet with argument GL_TEXTURE_MATRIX
        )

val glFrustum :
       left:float -> right:float ->
       bottom:float -> top:float ->
       near:float -> far:float -> unit
*)


(*
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/depthrange.html
glDepthRange ???

   associated gets:
        glGet with argument GL_DEPTH_RANGE

val glDepthRange : near_val:float -> far_val:float -> unit
*)

(*
glColorMask
*)



# 282 "FunGL.ml.pp"
val draw_enabled: cap:GL.gl_capability -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glEnable} *)
# 295 "FunGL.ml.pp"



# 299 "FunGL.ml.pp"
val draw_disabled: cap:GL.gl_capability -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glDisable} *)
# 312 "FunGL.ml.pp"



# 316 "FunGL.ml.pp"
val draw_with_viewport : viewport:int * int * int * int -> (unit -> unit) -> unit
(** use this function instead of {!GL.glViewport} when you use a local viewport *)
# 329 "FunGL.ml.pp"



# 333 "FunGL.ml.pp"
val draw_with_polygonMode : face:GL.face_mode -> mode:GL.polygon_mode -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glPolygonMode} *)
# 346 "FunGL.ml.pp"



# 350 "FunGL.ml.pp"
val draw_with_polygonMode2 : front:GL.polygon_mode -> back:GL.polygon_mode -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glPolygonMode} *)
# 363 "FunGL.ml.pp"



# 367 "FunGL.ml.pp"
val do_with_matrixMode : mode:GL.matrix_mode -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glMatrixMode} *)
# 380 "FunGL.ml.pp"



# 384 "FunGL.ml.pp"

val draw_with_lineWidth: width:float -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glLineWidth} *)

val draw_with_pointSize: size:float -> (unit -> unit) -> unit
(** use this function as replacement of {!GL.glPointSize} *)

# 416 "FunGL.ml.pp"
(*
 glGet with argument GL_LINE_WIDTH
 glGet with argument GL_POINT_SIZE

http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/linewidth.html
val glLineWidth : width:float -> unit

http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/pointsize.html
val glPointSize : size:float -> unit
*)


(*
  http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml
  glIsEnabled with argument GL_POLYGON_OFFSET_FILL, GL_POLYGON_OFFSET_LINE,
  or GL_POLYGON_OFFSET_POINT.
  glGet with argument GL_POLYGON_OFFSET_FACTOR or GL_POLYGON_OFFSET_UNITS.
  val glPolygonOffset : factor:float -> units:float -> unit
*)


(*
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/texparameter.html
*)




(*
# 446 "FunGL.ml.pp"
val draw_with_texParameter : 
      target:GL.TexParam.tex_param_target ->
      pname:GL.TexParam.tex_param_pname ->
      param:GL.TexParam.tex_parameter ->
      (unit -> unit) -> unit
(** use this function as replacement of {!GL.glTexParameter} *)
# 463 "FunGL.ml.pp"
          glGetTexParameter
          glGetTexLevelParameter
      http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml
      http://www.opengl.org/sdk/docs/man/xhtml/glGetTexLevelParameter.xml
*)




(*
val glNewList: gl_list:int -> mode:list_mode -> unit
val glEndList: unit -> unit
val glGenLists: range:int -> int
val glCallList: gl_list:int -> unit
*)


(*
val glRenderMode : mode:render_mode -> int
val glInitNames : unit -> unit
val glLoadName : name:int -> unit
val glPushName : name:int -> unit
val glPopName : unit -> unit
val glSelectBufferBA
*)


(*
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/fog.html
val glFog : pname:fog_param -> unit
*)


(*
glClearStencil ???
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/clearstencil.html
*)


(*
http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/ortho.html
glOrtho
 with check if matrix mode is the good one
*)


# 510 "FunGL.ml.pp"
val draw_using_program: program:GL.shader_program -> (unit -> unit) -> unit
(**  *)
# 523 "FunGL.ml.pp"


(** {3 Drawing} *)


type qualified_vertices =
  | Vertices2 of vertex2 list
  | Vertices3 of vertex3 list
  | Vertices4 of vertex4 list
  | Normal_Vertices2 of (vector * vertex2) list
  | Normal_Vertices3 of (vector * vertex3) list
  | Normal_Vertices4 of (vector * vertex4) list
 
  | RGB_Vertices2 of (rgb * vertex2) list
  | RGB_Vertices3 of (rgb * vertex3) list
  | RGB_Vertices4 of (rgb * vertex4) list
  | RGBA_Vertices2 of (rgba * vertex2) list
  | RGBA_Vertices3 of (rgba * vertex3) list
  | RGBA_Vertices4 of (rgba * vertex4) list
 
  | Normal_RGB_Vertices2 of (vector * rgb * vertex2) list
  | Normal_RGB_Vertices3 of (vector * rgb * vertex3) list
  | Normal_RGB_Vertices4 of (vector * rgb * vertex4) list
  | Normal_RGBA_Vertices2 of (vector * rgba * vertex2) list
  | Normal_RGBA_Vertices3 of (vector * rgba * vertex3) list
  | Normal_RGBA_Vertices4 of (vector * rgba * vertex4) list
 
  (* the same but with UV *)
  | UV_Vertices2 of (uv * vertex2) list
  | UV_Vertices3 of (uv * vertex3) list
  | UV_Vertices4 of (uv * vertex4) list
 
  | UV_Normal_Vertices2 of (uv * vector * vertex2) list
  | UV_Normal_Vertices3 of (uv * vector * vertex3) list
  | UV_Normal_Vertices4 of (uv * vector * vertex4) list
 
  | UV_RGB_Vertices2 of (uv * rgb * vertex2) list
  | UV_RGB_Vertices3 of (uv * rgb * vertex3) list
  | UV_RGB_Vertices4 of (uv * rgb * vertex4) list
  | UV_RGBA_Vertices2 of (uv * rgba * vertex2) list
  | UV_RGBA_Vertices3 of (uv * rgba * vertex3) list
  | UV_RGBA_Vertices4 of (uv * rgba * vertex4) list
 
  | UV_Normal_RGB_Vertices2 of (uv * vector * rgb * vertex2) list
  | UV_Normal_RGB_Vertices3 of (uv * vector * rgb * vertex3) list
  | UV_Normal_RGB_Vertices4 of (uv * vector * rgb * vertex4) list
  | UV_Normal_RGBA_Vertices2 of (uv * vector * rgba * vertex2) list
  | UV_Normal_RGBA_Vertices3 of (uv * vector * rgba * vertex3) list
  | UV_Normal_RGBA_Vertices4 of (uv * vector * rgba * vertex4) list

# 574 "FunGL.ml.pp"

val render_primitive: GL.primitive -> qualified_vertices -> unit
(** render the given list of qualified vertices as the required primitive *)

# 675 "FunGL.ml.pp"




(* vim: sw=2 sts=2 ts=2 et fdm=marker filetype=ocaml nowrap
 *)
