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

(** Bindings to the GLE library. A set of functions to make extrusions. *)

(** GLE is a library that draws extruded surfaces, including surfaces of
    revolution, sweeps, tubes, polycones, polycylinders and helicoids.
    Generically, the extruded surface is specified with a 2D polyline that
    is extruded along a 3D path.  A local coordinate system allows for
    additional flexibility in the primitives drawn.  Extrusions may be
    texture mapped in a variety of ways.  The GLE library generates 3D
    triangle coordinates, lighting normal vectors and texture coordinates
    as output. *)

type join_style =
  | TUBE_JN_RAW
  | TUBE_JN_ANGLE
  | TUBE_JN_CUT
  | TUBE_JN_ROUND
  | TUBE_JN_CAP
  | TUBE_NORM_FACET
  | TUBE_NORM_EDGE
  | TUBE_NORM_PATH_EDGE
  | TUBE_CONTOUR_CLOSED

external gleSetJoinStyle: join_style list -> unit = "ml_glesetjoinstyle"
external gleGetJoinStyle: unit -> join_style list = "ml_glegetjoinstyle"
(** control join style of the tubes *)

external gleDestroyGC : unit -> unit = "ml_gledestroygc"
(** clean up global memory usage *)


# 66 "GLE.ml.pp"


# 69 "GLE.ml.pp"
(*
val which_float: unit -> which_ba
(** This function tells you which kind of bigarray to use. *)
*)
# 82 "GLE.ml.pp"


type gle_float
# 86 "GLE.ml.pp"
(*
val cast_ba1:
      (float, 'a, Bigarray.c_layout) Bigarray.Array1.t ->
      (float, gle_float, Bigarray.c_layout) Bigarray.Array1.t
val cast_ba2:
      (float, 'a, Bigarray.c_layout) Bigarray.Array2.t ->
      (float, gle_float, Bigarray.c_layout) Bigarray.Array2.t
(** Provides a way to keep the code generic for any kind of floats. *)
*)
# 104 "GLE.ml.pp"




# 109 "GLE.ml.pp"

val ba2_float32_of_array :
      float array array ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
(** identical to [Bigarray.Array2.of_array Bigarray.float32 Bigarray.c_layout array] *)

val ba2_float32_create :
      int -> int ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
(** identical to [Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout dim1 dim2] *)

val ba2_glefloat_of_array :
      float array array ->
      (float, gle_float, Bigarray.c_layout) Bigarray.Array2.t
(** identical to [ba2_float32_of_array] but with [gle_float] *)

val ba2_glefloat_create :
      int -> int ->
      (float, gle_float, Bigarray.c_layout) Bigarray.Array2.t
(** identical to [ba2_float32_create] but with [gle_float] *)

val ba1_glefloat_of_array :
      float array ->
      (float, gle_float, Bigarray.c_layout) Bigarray.Array1.t

val ba1_glefloat_create :
      int ->
      (float, gle_float, Bigarray.c_layout) Bigarray.Array1.t

# 168 "GLE.ml.pp"



# 172 "GLE.ml.pp"
val colors_none : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
# 177 "GLE.ml.pp"



# 181 "GLE.ml.pp"
val glePolyCylinder: 
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        radius:float -> unit
(** draw polyclinder, specified as a polyline
    @param  points  polyline vertices
    @param  colors  colors at polyline verts
    @param  radius       radius of polycylinder
*)
# 213 "GLE.ml.pp"




# 218 "GLE.ml.pp"
val glePolyCone:
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        radii:(float, gle_float, Bigarray.c_layout) Bigarray.Array1.t ->
        unit
(** draw polycone, specified as a polyline with radii
    @param  points   polyline vertices
    @param  colors   colors at polyline verts
    @param  radii  cone radii at polyline verts
*)
# 255 "GLE.ml.pp"



# 259 "GLE.ml.pp"
val glePolyCone_c4f:
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        radii:(float, gle_float, Bigarray.c_layout) Bigarray.Array1.t ->
        unit
(** same than [glePolyCone] but with RGBA colors *)
# 292 "GLE.ml.pp"





# 298 "GLE.ml.pp"

val gleExtrusion:
        contour:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        cont_normals:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        up:(float * float * float) option ->
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        unit
(** extrude arbitrary 2D contour along arbitrary 3D path
   @param  contour       2D contour
   @param  cont_normals  2D contour normals
   @param  up            up vector for contour
   @param  points        polyline vertices
   @param  colors        colors at polyline verts
*)
# 360 "GLE.ml.pp"




# 365 "GLE.ml.pp"

val gleTwistExtrusion:
        contour:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        cont_normals:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        up:(float * float * float) option ->
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        twist:(float, gle_float, Bigarray.c_layout) Bigarray.Array1.t ->
        unit
(** extrude 2D contour, specifying local rotations (twists)

    @param  contour       2D contour
    @param  cont_normal   2D contour normals
    @param  up            up vector for contour 
    @param  point_array   polyline vertices
    @param  color_array   color at polyline verts
    @param  twist_array   countour twists (in degrees)
*)
# 436 "GLE.ml.pp"



(* vim: sw=2 sts=2 ts=2 et fdm=marker filetype=ocaml
 *)
