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


# 57 "GLE.ml.pp"
(* ML *)
(** Depending on how the lib-gle was compiled, to use C floats or doubles,
    you will have to adapt the [Bigarray.kind] to [Bigarray.float32] or
    [Bigarray.float64] for the point coordinates.
    The function [which_float] will tell you which one to use. *)
type which_ba =
  | BA_float32  (** you have to use [Bigarray.float32] *)
  | BA_float64  (** you have to use [Bigarray.float64] *)
# 66 "GLE.ml.pp"


# 74 "GLE.ml.pp"
(* ML *)
external sizeof_gleDouble: unit -> int = "sizeof_gledouble"
let which_float () =
  match sizeof_gleDouble() with
  | 4 -> BA_float32
  | 8 -> BA_float64
  | _ -> assert(false)
# 82 "GLE.ml.pp"


type gle_float
# 96 "GLE.ml.pp"
(* ML *)
let cast_ba1
      (a :( (float, 'a, Bigarray.c_layout) Bigarray.Array1.t) ) =
      (Obj.magic a :( (float, gle_float, Bigarray.c_layout) Bigarray.Array1.t) )
let cast_ba2
      (a :( (float, 'a, Bigarray.c_layout) Bigarray.Array2.t) ) =
      (Obj.magic a :( (float, gle_float, Bigarray.c_layout) Bigarray.Array2.t) )
# 104 "GLE.ml.pp"




# 139 "GLE.ml.pp"
(* ML *)

let ba2_float32_of_array =
  Bigarray.Array2.of_array Bigarray.float32 Bigarray.c_layout ;;

let ba2_float32_create =
  Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout ;;

let ba2_glefloat_of_array arr =
  match which_float() with
  | BA_float32 -> cast_ba2(Bigarray.Array2.of_array Bigarray.float32 Bigarray.c_layout arr)
  | BA_float64 -> cast_ba2(Bigarray.Array2.of_array Bigarray.float64 Bigarray.c_layout arr)

let ba2_glefloat_create dim1 dim2 =
  match which_float() with
  | BA_float32 -> cast_ba2(Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout dim1 dim2)
  | BA_float64 -> cast_ba2(Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout dim1 dim2)

let ba1_glefloat_of_array arr =
  match which_float() with
  | BA_float32 -> cast_ba1(Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout arr)
  | BA_float64 -> cast_ba1(Bigarray.Array1.of_array Bigarray.float64 Bigarray.c_layout arr)

let ba1_glefloat_create dim =
  match which_float() with
  | BA_float32 -> cast_ba1(Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout dim)
  | BA_float64 -> cast_ba1(Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout dim)

# 168 "GLE.ml.pp"



# 174 "GLE.ml.pp"
(* ML *)
let colors_none = Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout 0 3
# 177 "GLE.ml.pp"



# 191 "GLE.ml.pp"
(* ML *)

external glePolyCylinder: npoints:int ->
    points:(float, 'a, Bigarray.c_layout) Bigarray.Array2.t ->
    colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
    radius:float -> unit = "ml_glepolycylinder"

let glePolyCylinder ~points ~colors ~radius =
  let point_components = Bigarray.Array2.dim2 points in
  if point_components <> 3 then
    invalid_arg "glePolyCylinder: should be 3 coordinates per point";
  let color_components = Bigarray.Array2.dim2 colors in
  if color_components <> 3 then
    invalid_arg "glePolyCylinder: should be 3 components per color";
  let npoints = Bigarray.Array2.dim1 points
  and ncolors = Bigarray.Array2.dim1 colors in
  if npoints <> ncolors then
    invalid_arg "glePolyCylinder: should have the same number of points and colors";
  glePolyCylinder ~npoints ~points ~colors ~radius;
;;

# 213 "GLE.ml.pp"




# 229 "GLE.ml.pp"
(* ML *)

external glePolyCone: npoints:int ->
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        radii:(float, gle_float, Bigarray.c_layout) Bigarray.Array1.t ->
        unit = "ml_glepolycone"

let glePolyCone ~points ~colors ~radii =
  let point_components = Bigarray.Array2.dim2 points in
  if point_components <> 3 then
    invalid_arg "glePolyCone: should be 3 coordinates per point";
  let color_components = Bigarray.Array2.dim2 colors in
  if color_components <> 3 then
    invalid_arg "glePolyCone: should be 3 components per color";
  let npoints = Bigarray.Array2.dim1 points
  and ncolors = Bigarray.Array2.dim1 colors
  and nradius = Bigarray.Array1.dim radii in
  if npoints <> ncolors && ncolors <> 0 then
    invalid_arg "glePolyCone: should have the same number of points and colors";
  if npoints <> nradius then
    invalid_arg "glePolyCone: should have the same number of points and radius";
  glePolyCone ~npoints ~points ~colors ~radii;
;;

# 255 "GLE.ml.pp"



# 266 "GLE.ml.pp"
(* ML *)

external glePolyCone_c4f: npoints:int ->
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        radii:(float, gle_float, Bigarray.c_layout) Bigarray.Array1.t ->
        unit = "ml_glepolycone_c4f"

let glePolyCone_c4f ~points ~colors ~radii =
  let point_components = Bigarray.Array2.dim2 points in
  if point_components <> 3 then
    invalid_arg "glePolyCone_c4f: should be 3 coordinates per point";
  let color_components = Bigarray.Array2.dim2 colors in
  if color_components <> 4 then
    invalid_arg "glePolyCone_c4f: should be 4 components per color";
  let npoints = Bigarray.Array2.dim1 points
  and ncolors = Bigarray.Array2.dim1 colors
  and nradius = Bigarray.Array1.dim radii in
  if npoints <> ncolors && ncolors <> 0 then
    invalid_arg "glePolyCone_c4f: should have the same number of points and colors";
  if npoints <> nradius then
    invalid_arg "glePolyCone_c4f: should have the same number of points and radius";
  glePolyCone_c4f ~npoints ~points ~colors ~radii;
;;

# 292 "GLE.ml.pp"





# 314 "GLE.ml.pp"
(* ML *)

external gleExtrusion:
        ncp:int ->
        contour:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        cont_normals:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        up:(float * float * float) option ->
        npoints:int ->
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t -> unit
        = "ml_gleextrusion_bytecode"
          "ml_gleextrusion"

let gleExtrusion ~contour ~cont_normals ~up ~points ~colors =

  let contour_components = Bigarray.Array2.dim2 contour in
  if contour_components <> 2 then
    invalid_arg "gleExtrusion: should be 2 components per contour";

  let contnorm_components = Bigarray.Array2.dim2 cont_normals in
  if contnorm_components <> 2 then
    invalid_arg "gleExtrusion: should be 2 components per contour normal";

  let point_components = Bigarray.Array2.dim2 points in
  if point_components <> 3 then
    invalid_arg "gleExtrusion: should be 3 coordinates per point";

  let color_components = Bigarray.Array2.dim2 colors in
  if color_components <> 3 then
    invalid_arg "gleExtrusion: should be 3 components per color";

  let ncp = Bigarray.Array2.dim1 contour
  and ncontnm = Bigarray.Array2.dim1 cont_normals
  and npoints = Bigarray.Array2.dim1 points
  and ncolors = Bigarray.Array2.dim1 colors
  in
  if ncp <> ncontnm then
    invalid_arg "gleExtrusion: bigarrays should contain the same number of coordinates";

  if npoints <> ncolors && ncolors <> 0 then
    invalid_arg "gleExtrusion: should have the same number of points and colors";

  gleExtrusion ~ncp ~contour ~cont_normals ~up ~npoints ~points ~colors;
;;

# 360 "GLE.ml.pp"




# 384 "GLE.ml.pp"
(* ML *)

external gleTwistExtrusion:
        ncp:int ->
        contour:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        cont_normals:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        up:(float * float * float) option ->
        npoints:int ->
        points:(float, gle_float, Bigarray.c_layout) Bigarray.Array2.t ->
        colors:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
        twist:(float, gle_float, Bigarray.c_layout) Bigarray.Array1.t ->
        unit
        = "ml_gletwistextrusion_bytecode"
          "ml_gletwistextrusion"

let gleTwistExtrusion ~contour ~cont_normals ~up ~points ~colors ~twist =

  let contour_components = Bigarray.Array2.dim2 contour in
  if contour_components <> 2 then
    invalid_arg "gleExtrusion: should be 2 components per contour";

  let contnorm_components = Bigarray.Array2.dim2 cont_normals in
  if contnorm_components <> 2 then
    invalid_arg "gleExtrusion: should be 2 components per contour normal";

  let point_components = Bigarray.Array2.dim2 points in
  if point_components <> 3 then
    invalid_arg "gleExtrusion: should be 3 coordinates per point";

  let color_components = Bigarray.Array2.dim2 colors in
  if color_components <> 3 then
    invalid_arg "gleExtrusion: should be 3 components per color";

  let ncp = Bigarray.Array2.dim1 contour
  and ncontnm = Bigarray.Array2.dim1 cont_normals
  and npoints = Bigarray.Array2.dim1 points
  and ncolors = Bigarray.Array2.dim1 colors
  and ntwists = Bigarray.Array1.dim twist
  in
  if ncp <> ncontnm then
    invalid_arg "gleExtrusion: bigarrays should contain the same number of coordinates";

  if npoints <> ncolors && ncolors <> 0 then
    invalid_arg "gleExtrusion: should have the same number of points and colors";

  if npoints <> ntwists then
    invalid_arg "gleExtrusion: should have the same number of points and twists";

  gleTwistExtrusion ~ncp ~contour ~cont_normals ~up ~npoints ~points ~colors ~twist;
;;

# 436 "GLE.ml.pp"



(* vim: sw=2 sts=2 ts=2 et fdm=marker filetype=ocaml
 *)
