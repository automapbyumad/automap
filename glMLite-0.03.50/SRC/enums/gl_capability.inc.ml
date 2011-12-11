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
