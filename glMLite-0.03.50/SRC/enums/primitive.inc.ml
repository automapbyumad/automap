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
