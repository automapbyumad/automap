  static const GLenum conv_polygon_mode_table[] = {
    GL_POINT,
    GL_LINE,
    GL_FILL,
  };
  polygon_mode = conv_polygon_mode_table[Int_val(_polygon_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (polygon_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
