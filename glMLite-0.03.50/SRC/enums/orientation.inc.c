  static const GLenum conv_orientation_table[] = {
    GL_CW,
    GL_CCW,
  };
  orientation = conv_orientation_table[Int_val(_orientation)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (orientation == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
