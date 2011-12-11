  static const GLenum conv_get_float_2_table[] = {
    GL_LINE_WIDTH_RANGE,
    GL_POINT_SIZE_RANGE,
    GL_DEPTH_RANGE,
  };
  get_float_2 = conv_get_float_2_table[Int_val(_get_float_2)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_float_2 == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
