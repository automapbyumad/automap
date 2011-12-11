  static const GLenum conv_min_filter_table[] = {
    GL_NEAREST,
    GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR,
  };
  min_filter = conv_min_filter_table[Int_val(_min_filter)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (min_filter == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
