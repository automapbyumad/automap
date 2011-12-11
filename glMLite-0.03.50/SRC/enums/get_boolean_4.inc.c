  static const GLenum conv_get_boolean_4_table[] = {
    GL_COLOR_WRITEMASK,
  };
  get_boolean_4 = conv_get_boolean_4_table[Int_val(_get_boolean_4)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_boolean_4 == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
