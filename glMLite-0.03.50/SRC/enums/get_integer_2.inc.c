  static const GLenum conv_get_integer_2_table[] = {
    GL_MAX_VIEWPORT_DIMS,
  };
  get_integer_2 = conv_get_integer_2_table[Int_val(_get_integer_2)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_integer_2 == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
