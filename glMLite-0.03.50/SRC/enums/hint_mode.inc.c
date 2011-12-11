  static const GLenum conv_hint_mode_table[] = {
    GL_FASTEST,
    GL_NICEST,
    GL_DONT_CARE,
  };
  hint_mode = conv_hint_mode_table[Int_val(_hint_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (hint_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
