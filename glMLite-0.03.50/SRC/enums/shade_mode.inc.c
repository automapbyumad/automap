  static const GLenum conv_shade_mode_table[] = {
    GL_FLAT,
    GL_SMOOTH,
  };
  shade_mode = conv_shade_mode_table[Int_val(_shade_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (shade_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
