  static const GLenum conv_blend_mode_table[] = {
    GL_FUNC_ADD,
    GL_FUNC_SUBTRACT,
    GL_FUNC_REVERSE_SUBTRACT,
    GL_MIN,
    GL_MAX,
  };
  blend_mode = conv_blend_mode_table[Int_val(_blend_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (blend_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
