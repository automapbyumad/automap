  static const GLenum conv_blend_dfactor_table[] = {
    GL_ZERO,
    GL_ONE,
    GL_SRC_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_ALPHA,
  };
  blend_dfactor = conv_blend_dfactor_table[Int_val(_blend_dfactor)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (blend_dfactor == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
