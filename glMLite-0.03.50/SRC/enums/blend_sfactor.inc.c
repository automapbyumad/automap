  static const GLenum conv_blend_sfactor_table[] = {
    GL_ZERO,
    GL_ONE,
    GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR,
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_ALPHA,
    GL_SRC_ALPHA_SATURATE,
  };
  blend_sfactor = conv_blend_sfactor_table[Int_val(_blend_sfactor)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (blend_sfactor == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
