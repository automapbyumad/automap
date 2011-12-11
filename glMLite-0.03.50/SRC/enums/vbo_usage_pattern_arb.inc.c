  static const GLenum conv_vbo_usage_pattern_arb_table[] = {
    GL_STREAM_DRAW_ARB,
    GL_STREAM_READ_ARB,
    GL_STREAM_COPY_ARB,
    GL_STATIC_DRAW_ARB,
    GL_STATIC_READ_ARB,
    GL_STATIC_COPY_ARB,
    GL_DYNAMIC_DRAW_ARB,
    GL_DYNAMIC_READ_ARB,
    GL_DYNAMIC_COPY_ARB,
  };
  vbo_usage_pattern_arb = conv_vbo_usage_pattern_arb_table[Int_val(_vbo_usage_pattern_arb)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (vbo_usage_pattern_arb == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
