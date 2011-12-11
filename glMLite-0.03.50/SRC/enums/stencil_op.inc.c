  static const GLenum conv_stencil_op_table[] = {
    GL_KEEP,
    GL_ZERO,
    GL_REPLACE,
    GL_INCR,
    GL_INCR_WRAP,
    GL_DECR,
    GL_DECR_WRAP,
    GL_INVERT,
  };
  stencil_op = conv_stencil_op_table[Int_val(_stencil_op)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (stencil_op == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
