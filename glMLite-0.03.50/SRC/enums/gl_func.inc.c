  static const GLenum conv_gl_func_table[] = {
    GL_NEVER,
    GL_LESS,
    GL_EQUAL,
    GL_LEQUAL,
    GL_GREATER,
    GL_NOTEQUAL,
    GL_GEQUAL,
    GL_ALWAYS,
  };
  gl_func = conv_gl_func_table[Int_val(_gl_func)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (gl_func == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
