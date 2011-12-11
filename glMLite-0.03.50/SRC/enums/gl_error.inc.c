  static const GLenum conv_gl_error_table[] = {
    GL_NO_ERROR,
    GL_INVALID_ENUM,
    GL_INVALID_VALUE,
    GL_INVALID_OPERATION,
    GL_STACK_OVERFLOW,
    GL_STACK_UNDERFLOW,
    GL_OUT_OF_MEMORY,
    GL_TABLE_TOO_LARGE,
  };
  gl_error = conv_gl_error_table[Int_val(_gl_error)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (gl_error == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
