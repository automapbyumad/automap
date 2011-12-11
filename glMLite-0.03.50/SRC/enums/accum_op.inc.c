  static const GLenum conv_accum_op_table[] = {
    GL_ACCUM,
    GL_LOAD,
    GL_ADD,
    GL_MULT,
    GL_RETURN,
  };
  accum_op = conv_accum_op_table[Int_val(_accum_op)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (accum_op == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
