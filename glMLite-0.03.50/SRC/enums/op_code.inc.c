  static const GLenum conv_op_code_table[] = {
    GL_CLEAR,
    GL_SET,
    GL_COPY,
    GL_COPY_INVERTED,
    GL_NOOP,
    GL_INVERT,
    GL_AND,
    GL_NAND,
    GL_OR,
    GL_NOR,
    GL_XOR,
    GL_EQUIV,
    GL_AND_REVERSE,
    GL_AND_INVERTED,
    GL_OR_REVERSE,
    GL_OR_INVERTED,
  };
  op_code = conv_op_code_table[Int_val(_op_code)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (op_code == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
