  static const GLenum conv_matrix_mode_table[] = {
    GL_MODELVIEW,
    GL_PROJECTION,
    GL_TEXTURE,
  };
  matrix_mode = conv_matrix_mode_table[Int_val(_matrix_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (matrix_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
