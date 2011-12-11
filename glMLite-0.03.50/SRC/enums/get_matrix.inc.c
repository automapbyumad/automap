  static const GLenum conv_get_matrix_table[] = {
    GL_COLOR_MATRIX,
    GL_MODELVIEW_MATRIX,
    GL_PROJECTION_MATRIX,
    GL_TEXTURE_MATRIX,
    GL_TRANSPOSE_COLOR_MATRIX,
    GL_TRANSPOSE_MODELVIEW_MATRIX,
    GL_TRANSPOSE_PROJECTION_MATRIX,
    GL_TRANSPOSE_TEXTURE_MATRIX,
  };
  get_matrix = conv_get_matrix_table[Int_val(_get_matrix)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_matrix == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
