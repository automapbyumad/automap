  static const GLenum conv_clear_mask_table[] = {
    GL_COLOR_BUFFER_BIT,
    GL_DEPTH_BUFFER_BIT,
    GL_ACCUM_BUFFER_BIT,
    GL_STENCIL_BUFFER_BIT,
  };
  clear_mask = conv_clear_mask_table[Int_val(_clear_mask)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (clear_mask == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
