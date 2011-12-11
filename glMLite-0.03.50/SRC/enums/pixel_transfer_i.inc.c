  static const GLenum conv_pixel_transfer_i_table[] = {
    GL_INDEX_SHIFT,
    GL_INDEX_OFFSET,
  };
  pixel_transfer_i = conv_pixel_transfer_i_table[Int_val(_pixel_transfer_i)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_transfer_i == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
