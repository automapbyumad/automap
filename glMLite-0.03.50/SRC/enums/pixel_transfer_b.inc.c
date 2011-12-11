  static const GLenum conv_pixel_transfer_b_table[] = {
    GL_MAP_COLOR,
    GL_MAP_STENCIL,
  };
  pixel_transfer_b = conv_pixel_transfer_b_table[Int_val(_pixel_transfer_b)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_transfer_b == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
