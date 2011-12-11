  static const GLenum conv_pixel_type_table[] = {
    GL_COLOR,
    GL_DEPTH,
    GL_STENCIL,
  };
  pixel_type = conv_pixel_type_table[Int_val(_pixel_type)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_type == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
