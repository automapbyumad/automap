  static const GLenum conv_mag_filter_table[] = {
    GL_NEAREST,
    GL_LINEAR,
  };
  mag_filter = conv_mag_filter_table[Int_val(_mag_filter)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (mag_filter == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
