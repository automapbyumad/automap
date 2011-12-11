  static const GLenum conv_get_float_3_table[] = {
    GL_CURRENT_NORMAL,
    GL_POINT_DISTANCE_ATTENUATION,
  };
  get_float_3 = conv_get_float_3_table[Int_val(_get_float_3)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_float_3 == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
