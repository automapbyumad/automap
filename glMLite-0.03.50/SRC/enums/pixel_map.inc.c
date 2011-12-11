  static const GLenum conv_pixel_map_table[] = {
    GL_PIXEL_MAP_I_TO_I,
    GL_PIXEL_MAP_S_TO_S,
    GL_PIXEL_MAP_I_TO_R,
    GL_PIXEL_MAP_I_TO_G,
    GL_PIXEL_MAP_I_TO_B,
    GL_PIXEL_MAP_I_TO_A,
    GL_PIXEL_MAP_R_TO_R,
    GL_PIXEL_MAP_G_TO_G,
    GL_PIXEL_MAP_B_TO_B,
    GL_PIXEL_MAP_A_TO_A,
  };
  pixel_map = conv_pixel_map_table[Int_val(_pixel_map)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_map == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
