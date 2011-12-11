  static const GLenum conv_map2_target_table[] = {
    GL_MAP2_VERTEX_3,
    GL_MAP2_VERTEX_4,
    GL_MAP2_INDEX,
    GL_MAP2_COLOR_4,
    GL_MAP2_NORMAL,
    GL_MAP2_TEXTURE_COORD_1,
    GL_MAP2_TEXTURE_COORD_2,
    GL_MAP2_TEXTURE_COORD_3,
    GL_MAP2_TEXTURE_COORD_4,
  };
  map2_target = conv_map2_target_table[Int_val(_map2_target)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (map2_target == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
