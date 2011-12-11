  static const GLenum conv_map1_target_table[] = {
    GL_MAP1_VERTEX_3,
    GL_MAP1_VERTEX_4,
    GL_MAP1_INDEX,
    GL_MAP1_COLOR_4,
    GL_MAP1_NORMAL,
    GL_MAP1_TEXTURE_COORD_1,
    GL_MAP1_TEXTURE_COORD_2,
    GL_MAP1_TEXTURE_COORD_3,
    GL_MAP1_TEXTURE_COORD_4,
  };
  map1_target = conv_map1_target_table[Int_val(_map1_target)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (map1_target == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
