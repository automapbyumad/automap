  static const GLenum conv_target_2d_table[] = {
    GL_TEXTURE_2D,
    GL_PROXY_TEXTURE_2D,
    GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
    GL_PROXY_TEXTURE_CUBE_MAP,
  };
  target_2d = conv_target_2d_table[Int_val(_target_2d)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (target_2d == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
