  static const GLenum conv_copy_tex_target_table[] = {
    GL_TEXTURE_2D,
    GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
  };
  copy_tex_target = conv_copy_tex_target_table[Int_val(_copy_tex_target)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (copy_tex_target == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
