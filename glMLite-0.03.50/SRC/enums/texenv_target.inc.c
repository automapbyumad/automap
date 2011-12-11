  static const GLenum conv_texenv_target_table[] = {
    GL_TEXTURE_ENV,
    GL_TEXTURE_FILTER_CONTROL,
    GL_POINT_SPRITE,
  };
  texenv_target = conv_texenv_target_table[Int_val(_texenv_target)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (texenv_target == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
