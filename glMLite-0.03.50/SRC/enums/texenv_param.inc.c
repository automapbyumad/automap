  static const GLenum conv_texenv_param_table[] = {
    GL_ADD,
    GL_ADD_SIGNED,
    GL_INTERPOLATE,
    GL_MODULATE,
    GL_DECAL,
    GL_BLEND,
    GL_REPLACE,
    GL_SUBTRACT,
    GL_COMBINE,
    GL_TEXTURE,
    GL_CONSTANT,
    GL_PRIMARY_COLOR,
    GL_PREVIOUS,
    GL_SRC_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_TRUE,
    GL_FALSE,
  };
  texenv_param = conv_texenv_param_table[Int_val(_texenv_param)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (texenv_param == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
