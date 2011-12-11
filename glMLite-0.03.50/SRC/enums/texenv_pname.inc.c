  static const GLenum conv_texenv_pname_table[] = {
    GL_TEXTURE_ENV_MODE,
    GL_TEXTURE_LOD_BIAS,
    GL_COMBINE_RGB,
    GL_COMBINE_ALPHA,
    GL_SRC0_RGB,
    GL_SRC1_RGB,
    GL_SRC2_RGB,
    GL_SRC0_ALPHA,
    GL_SRC1_ALPHA,
    GL_SRC2_ALPHA,
    GL_OPERAND0_RGB,
    GL_OPERAND1_RGB,
    GL_OPERAND2_RGB,
    GL_OPERAND0_ALPHA,
    GL_OPERAND1_ALPHA,
    GL_OPERAND2_ALPHA,
    GL_RGB_SCALE,
    GL_ALPHA_SCALE,
    GL_COORD_REPLACE,
  };
  texenv_pname = conv_texenv_pname_table[Int_val(_texenv_pname)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (texenv_pname == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
