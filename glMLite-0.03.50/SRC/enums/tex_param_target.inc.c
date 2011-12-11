  static const GLenum conv_tex_param_target_table[] = {
    GL_TEXTURE_1D,
    GL_TEXTURE_2D,
    GL_TEXTURE_3D,
    GL_TEXTURE_CUBE_MAP,
  };
  tex_param_target = conv_tex_param_target_table[Int_val(_tex_param_target)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (tex_param_target == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
