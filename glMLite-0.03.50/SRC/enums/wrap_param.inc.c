  static const GLenum conv_wrap_param_table[] = {
    GL_CLAMP,
    GL_CLAMP_TO_BORDER,
    GL_CLAMP_TO_EDGE,
    GL_MIRRORED_REPEAT,
    GL_REPEAT,
  };
  wrap_param = conv_wrap_param_table[Int_val(_wrap_param)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (wrap_param == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
