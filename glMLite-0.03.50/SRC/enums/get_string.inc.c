  static const GLenum conv_get_string_table[] = {
    GL_VENDOR,
    GL_RENDERER,
    GL_VERSION,
    GL_SHADING_LANGUAGE_VERSION,
    GL_EXTENSIONS,
  };
  get_string = conv_get_string_table[Int_val(_get_string)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_string == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
