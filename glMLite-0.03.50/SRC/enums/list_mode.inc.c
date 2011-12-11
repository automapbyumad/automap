  static const GLenum conv_list_mode_table[] = {
    GL_COMPILE,
    GL_COMPILE_AND_EXECUTE,
  };
  list_mode = conv_list_mode_table[Int_val(_list_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (list_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
