  static const GLenum conv_render_mode_table[] = {
    GL_RENDER,
    GL_SELECT,
    GL_FEEDBACK,
  };
  render_mode = conv_render_mode_table[Int_val(_render_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (render_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
