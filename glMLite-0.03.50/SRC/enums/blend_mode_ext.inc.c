  static const GLenum conv_blend_mode_ext_table[] = {
    GL_FUNC_ADD_EXT,
    GL_FUNC_SUBTRACT_EXT,
    GL_FUNC_REVERSE_SUBTRACT_EXT,
    GL_MIN_EXT,
    GL_MAX_EXT,
    GL_ALPHA_MIN_SGIX,
    GL_ALPHA_MAX_SGIX,
    GL_LOGIC_OP,
  };
  blend_mode_ext = conv_blend_mode_ext_table[Int_val(_blend_mode_ext)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (blend_mode_ext == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
