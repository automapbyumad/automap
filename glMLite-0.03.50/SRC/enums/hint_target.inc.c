  static const GLenum conv_hint_target_table[] = {
    GL_FOG_HINT,
    GL_LINE_SMOOTH_HINT,
    GL_PERSPECTIVE_CORRECTION_HINT,
    GL_POINT_SMOOTH_HINT,
    GL_POLYGON_SMOOTH_HINT,
    GL_TEXTURE_COMPRESSION_HINT,
    GL_GENERATE_MIPMAP_HINT,
    GL_FRAGMENT_SHADER_DERIVATIVE_HINT,
  };
  hint_target = conv_hint_target_table[Int_val(_hint_target)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (hint_target == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
