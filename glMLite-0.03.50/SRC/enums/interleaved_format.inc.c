  static const GLenum conv_interleaved_format_table[] = {
    GL_V2F,
    GL_V3F,
    GL_C4UB_V2F,
    GL_C4UB_V3F,
    GL_C3F_V3F,
    GL_N3F_V3F,
    GL_C4F_N3F_V3F,
    GL_T2F_V3F,
    GL_T4F_V4F,
    GL_T2F_C4UB_V3F,
    GL_T2F_C3F_V3F,
    GL_T2F_N3F_V3F,
    GL_T2F_C4F_N3F_V3F,
    GL_T4F_C4F_N3F_V4F,
  };
  interleaved_format = conv_interleaved_format_table[Int_val(_interleaved_format)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (interleaved_format == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
