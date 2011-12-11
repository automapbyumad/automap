  static const GLenum conv_face_mode_table[] = {
    GL_FRONT,
    GL_BACK,
    GL_FRONT_AND_BACK,
  };
  face_mode = conv_face_mode_table[Int_val(_face_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (face_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
