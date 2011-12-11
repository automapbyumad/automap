  static const GLenum conv_init_mode_table[] = {
    GLUT_RGBA,
    GLUT_RGB,
    GLUT_INDEX,
    GLUT_SINGLE,
    GLUT_DOUBLE,
    GLUT_ACCUM,
    GLUT_ALPHA,
    GLUT_DEPTH,
    GLUT_STENCIL,
    GLUT_MULTISAMPLE,
    GLUT_STEREO,
    GLUT_LUMINANCE,
  };
  init_mode = conv_init_mode_table[Int_val(_init_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (init_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
