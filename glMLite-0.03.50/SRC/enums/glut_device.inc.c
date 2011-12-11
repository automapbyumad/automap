  static const GLenum conv_glut_device_table[] = {
    GLUT_HAS_KEYBOARD,
    GLUT_HAS_MOUSE,
    GLUT_HAS_SPACEBALL,
    GLUT_HAS_DIAL_AND_BUTTON_BOX,
    GLUT_HAS_TABLET,
    GLUT_NUM_MOUSE_BUTTONS,
    GLUT_NUM_SPACEBALL_BUTTONS,
    GLUT_NUM_BUTTON_BOX_BUTTONS,
    GLUT_NUM_DIALS,
    GLUT_NUM_TABLET_BUTTONS,
  };
  glut_device = conv_glut_device_table[Int_val(_glut_device)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (glut_device == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
