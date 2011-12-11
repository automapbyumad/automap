  static const GLenum conv_mouse_button_table[] = {
    GLUT_LEFT_BUTTON,
    GLUT_MIDDLE_BUTTON,
    GLUT_RIGHT_BUTTON,
    GLUT_WHEEL_UP,
    GLUT_WHEEL_DOWN,
  };
  mouse_button = conv_mouse_button_table[Int_val(_mouse_button)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (mouse_button == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
