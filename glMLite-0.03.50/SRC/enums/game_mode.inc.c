  static const GLenum conv_game_mode_table[] = {
    GLUT_GAME_MODE_ACTIVE,
    GLUT_GAME_MODE_POSSIBLE,
    GLUT_GAME_MODE_WIDTH,
    GLUT_GAME_MODE_HEIGHT,
    GLUT_GAME_MODE_PIXEL_DEPTH,
    GLUT_GAME_MODE_REFRESH_RATE,
    GLUT_GAME_MODE_DISPLAY_CHANGED,
  };
  game_mode = conv_game_mode_table[Int_val(_game_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (game_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
