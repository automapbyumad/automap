  static const GLenum conv_pixel_transfer_f_table[] = {
    GL_RED_SCALE,
    GL_RED_BIAS,
    GL_GREEN_SCALE,
    GL_GREEN_BIAS,
    GL_BLUE_SCALE,
    GL_BLUE_BIAS,
    GL_ALPHA_SCALE,
    GL_ALPHA_BIAS,
    GL_DEPTH_SCALE,
    GL_DEPTH_BIAS,
  };
  pixel_transfer_f = conv_pixel_transfer_f_table[Int_val(_pixel_transfer_f)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_transfer_f == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
