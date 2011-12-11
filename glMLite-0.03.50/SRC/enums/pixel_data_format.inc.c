  static const GLenum conv_pixel_data_format_table[] = {
    GL_COLOR_INDEX,
    GL_RED,
    GL_GREEN,
    GL_BLUE,
    GL_ALPHA,
    GL_RGB,
    GL_RGBA,
    GL_LUMINANCE,
    GL_LUMINANCE_ALPHA,
  };
  pixel_data_format = conv_pixel_data_format_table[Int_val(_pixel_data_format)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_data_format == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
