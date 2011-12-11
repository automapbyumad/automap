  static const GLenum conv_pixel_buffer_format_table[] = {
    GL_COLOR_INDEX,
    GL_STENCIL_INDEX,
    GL_DEPTH_COMPONENT,
    GL_RED,
    GL_GREEN,
    GL_BLUE,
    GL_ALPHA,
    GL_RGB,
    GL_BGR,
    GL_RGBA,
    GL_BGRA,
    GL_LUMINANCE,
    GL_LUMINANCE_ALPHA,
  };
  pixel_buffer_format = conv_pixel_buffer_format_table[Int_val(_pixel_buffer_format)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_buffer_format == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
