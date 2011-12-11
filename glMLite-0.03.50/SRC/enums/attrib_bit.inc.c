  static const GLenum conv_attrib_bit_table[] = {
    GL_ACCUM_BUFFER_BIT,
    GL_COLOR_BUFFER_BIT,
    GL_CURRENT_BIT,
    GL_DEPTH_BUFFER_BIT,
    GL_ENABLE_BIT,
    GL_EVAL_BIT,
    GL_FOG_BIT,
    GL_HINT_BIT,
    GL_LIGHTING_BIT,
    GL_LINE_BIT,
    GL_LIST_BIT,
    GL_MULTISAMPLE_BIT,
    GL_PIXEL_MODE_BIT,
    GL_POINT_BIT,
    GL_POLYGON_BIT,
    GL_POLYGON_STIPPLE_BIT,
    GL_SCISSOR_BIT,
    GL_STENCIL_BUFFER_BIT,
    GL_TEXTURE_BIT,
    GL_TRANSFORM_BIT,
    GL_VIEWPORT_BIT,
  };
  attrib_bit = conv_attrib_bit_table[Int_val(_attrib_bit)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (attrib_bit == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
