  static const GLenum conv_get_float_4_table[] = {
    GL_ACCUM_CLEAR_VALUE,
    GL_BLEND_COLOR,
    GL_COLOR_CLEAR_VALUE,
    GL_CURRENT_COLOR,
    GL_CURRENT_RASTER_COLOR,
    GL_CURRENT_RASTER_POSITION,
    GL_CURRENT_RASTER_SECONDARY_COLOR,
    GL_CURRENT_RASTER_TEXTURE_COORDS,
    GL_CURRENT_SECONDARY_COLOR,
    GL_CURRENT_TEXTURE_COORDS,
    GL_FOG_COLOR,
    GL_LIGHT_MODEL_AMBIENT,
    GL_MAP2_GRID_DOMAIN,
  };
  get_float_4 = conv_get_float_4_table[Int_val(_get_float_4)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_float_4 == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
