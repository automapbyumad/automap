  static const GLenum conv_get_integer_1_table[] = {
    GL_ACCUM_RED_BITS,
    GL_ACCUM_GREEN_BITS,
    GL_ACCUM_BLUE_BITS,
    GL_ACCUM_ALPHA_BITS,
    GL_CURRENT_RASTER_INDEX,
    GL_LIST_BASE,
    GL_LIST_INDEX,
    GL_MAX_LIGHTS,
    GL_MAX_LIST_NESTING,
    GL_MAX_TEXTURE_SIZE,
    GL_MAX_TEXTURE_COORDS,
    GL_ACTIVE_TEXTURE,
    GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS,
    GL_MAX_TEXTURE_UNITS,
    GL_MAX_ELEMENTS_INDICES,
    GL_MAX_ELEMENTS_VERTICES,
    GL_SUBPIXEL_BITS,
    GL_DEPTH_BITS,
    GL_STENCIL_BITS,
    GL_NAME_STACK_DEPTH,
    GL_COLOR_MATRIX_STACK_DEPTH,
    GL_MODELVIEW_STACK_DEPTH,
    GL_PROJECTION_STACK_DEPTH,
    GL_TEXTURE_STACK_DEPTH,
    GL_MAX_MODELVIEW_STACK_DEPTH,
    GL_MAX_PROJECTION_STACK_DEPTH,
    GL_MAX_TEXTURE_STACK_DEPTH,
    GL_SAMPLE_BUFFERS,
    GL_SAMPLES,
    GL_MAX_CLIP_PLANES,
    GL_AUX_BUFFERS,
    GL_RED_BITS,
    GL_GREEN_BITS,
    GL_BLUE_BITS,
    GL_ALPHA_BITS,
    GL_MAX_3D_TEXTURE_SIZE,
    GL_MAX_CLIENT_ATTRIB_STACK_DEPTH,
    GL_MAX_ATTRIB_STACK_DEPTH,
    GL_MAX_COLOR_MATRIX_STACK_DEPTH,
    GL_MAX_CUBE_MAP_TEXTURE_SIZE,
    GL_MAX_DRAW_BUFFERS,
    GL_MAX_NAME_STACK_DEPTH,
    GL_NORMAL_ARRAY_STRIDE,
    GL_INDEX_ARRAY_STRIDE,
    GL_COLOR_ARRAY_STRIDE,
    GL_EDGE_FLAG_ARRAY_STRIDE,
    GL_TEXTURE_COORD_ARRAY_STRIDE,
    GL_VERTEX_ARRAY_STRIDE,
    GL_SECONDARY_COLOR_ARRAY_STRIDE,
    GL_FOG_COORD_ARRAY_STRIDE,
    GL_SECONDARY_COLOR_ARRAY_SIZE,
    GL_SELECTION_BUFFER_SIZE,
    GL_MAX_FRAGMENT_UNIFORM_COMPONENTS,
    GL_MAX_VERTEX_UNIFORM_COMPONENTS,
    GL_MAX_VERTEX_ATTRIBS,
  };
  get_integer_1 = conv_get_integer_1_table[Int_val(_get_integer_1)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_integer_1 == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif