  static const GLenum conv_pixel_packing_i_table[] = {
    GL_PACK_ROW_LENGTH,
    GL_PACK_IMAGE_HEIGHT,
    GL_PACK_SKIP_PIXELS,
    GL_PACK_SKIP_ROWS,
    GL_PACK_SKIP_IMAGES,
    GL_PACK_ALIGNMENT,
    GL_UNPACK_ROW_LENGTH,
    GL_UNPACK_IMAGE_HEIGHT,
    GL_UNPACK_SKIP_PIXELS,
    GL_UNPACK_SKIP_ROWS,
    GL_UNPACK_SKIP_IMAGES,
    GL_UNPACK_ALIGNMENT,
  };
  pixel_packing_i = conv_pixel_packing_i_table[Int_val(_pixel_packing_i)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_packing_i == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
