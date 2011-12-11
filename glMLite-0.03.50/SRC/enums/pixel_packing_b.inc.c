  static const GLenum conv_pixel_packing_b_table[] = {
    GL_PACK_SWAP_BYTES,
    GL_PACK_LSB_FIRST,
    GL_UNPACK_SWAP_BYTES,
    GL_UNPACK_LSB_FIRST,
  };
  pixel_packing_b = conv_pixel_packing_b_table[Int_val(_pixel_packing_b)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_packing_b == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
