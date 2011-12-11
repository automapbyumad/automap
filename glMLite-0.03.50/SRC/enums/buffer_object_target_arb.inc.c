  static const GLenum conv_buffer_object_target_arb_table[] = {
    GL_ARRAY_BUFFER_ARB,
    GL_ELEMENT_ARRAY_BUFFER_ARB,
    GL_PIXEL_PACK_BUFFER_ARB,
    GL_PIXEL_UNPACK_BUFFER_ARB,
  };
  buffer_object_target_arb = conv_buffer_object_target_arb_table[Int_val(_buffer_object_target_arb)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (buffer_object_target_arb == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
