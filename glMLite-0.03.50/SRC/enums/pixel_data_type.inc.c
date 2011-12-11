  static const GLenum conv_pixel_data_type_table[] = {
    GL_UNSIGNED_BYTE,
    GL_BYTE,
    GL_BITMAP,
    GL_UNSIGNED_SHORT,
    GL_SHORT,
    GL_UNSIGNED_INT,
    GL_INT,
    GL_FLOAT,
    GL_UNSIGNED_BYTE_3_3_2,
    GL_UNSIGNED_BYTE_2_3_3_REV,
    GL_UNSIGNED_SHORT_5_6_5,
    GL_UNSIGNED_SHORT_5_6_5_REV,
    GL_UNSIGNED_SHORT_4_4_4_4,
    GL_UNSIGNED_SHORT_4_4_4_4_REV,
    GL_UNSIGNED_SHORT_5_5_5_1,
    GL_UNSIGNED_SHORT_1_5_5_5_REV,
    GL_UNSIGNED_INT_8_8_8_8,
    GL_UNSIGNED_INT_8_8_8_8_REV,
    GL_UNSIGNED_INT_10_10_10_2,
    GL_UNSIGNED_INT_2_10_10_10_REV,
  };
  pixel_data_type = conv_pixel_data_type_table[Int_val(_pixel_data_type)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (pixel_data_type == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
