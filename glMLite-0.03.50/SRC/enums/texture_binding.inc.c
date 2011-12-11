  static const GLenum conv_texture_binding_table[] = {
    GL_TEXTURE_1D,
    GL_TEXTURE_2D,
    GL_TEXTURE_3D,
    GL_TEXTURE_CUBE_MAP,
  };
  texture_binding = conv_texture_binding_table[Int_val(_texture_binding)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (texture_binding == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
