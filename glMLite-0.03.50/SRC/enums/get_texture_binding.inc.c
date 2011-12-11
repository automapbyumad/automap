  static const GLenum conv_get_texture_binding_table[] = {
    GL_TEXTURE_BINDING_1D,
    GL_TEXTURE_BINDING_2D,
    GL_TEXTURE_BINDING_3D,
    GL_TEXTURE_BINDING_CUBE_MAP,
  };
  get_texture_binding = conv_get_texture_binding_table[Int_val(_get_texture_binding)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (get_texture_binding == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
