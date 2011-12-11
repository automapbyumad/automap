  static const GLenum conv_tex_coord_table[] = {
    GL_S,
    GL_T,
    GL_R,
    GL_Q,
  };
  tex_coord = conv_tex_coord_table[Int_val(_tex_coord)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (tex_coord == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
