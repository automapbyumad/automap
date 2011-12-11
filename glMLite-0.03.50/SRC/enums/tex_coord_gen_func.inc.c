  static const GLenum conv_tex_coord_gen_func_table[] = {
    GL_TEXTURE_GEN_MODE,
  };
  tex_coord_gen_func = conv_tex_coord_gen_func_table[Int_val(_tex_coord_gen_func)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (tex_coord_gen_func == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
