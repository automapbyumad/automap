  static const GLenum conv_tex_coord_fun_params_table[] = {
    GL_OBJECT_PLANE,
    GL_EYE_PLANE,
  };
  tex_coord_fun_params = conv_tex_coord_fun_params_table[Int_val(_tex_coord_fun_params)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (tex_coord_fun_params == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
