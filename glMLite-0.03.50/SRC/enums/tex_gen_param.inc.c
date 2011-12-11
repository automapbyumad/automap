  static const GLenum conv_tex_gen_param_table[] = {
    GL_OBJECT_LINEAR,
    GL_EYE_LINEAR,
    GL_SPHERE_MAP,
    GL_NORMAL_MAP,
    GL_REFLECTION_MAP,
  };
  tex_gen_param = conv_tex_gen_param_table[Int_val(_tex_gen_param)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (tex_gen_param == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
