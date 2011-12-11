  static const GLenum conv_color_material_mode_table[] = {
    GL_EMISSION,
    GL_AMBIENT,
    GL_DIFFUSE,
    GL_SPECULAR,
    GL_AMBIENT_AND_DIFFUSE,
  };
  color_material_mode = conv_color_material_mode_table[Int_val(_color_material_mode)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (color_material_mode == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
