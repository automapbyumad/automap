  static const GLenum conv_clip_plane_table[] = {
    GL_CLIP_PLANE0,
    GL_CLIP_PLANE1,
    GL_CLIP_PLANE2,
    GL_CLIP_PLANE3,
    GL_CLIP_PLANE4,
    GL_CLIP_PLANE5,
  };
  clip_plane = conv_clip_plane_table[Int_val(_clip_plane)];
#if defined(USE_MY_GL3_CORE_PROFILE)
  if (clip_plane == 0x000A)
    caml_failwith("using gl-enum deprecated in core OpenGL 3");
#endif
