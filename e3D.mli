val xrot : float ref
val yrot : float ref
val zrot : float ref
val xpos : float ref
val ypos : float ref
val zpos : float ref
val yDecal : float ref
val yScale : float ref
val liX : float ref
val liY : float ref
val liZ : float ref
val liNX : float ref
val liNY : float ref
val liNZ : float ref
val zoom : float ref
val pw : float ref
val ph : float ref
val dw : float ref
val dh : float ref
val displayMode : int ref
val animate : bool ref
val fullscreen : bool ref
val intro : bool ref
val intro_step : int ref
val intro_wiggle : float ref
val intro_xpos : float ref
val intro_ypos : float ref
val intro_zpos : float ref
val intro_yScale : float ref
val intro_yDecal : float ref
val anaglyph : bool ref
val ortho : bool ref
val textured : bool ref
val shadowMap : bool ref
val internalTimer : int ref
val refTimer : int ref
val ww : float ref
val wh : float ref
val wratio : float ref
val countVerticesAndFaces : in_channel -> int * int
val findObjFile : string -> string
exception NoSuchImporter of string
val load_image :
  string ->
  GL.image_data * int * int * GL.InternalFormat.internal_format *
  GL.pixel_data_format
val loadTextures : string array -> GL.texture_id array
val enableTexture : GL.texture_id -> unit
class obj3D :
  int * int ->
  object
    val nb_faces : int
    val nb_vertices : int
    val vboDetailMap : VBO.vbo_id
    val vboFaces : VBO.vbo_id
    val vboNormals : VBO.vbo_id
    val vboTexture : VBO.vbo_id
    val vboVertices : VBO.vbo_id
    method destroy : unit
    method draw : GL.texture_id array -> unit
    method load : string -> string -> unit
  end
val drawScene : 'a -> obj3D -> GL.texture_id array -> unit
val disableEverything : unit -> unit
val toggleDisplayMode : int -> unit
val wiggle : float -> float
val animateIntro : unit -> unit
val rotateView : float -> float -> unit
val panView : int -> int -> unit
val mainLoop : 'a -> obj3D -> GL.texture_id array -> unit
val main : string -> string -> string -> int -> unit
