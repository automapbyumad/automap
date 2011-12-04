(*------------------------------ 80 caracters --------------------------------*)

(* Depencies :
#directory "+lablGL";;
#directory "+sdl";;
#load "lablgl.cma";;
#load "bigarray.cma";;
#load "sdl.cma";;
#load "sdlloader.cma";;
#load "str.cma";;
*)

let xrot = ref 45.0 and yrot = ref 45.0 and zrot = ref 0.0
let xpos = ref 0.0 and ypos = ref 0.0 and zpos = ref 0.0
let zoom = ref 1.
let pw = ref 512. and ph = ref 512.
let displayMode = ref 1
let animate = ref false
let anaglyph = ref false
let ortho = ref true
let textured = ref true
let textures = ref (Array.make 0 (GL.glGenTexture ()))
let internalTimer = ref 0
let refTimer = ref 70
let wratio = ref 0.
 
(* Count how many vertices and faces there are in the input OBJ file
   BUT can also find at the end of the file a line containing those data
   like that : # numVertices numFaces
*)
let countVerticesAndFaces inputFile = 
  begin let pos = ref 1 and c = ref ' ' in
    try
      while !c != 'v' && !c != 'f' && !c != '#' do
	seek_in inputFile (in_channel_length inputFile - !pos);
	c := input_char inputFile;
	pos := !pos + 1
      done
  with Exit -> seek_in inputFile (in_channel_length inputFile - !pos); end;
  match Str.split (Str.regexp_string " ") (input_line inputFile) with
    | ["#"; vn; fn] -> 
	close_in inputFile;
	(int_of_string vn, int_of_string fn)
    | _ -> 
	seek_in inputFile 0;
	let vn = ref 0 and fn = ref 0 in
	  begin try
	    while true do
	      match Str.split (Str.regexp_string " ") (input_line inputFile) with
		| e::_ when e = "v" -> vn := !vn + 1
		| e::_ when e = "f" -> fn := !fn + 1
		| _ -> ()
	    done
	  with End_of_file -> close_in inputFile end;
	  (!vn, !fn)
	    
(* Converts ["1"; "2"; "3"] in [1; 2; 3] 
   AND determines the maximum index number (here, 3)  *)
let rec newFace string_list newface=
  match string_list with
    | [] -> newface
    | e::l -> newFace l ((int_of_string e)::newface)

(* Check if the given file name corresponds to an existing file 
   If so, it returns the correct filename
   If not, try to load the default file, and if it fails again
   exit the program (no way to continue)
*)
let rec findObjFile filename = 
  try
      ignore(open_in filename);
      filename;
  with Sys_error e -> 
    print_endline e;
    let newfile = "thegame.obj" in
      if filename <> newfile then
	begin
	  print_endline "loading default file";
	  findObjFile newfile;
	end
      else
	begin
	  print_endline "Error while loading default file";
	  print_endline ("No such "^newfile);
	  exit 1;
	end

exception NoSuchImporter of string

let load_image filename = 
  match Str.string_after filename (String.length filename - 4) with
    | ".png" | ".PNG" ->
	Png_loader.load_img (GL.Filename filename)
    | ".jpg" | ".JPG" | ".jpeg" | ".JPEG" ->
	Jpeg_loader.load_img (GL.Filename filename)
    | s -> 
	try Genimg_loader.load_img (GL.Filename filename) 
	with _ -> raise (NoSuchImporter s)

let loadTextures names =
  let texArray = GL.glGenTextures (Array.length names) in
    Array.iteri (fun i tex_id -> 
		   try
		     let (data, w, h, iformat, pformat) = load_image names.(i) in
		       begin 
			 if i = 0 then
			   begin
			     pw:=float w;
			     ph:=float h
			   end
			 else ()
		       end;
		       GL.glBindTexture2D tex_id;
		       GL.glTexImage2D 
			 ~target:GL.TexTarget.GL_TEXTURE_2D
			 ~level:0
			 ~internal_format:iformat 
			 ~width:w 
			 ~height:h
			 ~format_:pformat 
			 ~type_:GL.GL_UNSIGNED_BYTE
			 ~pixels:data
		   with NoSuchImporter s -> print_endline ("no such importer for "^s)
		)
      texArray;
    texArray

let enableTexture id = 
  GL.glTexParameter GL.TexParam.GL_TEXTURE_2D (GL.TexParam.GL_TEXTURE_MIN_FILTER GL.Min.GL_LINEAR);
  GL.glTexParameter GL.TexParam.GL_TEXTURE_2D (GL.TexParam.GL_TEXTURE_MAG_FILTER GL.Mag.GL_LINEAR);
  GL.glBindTexture2D !textures.(id)

class obj3D (nv,nf) = object (self)
  val nb_vertices = nv
  val nb_faces = nf
  val vboVertices = VBO.glGenBuffer ()
  val vboFaces = VBO.glGenBuffer ()
  val vboTexture = VBO.glGenBuffer ()
  val vboNormals = VBO.glGenBuffer ()
  val mutable vertexArray = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*3)
  val mutable textureArray = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*2)
  val mutable faceArray =  Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (nf*4)
  val mutable faceNormalArray = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nf*3)
  val mutable vertexNormalArray = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*3)
  method load fname = 
    Printf.printf "vertices : %d\nfaces : %d\n" nb_vertices nb_faces;
    (* Parses vertices found in the input OBJ file 
       AND returns the bounding box *)
    (* Parses faces found in the input OBJ file *)
    let inputFile = open_in fname in
    let max = ref 0. and may = ref 0. and maz = ref 0. and
	mix = ref 0. and miy = ref 0. and miz = ref 0. and
	nv = ref 0 and nf = ref 0 in
      begin try
	while true do
	  match Str.split (Str.regexp_string " ") (input_line inputFile) with
	    | ["v"; sx; sy; sz] ->
		let x = float_of_string sx and
		    y = float_of_string sy and
		    z = float_of_string sz in
		  if x > !max then max := x; if x < !mix then mix := x;
		  if y > !may then may := y; if y < !miy then miy := x;
		  if z > !maz then maz := z; if z < !miz then miz := x;
		  vertexArray.{ 3*(!nv) } <- x;
		  vertexArray.{3*(!nv)+1} <- y;
		  vertexArray.{3*(!nv)+2} <- z;
		  textureArray.{ 2*(!nv) } <- (x-.(!pw)/.2.)/.(!pw);
		  textureArray.{2*(!nv)+1} <- (z-.(!ph)/.2.)/.(!ph);
		  nv := !nv + 1
	    | ["f"; a; b; c; d] ->
		let a =  int_of_string a - 1 and
		    b =  int_of_string b - 1 and
		    c =  int_of_string c - 1 and
		    d =  int_of_string d - 1 in
		faceArray.{4*(!nf)  } <- Int32.of_int a;
		faceArray.{4*(!nf)+1} <- Int32.of_int b;
		faceArray.{4*(!nf)+2} <- Int32.of_int c;
		faceArray.{4*(!nf)+3} <- Int32.of_int d;
		  nf := !nf + 1
	    | ["f"; a; b; c] ->
		faceArray.{4*(!nf)  } <- Int32.pred (Int32.of_string a);
		faceArray.{4*(!nf)+1} <- Int32.pred (Int32.of_string b);
		faceArray.{4*(!nf)+2} <- Int32.pred (Int32.of_string c);
		faceArray.{4*(!nf)+3} <- Int32.pred (Int32.of_string c);
		nf := !nf + 1
	    |_ -> ()
	done
      with End_of_file -> close_in inputFile end;
      print_endline "done";
      zoom := (!maz -. !miz) /. 3.5;
      
      for i = 0 to nb_faces-1 do
	let a = Int32.to_int faceArray.{4*i  } and
	    b = Int32.to_int faceArray.{4*i+1} and
	    c = Int32.to_int faceArray.{4*i+2} in
	let x0 = vertexArray.{3*a  } and
	    y0 = vertexArray.{3*a+1} and
	    z0 = vertexArray.{3*a+2} and
	    x1 = vertexArray.{3*b  } and
	    y1 = vertexArray.{3*b+1} and
	    z1 = vertexArray.{3*b+2} and
	    x2 = vertexArray.{3*c  } and
	    y2 = vertexArray.{3*c+1} and
	    z2 = vertexArray.{3*c+2} in
	let (vx, vy, vz) = (x1-.x0, y1-.y0, z1-.z0) and
	    (ux, uy, uz) = (x2-.x0, y2-.y0, z2-.z0) in
	let fnx = uy*.vz-.uz*.vy and
	    fny = uz*.vx-.ux*.vz and
	    fnz = ux*.vy-.uy*.vx in
	  vertexNormalArray.{3*a  } <- vertexNormalArray.{3*a  } +. fnx;
	  vertexNormalArray.{3*a+1} <- vertexNormalArray.{3*a+1} +. fny;
	  vertexNormalArray.{3*a+2} <- vertexNormalArray.{3*a+2} +. fnz;
	  vertexNormalArray.{3*b  } <- vertexNormalArray.{3*b  } +. fnx;
	  vertexNormalArray.{3*b+1} <- vertexNormalArray.{3*b+1} +. fny;
	  vertexNormalArray.{3*b+2} <- vertexNormalArray.{3*b+2} +. fnz;
	  vertexNormalArray.{3*c  } <- vertexNormalArray.{3*c  } +. fnx;
	  vertexNormalArray.{3*c+1} <- vertexNormalArray.{3*c+1} +. fny;
	  vertexNormalArray.{3*c+2} <- vertexNormalArray.{3*c+2} +. fnz
      done;
      
      for n = 0 to nb_vertices-1 do
	let vnx = vertexNormalArray.{3*n  } and
	    vny = vertexNormalArray.{3*n+1} and
	    vnz = vertexNormalArray.{3*n+2} in
	let l = sqrt(vnx*.vnx +. vny*.vny +. vnz*.vnz) in
	  vertexNormalArray.{3*n  } <- vnx/.l;
	  vertexNormalArray.{3*n+1} <- vny/.l;
	  vertexNormalArray.{3*n+2} <- vnz/.l
      done;
      
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboVertices;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof vertexArray) vertexArray VBO.GL_STATIC_DRAW;
      
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboTexture;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof textureArray) textureArray VBO.GL_STATIC_DRAW;
      
      VBO.glBindBuffer VBO.GL_ELEMENT_ARRAY_BUFFER vboFaces;
      VBO.glBufferData VBO.GL_ELEMENT_ARRAY_BUFFER 
	(VBO.ba_sizeof faceArray) faceArray VBO.GL_STATIC_DRAW;

      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboNormals;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof vertexNormalArray) vertexNormalArray VBO.GL_STATIC_DRAW;
      
      VertArray.glEnableClientState VertArray.GL_NORMAL_ARRAY;
      VertArray.glEnableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
      VertArray.glEnableClientState VertArray.GL_VERTEX_ARRAY;
      
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboNormals;
      VertArray.glNormalPointer0 VertArray.Norm.GL_FLOAT 0;
      
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboTexture;
      VertArray.glTexCoordPointer0 2 VertArray.Coord.GL_FLOAT 0;
      
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboVertices;
      VBO.glBindBuffer VBO.GL_ELEMENT_ARRAY_BUFFER vboFaces;
      VertArray.glVertexPointer0 3 VertArray.Coord.GL_FLOAT 0

  method draw = 
    if !textured then enableTexture 0;
    (*    VertArray.glDrawArrays GL.GL_POINTS 0 (nb_vertices*3);*)
    VertArray.glDrawElements0 GL.GL_QUADS (nb_faces*4) VertArray.Elem.GL_UNSIGNED_INT

  method free =
    VertArray.glDisableClientState VertArray.GL_VERTEX_ARRAY;
    VertArray.glDisableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
    VertArray.glDisableClientState VertArray.GL_NORMAL_ARRAY;

    VBO.glUnbindBuffer VBO.GL_ARRAY_BUFFER;
    VBO.glUnbindBuffer VBO.GL_ELEMENT_ARRAY_BUFFER;

    VBO.glDeleteBuffer vboVertices;
    VBO.glDeleteBuffer vboTexture;
    VBO.glDeleteBuffer vboNormals;
    VBO.glDeleteBuffer vboFaces;
    Gc.full_major();
    print_endline "VBO removed"
	  
  method faces = faceArray
  method vertices = vertexArray
end

(* Display scene and fill screen *)
let drawScene screen (model:obj3D) =
  GL.glMatrixMode GL.GL_PROJECTION;
  GL.glLoadIdentity ();

  if !ortho then
    GL.glOrtho
      ~left:(-3. *. !zoom)
      ~right:(3. *. !zoom)
      ~bottom:(-3. *. !zoom /. !wratio)
      ~top:(3. *. !zoom /. !wratio)
      ~near:(-30.5 *. !zoom)
      ~far:(30.5 *. !zoom)
  else
    begin 
      Glu.gluPerspective 60.0 (!wratio) (0.3 *. !zoom) (3000. *. !zoom);
      Glu.gluLookAt 0. 0. ( !pw *. !zoom /. 100.) 0. 0. (1. *. !zoom /. 100.) 0. 1. 0.;
    end;
  GL.glMatrixMode GL.GL_MODELVIEW;
  GL.glLoadIdentity (); 
  GL.glColorMask true true true true;
  GL.glClearColor 0.2 0.2 0.2 1.;
  GL.glClear [GL.GL_COLOR_BUFFER_BIT;GL.GL_DEPTH_BUFFER_BIT];
(*  GL.glScale !zoom !zoom !zoom;*)
  GL.glPushMatrix ();
  GL.glTranslate ~x:!xpos ~y:!ypos ~z:(!zpos);
  GL.glRotate ~angle:!xrot ~x:1. ~y:0. ~z:0.;
  GL.glRotate ~angle:!yrot ~y:1. ~x:0. ~z:0.;

  begin if !anaglyph then 
    GL.glColorMask false true true false;
  end;
  GL.glLight ~light:(GL.GL_LIGHT 0) ~pname:(GL.Light.GL_POSITION (0., 100., 1000., -1.));

  begin match !displayMode with
    | 0 -> 
	GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_LINE;
    | e -> 
	GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL;
	if e = 5 then (* Lighting mode *)
	  begin
	    GL.glEnable GL.GL_LIGHT0;
	    GL.glDepthFunc GL.GL_LESS;
	    GL.glDepthMask true;
	    GL.glDisable GL.GL_STENCIL_TEST;
	    model#draw;    
(*recalculate silhouette of blocking objects viewed from casting light position using GLU tessellator *)
	    GL.glColorMask false false false false;
	    GL.glDepthMask false;
	    GL.glEnable GL.GL_STENCIL_TEST;
	    GL.glEnable GL.GL_POLYGON_OFFSET_FILL;
	    GL.glPolygonOffset 0.0 100.0;
	    GL.glClear [GL.GL_STENCIL_BUFFER_BIT];
	    GL.glCullFace GL.GL_FRONT;
	    GL.glDrawBuffer GL.DrawBuffer.GL_FRONT;
	    GL.glStencilFunc GL.GL_ALWAYS 0 255;
	    GL.glStencilOp GL.GL_KEEP GL.GL_KEEP GL.GL_INVERT;
	    model#draw;    
(* draw shadow volume *)
	    GL.glCullFace GL.GL_BACK;
	    GL.glDrawBuffer GL.DrawBuffer.GL_BACK;
	    model#draw;    
(* draw shadow volume *)
	    GL.glColorMask true true true true;
	    GL.glDepthFunc GL.GL_EQUAL;
	    GL.glStencilOp GL.GL_KEEP GL.GL_KEEP GL.GL_KEEP;
	    GL.glStencilFunc GL.GL_ALWAYS 0 255; (*GL.GL_NOTEQUAL*)
	    GL.glDisable GL.GL_LIGHT0
	  end
  end;
  model#draw;

  if !anaglyph then
    begin
      GL.glClear [GL.GL_DEPTH_BUFFER_BIT];
      GL.glColorMask true false false false;
      if !ortho then
	begin
	  GL.glPopMatrix ();
	  GL.glPushMatrix ();
	  GL.glTranslate ~x:(!xpos-.10.) ~y:!ypos ~z:!zpos;
	  GL.glRotate ~angle:!xrot ~x:1. ~y:0. ~z:0.;
	  GL.glRotate ~angle:!yrot ~y:1. ~x:0. ~z:0.;
	end
      else
	begin
	  GL.glMatrixMode GL.GL_PROJECTION;
	  GL.glLoadIdentity ();
	  Glu.gluPerspective 60.0 (!wratio) (0.3 *. !zoom) (3000. *. !zoom);
	  Glu.gluLookAt 10. 0. ( !pw *. !zoom /. 100.) 0. 0. (1. *. !zoom /. 100.) 0. 1. 0.;
	  GL.glMatrixMode GL.GL_MODELVIEW;
    	end;
      model#draw;
      GL.glPopMatrix ();
    end;

  GL.glFlush ();
  Sdlgl.swap_buffers ()

let toggleDisplayMode (n) =
  let max = 3 in
    displayMode := (if n < 0 && !displayMode = 0 then max - 1 
		    else (!displayMode + n) mod max);
    GL.glDepthFunc GL.GL_LESS;
    GL.glDisable GL.GL_CULL_FACE;
    GL.glDisable GL.GL_DEPTH_TEST;
    GL.glDisable GL.GL_TEXTURE_2D;
    GL.glDisable GL.GL_LIGHTING;
    GL.glDisable GL.GL_LIGHT0;
    GL.glDisable GL.GL_COLOR_MATERIAL;

    print_string "Changed display mode to ";

    if !textured then
      begin
	GL.glEnable GL.GL_TEXTURE_2D;
	print_string "textured ";
      end;
    
    begin match !displayMode with
      | 0 -> print_endline "wireframe"
      | 1 ->
	  GL.glEnable GL.GL_CULL_FACE;
	  GL.glCullFace GL.GL_BACK;
          GL.glEnable GL.GL_DEPTH_TEST;
          GL.glDepthMask true;
          GL.glDepthRange ~near:0.1 ~far:10.0;
          print_endline "solid"
      | 2 ->
	  GL.glEnable GL.GL_CULL_FACE;
	  GL.glCullFace GL.GL_BACK;
          GL.glEnable GL.GL_DEPTH_TEST;
          GL.glDepthMask true;
          GL.glDepthRange ~near:0.1 ~far:10.0;
 	  GL.glEnable GL.GL_LIGHTING;
	  GL.glEnable GL.GL_LIGHT0;
	  GL.glMaterial GL.GL_FRONT_AND_BACK (GL.Material.GL_SHININESS 100.);
	  GL.glEnable GL.GL_COLOR_MATERIAL;
	  GL.glColorMaterial GL.GL_FRONT_AND_BACK GL.GL_SPECULAR;
	  GL.glColorMaterial GL.GL_FRONT_AND_BACK GL.GL_AMBIENT_AND_DIFFUSE;
	  print_endline "shaded"
      | _ -> ();
    end

let rotateView xrel yrel =
  yrot := !yrot +. xrel;
  let newXrot = !xrot +. yrel in
    if newXrot < 90. && newXrot > -90. then xrot := newXrot

let panView xrel yrel =
  xpos := !xpos +. float xrel /. 150.0 *. !zoom;
  ypos := !ypos -. float yrel /. 150.0 *. !zoom

(* Here we handle all events *)
let rec mainLoop screen model =
  begin 
    let newTimer = Sdltimer.get_ticks () in
      if newTimer - !internalTimer > !refTimer then
	begin
	  internalTimer := newTimer;
	  drawScene screen model;
	end;
  end;

  begin match Sdlevent.poll () with
    | Some Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_ESCAPE}
    | Some Sdlevent.QUIT -> ()
    | Some event -> 
	begin match event with
	  | Sdlevent.KEYDOWN k -> begin match k.Sdlevent.keysym with 
		| Sdlkey.KEY_z -> toggleDisplayMode (1);
		| Sdlkey.KEY_s -> toggleDisplayMode (-1);
		| Sdlkey.KEY_a -> animate := not !animate; 
		| Sdlkey.KEY_d -> anaglyph := not !anaglyph;
		    if !anaglyph then print_endline "IN 3 D !!!!"
		| Sdlkey.KEY_t -> textured := not !textured; toggleDisplayMode (0);
		| Sdlkey.KEY_e -> ortho := not !ortho;
		| Sdlkey.KEY_UP -> rotateView 0. 5.;
		| Sdlkey.KEY_DOWN -> rotateView 0. (-5.);
		| Sdlkey.KEY_LEFT -> rotateView 5. 0.;
		| Sdlkey.KEY_RIGHT -> rotateView (-5.) 0.;
		| _ -> ();
	    end
	  | Sdlevent.MOUSEMOTION e -> begin match e.Sdlevent.mme_state with
	      | [Sdlmouse.BUTTON_RIGHT] -> 
		  rotateView (float e.Sdlevent.mme_xrel) (float e.Sdlevent.mme_yrel);
	      | [Sdlmouse.BUTTON_LEFT] ->
		  panView e.Sdlevent.mme_xrel e.Sdlevent.mme_yrel;
	      | _ -> ();
	    end
	  | Sdlevent.MOUSEBUTTONDOWN b 
	      when b.Sdlevent.mbe_button = Sdlmouse.BUTTON_WHEELDOWN ->
	      zoom := !zoom *. 1.1;
	  | Sdlevent.MOUSEBUTTONDOWN b 
	      when b.Sdlevent.mbe_button = Sdlmouse.BUTTON_WHEELUP ->
	      zoom := !zoom /. 1.1;
	  | event -> ();
	      (* print_endline (Sdlevent.string_of_event event);*)
	end;
	mainLoop screen model

    | None -> 
	begin if !animate then
	    rotateView 0.00005 0.
	else () end;
	mainLoop screen model
  end


(* Setup the screen *)
let main obj tex fps =
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title:"AutoMap" ~icon:"";
  internalTimer := Sdltimer.get_ticks ();
  refTimer := 1000 / fps;
  let screen = Sdlvideo.set_video_mode 0 0 [(*`FULLSCREEN;*) `OPENGL; `DOUBLEBUF] in
    begin let (w, h, _) = Sdlvideo.surface_dims screen in
      wratio := (float w) /. (float h)
    end;
    toggleDisplayMode (2);
    textures := loadTextures [|tex|];

    let filename = findObjFile obj in
      print_endline "Trying to load 3D model ...";       
      let model = new obj3D (countVerticesAndFaces (open_in filename)) in
	print_endline "Loading 3D model ...";
	model#load filename;
	print_endline "3D model successfuly loaded !";
	mainLoop screen model;
	model#free;
	Sdl.quit ()

let _ = 
  if Array.length Sys.argv >= 3 then
    begin
      let obj = Sys.argv.(1) and
	  tex = Sys.argv.(2) and
	  fps = if Array.length Sys.argv >= 4 then Sys.argv.(3) else "14" in
	main obj tex (int_of_string fps)
    end
  else
    print_endline "USAGE : e3D obj_filename texture_filename [fps]"
