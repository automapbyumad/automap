(* Automap 3D Engine                       *)
(*     by UMAD team :                      *)
(*   http://umad.fr.nf                     *)
(* Supports up to 14 millions of points    *)
(* on regular laptops                      *)


(*------------------------------ 80 caracters --------------------------------*)

(* Depencies :
#directory "+glMLite";;
#directory "+sdl";;
#load "vertArray.cma";;
#load "vbo.cma";;
...
#load "glmlite.cma";;
#load "bigarray.cma";;
#load "sdl.cma";;
#load "sdlloader.cma";;
#load "str.cma";;
*)

(* Map rotation *)   let xrot = ref 45. and yrot = ref 45. and zrot = ref 0.
(* Map position *)   let xpos = ref 0. and ypos = ref 0. and zpos = ref 0.
(* Height offset *)  let yDecal = ref 0.
(* Height scale *)   let yScale = ref 1.
(* Light position *) let liX= ref (-200.) and liY= ref 100. and liZ= ref (-100.)
(* Normalized light vector *) let (liNX, liNY, liNZ) = 
  begin 
    let l = sqrt(!liX *. !liX +. !liY *. !liY +. !liZ *. !liZ) in 
     (ref (!liX/.l), ref (!liY/.l), ref (!liZ/.l))
  end
(* Zoom factor *)     let zoom = ref 1.
(* Picture Size *)    let pw = ref 512. and ph = ref 512.
(* Detail Map Size *) let dw = ref 5. and dh = ref 5.
let displayMode = ref 1
let animate = ref false
let fullscreen = ref true
let intro = ref true
let intro_step = ref 0
let intro_wiggle = ref 0.
let intro_xpos = ref 0. and intro_ypos = ref 0. and intro_zpos = ref 0.
let intro_yScale = ref 0.1 and intro_yDecal = ref 0.
let anaglyph = ref false
let ortho = ref false
let textured = ref true
let shadowMap = ref true
(*let textures = ref (Array.make 0 (GL.glGenTexture ()))*)
let internalTimer = ref 0
let refTimer = ref 70 (* 1000 / fps = refTimer *)
(* Window dimensions *)   let ww = ref 640. and wh = ref 400.
(* Window aspect ratio *) let wratio = ref 0.
 
(* Count how many vertices and faces there are in the input OBJ file
   BUT can also find at the end of the file a line containing those data
   like that : # numVertices numFaces *)
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
	      match Str.split (Str.regexp_string " ") (input_line inputFile) 
	      with
		| e::_ when e = "v" -> vn := !vn + 1
		| e::_ when e = "f" -> fn := !fn + 1
		| _ -> ()
	    done
	  with End_of_file -> close_in inputFile end;
	  (!vn, !fn)
	    
(* Check if the given file name corresponds to an existing file.
   If so, it returns the correct filename.
   If not, try to load the default file, and if it fails again
   exit the program (no way to continue) *)
let rec findObjFile filename = 
  try
      ignore(open_in filename);
      filename;
  with Sys_error e -> 
    print_endline e;
    let newfile = "not_found.obj" in
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
  try Genimg_loader.load_img (GL.Filename filename) 
  with _ -> raise (NoSuchImporter filename)

let loadTextures names =
  let texArray = GL.glGenTextures (Array.length names) in
    Array.iteri (fun i tex_id -> 
		   try
		     let (data, w, h, iformat, pformat) = 
		       load_image names.(i) in
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
		   with NoSuchImporter s -> 
		     print_endline ("no such importer for "^s)
    )
      texArray;
  texArray

let enableTexture id = 
  GL.glTexParameter GL.TexParam.GL_TEXTURE_2D 
    (GL.TexParam.GL_TEXTURE_MIN_FILTER GL.Min.GL_LINEAR);
  GL.glTexParameter GL.TexParam.GL_TEXTURE_2D 
    (GL.TexParam.GL_TEXTURE_MAG_FILTER GL.Mag.GL_LINEAR);
  GL.glBindTexture2D id

class obj3D (nv,nf) = object (self)
  val nb_vertices = nv
  val nb_faces = nf
  val vboVertices = VBO.glGenBuffer ()
  val vboFaces = VBO.glGenBuffer ()
  val vboTexture = VBO.glGenBuffer ()
  val vboDetailMap = VBO.glGenBuffer ()
  val vboNormals = VBO.glGenBuffer ()

(* obj#load <obj file> <heightmap used for generation>*)
  method load fname heightmap =
    let vertexArray = 
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*3)
    and textureArray = 
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*2)
    and detailMapArray = 
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*2)
    and faceArray =  
      Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (nf*4)
    and vertexNormalArray = 
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (nv*3)
    in
    Printf.printf "vertices : %d\nfaces : %d\n" nb_vertices nb_faces;
    (* Parses vertices found in the input OBJ file 
       AND returns the bounding box *)
    (* Parses faces found in the input OBJ file *)
    let inputFile = open_in fname 
    and max = ref 0. and may = ref 0. and maz = ref 0. and
	mix = ref 0. and miy = ref 0. and miz = ref 0. and
	nv = ref 0 and nf = ref 0 in
    let hmap = Sdlloader.load_image heightmap in
    let (wh, hh, _)  = Sdlvideo.surface_dims hmap in
    let getLevel x y = 
      let (r,g,b) = 
	Sdlvideo.get_pixel_color hmap (int_of_float x) (int_of_float y) in
	int_of_float (0.3*.(float r) +. 0.59*.(float g) +. 0.11*.(float b))
    in
(* Decide wheter or not a given point is in the shadow of another blocking
   object like a mountain according to the given heightmap *)
    let isPointInShadow x0 y0 z0 =
      let x = ref (x0 -. !mix)
      and z = ref (z0 -. !miz ) in
      if  !x >= 0. && !z >= 0. && !x < (float wh) && !z < (float hh) then
	begin
	  let y = ref (float (getLevel !x !z)) in
	  let h = ref !y in
	  while !x >= 0. && !z >= 0.
	    && !x < (float wh) && !z < (float hh)
	    && !y >= !h do
	    h := float (getLevel !x !z) (* current heigh *);
	      x := !x +. !liNX;
	      y := !y +. !liNY;
	      z := !z +. !liNZ;
	  done;
	  !y < !h (* returns if ray is blocked by current height *)
	end
      else
	false
    in
      begin try
	while true do
	  match Str.split (Str.regexp_string " ") (input_line inputFile) with
	    | ["v"; sx; sy; sz] -> (* Vertice *)
		let x = float_of_string sx and
		    y = float_of_string sy and
		    z = float_of_string sz in
		  if x > !max then max := x; if x < !mix then mix := x;
		  if y > !may then may := y; if y < !miy then miy := x;
		  if z > !maz then maz := z; if z < !miz then miz := x;
		  vertexArray.{ 3*(!nv) } <- x;
		  vertexArray.{3*(!nv)+1} <- y;
		  vertexArray.{3*(!nv)+2} <- z;
		  vertexNormalArray.{ 3*(!nv) } <- 0.;
		  vertexNormalArray.{3*(!nv)+1} <- 0.;
		  vertexNormalArray.{3*(!nv)+2} <- 0.;
		  textureArray.{ 2*(!nv) } <- (x-.(!pw)/.2.)/.(!pw);
		  textureArray.{2*(!nv)+1} <- (z-.(!ph)/.2.)/.(!ph);
		  detailMapArray.{ 2*(!nv) } <- (x-.(!dw)/.2.)/.(!dw);
		  detailMapArray.{2*(!nv)+1} <- (z-.(!dh)/.2.)/.(!dh);
		  nv := !nv + 1
	    | ["f"; a; b; c; d] -> (* Quad *)
		let a =  int_of_string a - 1 and
		    b =  int_of_string b - 1 and
		    c =  int_of_string c - 1 and
		    d =  int_of_string d - 1 in
		faceArray.{4*(!nf)  } <- Int32.of_int a;
		faceArray.{4*(!nf)+1} <- Int32.of_int b;
		faceArray.{4*(!nf)+2} <- Int32.of_int c;
		faceArray.{4*(!nf)+3} <- Int32.of_int d;
		  nf := !nf + 1
	    | ["f"; a; b; c] -> (* Triangle *)
		faceArray.{4*(!nf)  } <- Int32.pred (Int32.of_string a);
		faceArray.{4*(!nf)+1} <- Int32.pred (Int32.of_string b);
		faceArray.{4*(!nf)+2} <- Int32.pred (Int32.of_string c);
		(* fake quad *)
		faceArray.{4*(!nf)+3} <- Int32.pred (Int32.of_string c);
		nf := !nf + 1
	    |_ -> ()
	done
      with End_of_file -> close_in inputFile end;
      print_endline "done";
      zoom := (!maz -. !miz) /. 3.5;
 
      
      print_endline "Launching vertex normal calculation...";
(* Vertex Normal calculation *)
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
(* If point is in shadow, it's normal is 0. *)
	if not !shadowMap || not (isPointInShadow x0 y0 z0) then
	  begin
	    vertexNormalArray.{3*a  } <- vertexNormalArray.{3*a  } +. fnx;
	    vertexNormalArray.{3*a+1} <- vertexNormalArray.{3*a+1} +. fny;
	    vertexNormalArray.{3*a+2} <- vertexNormalArray.{3*a+2} +. fnz;
	    vertexNormalArray.{3*b  } <- vertexNormalArray.{3*b  } +. fnx;
	    vertexNormalArray.{3*b+1} <- vertexNormalArray.{3*b+1} +. fny;
	    vertexNormalArray.{3*b+2} <- vertexNormalArray.{3*b+2} +. fnz;
	    vertexNormalArray.{3*c  } <- vertexNormalArray.{3*c  } +. fnx;
	    vertexNormalArray.{3*c+1} <- vertexNormalArray.{3*c+1} +. fny;
	    vertexNormalArray.{3*c+2} <- vertexNormalArray.{3*c+2} +. fnz
	  end
      done;
      
  (* Vertex Normal normalization => n in [-1;1] *)      
      for n = 0 to nb_vertices-1 do
	let vnx = vertexNormalArray.{3*n  } and
	    vny = vertexNormalArray.{3*n+1} and
	    vnz = vertexNormalArray.{3*n+2} in
	let l = sqrt(vnx*.vnx +. vny*.vny +. vnz*.vnz) in
	vertexNormalArray.{3*n  } <- vnx/.l;
	vertexNormalArray.{3*n+1} <- vny/.l;
	vertexNormalArray.{3*n+2} <- vnz/.l
      done;
      Printf.printf "done\n%!";
	
(* Add Vertices to VBO *)
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboVertices;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof vertexArray) vertexArray VBO.GL_STATIC_DRAW;

(* Add Textures Coordinates to VBO *)
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboTexture;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof textureArray) textureArray VBO.GL_STATIC_DRAW;

(* Add Detail Map Coordinates to VBO *)
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboDetailMap;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof detailMapArray) detailMapArray VBO.GL_STATIC_DRAW;

(* Add Face indices to VBO *)
      VBO.glBindBuffer VBO.GL_ELEMENT_ARRAY_BUFFER vboFaces;
      VBO.glBufferData VBO.GL_ELEMENT_ARRAY_BUFFER 
	(VBO.ba_sizeof faceArray) faceArray VBO.GL_STATIC_DRAW;
 
(* Add Vertex Normals to VBO *)
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboNormals;
      VBO.glBufferData VBO.GL_ARRAY_BUFFER 
	(VBO.ba_sizeof vertexNormalArray) vertexNormalArray VBO.GL_STATIC_DRAW;
      
      VertArray.glEnableClientState VertArray.GL_NORMAL_ARRAY;
      VertArray.glEnableClientState VertArray.GL_VERTEX_ARRAY;
      
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboNormals;
      VertArray.glNormalPointer0 VertArray.Norm.GL_FLOAT 0;
         
      VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboVertices;
      VBO.glBindBuffer VBO.GL_ELEMENT_ARRAY_BUFFER vboFaces;
      VertArray.glVertexPointer0 3 VertArray.Coord.GL_FLOAT 0

  method draw textures = 
    if !textured then 
      begin
	GL.glClientActiveTexture GL.GL_TEXTURE0;
	GL.glEnable GL.GL_TEXTURE_2D;
	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboTexture;
	VertArray.glTexCoordPointer0 2 VertArray.Coord.GL_FLOAT 0;
	GL.glActiveTexture GL.GL_TEXTURE0;
	VertArray.glEnableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
	enableTexture textures.(0);
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_TEXTURE_ENV_MODE GL.TexEnv.GL_MODULATE;

	GL.glClientActiveTexture GL.GL_TEXTURE1;
	GL.glEnable GL.GL_TEXTURE_2D;
	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboDetailMap;
	VertArray.glTexCoordPointer0 2 VertArray.Coord.GL_FLOAT 0;
	GL.glActiveTexture GL.GL_TEXTURE1;
	VertArray.glEnableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
	enableTexture textures.(1);
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_TEXTURE_ENV_MODE GL.TexEnv.GL_COMBINE;
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_COMBINE_RGB GL.TexEnv.GL_INTERPOLATE;
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_SRC0_RGB GL.TexEnv.GL_TEXTURE;
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_OPERAND0_RGB GL.TexEnv.GL_SRC_COLOR;
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_SRC2_RGB GL.TexEnv.GL_PREVIOUS;
	GL.glTexEnv GL.TexEnv.GL_TEXTURE_ENV 
	  GL.TexEnv.GL_OPERAND2_RGB GL.TexEnv.GL_SRC_COLOR;

	GL.glClientActiveTexture GL.GL_TEXTURE2;
	GL.glEnable GL.GL_TEXTURE_2D;
	VBO.glBindBuffer VBO.GL_ARRAY_BUFFER vboDetailMap;
	VertArray.glTexCoordPointer0 2 VertArray.Coord.GL_FLOAT 0;
	GL.glActiveTexture GL.GL_TEXTURE2;
	VertArray.glEnableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
	enableTexture textures.(1);

	VertArray.glDrawElements0 GL.GL_QUADS (nb_faces*4) 
	  VertArray.Elem.GL_UNSIGNED_INT;

	GL.glClientActiveTexture GL.GL_TEXTURE0;
	VertArray.glDisableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glDisable GL.GL_TEXTURE_2D;

	GL.glClientActiveTexture GL.GL_TEXTURE1;
	VertArray.glDisableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glDisable GL.GL_TEXTURE_2D;

	GL.glClientActiveTexture GL.GL_TEXTURE2;
	VertArray.glDisableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glDisable GL.GL_TEXTURE_2D;

	GL.glActiveTexture GL.GL_TEXTURE0
	
      end
    else
    (*    VertArray.glDrawArrays GL.GL_POINTS 0 (nb_vertices*3);*)
      VertArray.glDrawElements0 GL.GL_QUADS (nb_faces*4) 
	VertArray.Elem.GL_UNSIGNED_INT

  method destroy =

    VBO.glUnbindBuffer VBO.GL_ARRAY_BUFFER;
    VBO.glUnbindBuffer VBO.GL_ELEMENT_ARRAY_BUFFER;

    VBO.glDeleteBuffer vboVertices;
    VBO.glDeleteBuffer vboTexture;
    VBO.glDeleteBuffer vboDetailMap;
    VBO.glDeleteBuffer vboNormals;
    VBO.glDeleteBuffer vboFaces;

    VertArray.glDisableClientState VertArray.GL_TEXTURE_COORD_ARRAY;
    VertArray.glDisableClientState VertArray.GL_NORMAL_ARRAY;
    VertArray.glDisableClientState VertArray.GL_VERTEX_ARRAY;


(* Call Garbage collector *)
    Gc.full_major();
    print_endline "VBO removed"
end

(* Display scene and fill screen *)
let drawScene screen (model:obj3D) textures =
  GL.glMatrixMode GL.GL_PROJECTION;
  GL.glLoadIdentity ();

  if !ortho then
    GL.glOrtho
      ~left:(-3. *. !zoom)
      ~right:(3. *. !zoom)
      ~bottom:(-3. *. !zoom /. !wratio)
      ~top:(3. *. !zoom /. !wratio)
      ~near:(-300.5 *. !zoom)
      ~far:(30.5 *. !zoom)
  else
    begin 
      Glu.gluPerspective 60.0 (!wratio) (0.1 *. !zoom) (300000. *. !zoom);
      Glu.gluLookAt 
	0. 0. (!pw *. !zoom /. 100.) 
	0. 0. (1. *. !zoom /. 100.) 
	0. 1. 0.;
    end;
  GL.glMatrixMode GL.GL_MODELVIEW;
  GL.glLoadIdentity ();
  GL.glColorMask true true true true;
  GL.glClearColor 0.2 0.2 0.2 1.;
  GL.glClear [GL.GL_COLOR_BUFFER_BIT;GL.GL_DEPTH_BUFFER_BIT];

  if !xrot < 0. && !ypos >= 0. then
    GL.glColor3 0. 0.4 0.8
  else
    GL.glColor3 1. 1. 1.;

  GL.glPushMatrix ();
  GL.glTranslatev (!intro_xpos, !intro_ypos, !intro_zpos);
  
  GL.glPushMatrix ();
  GL.glTranslatev (!xpos, !ypos+. !yDecal +. !intro_yDecal, !zpos);
  GL.glRotatev ~angle:!xrot ~vec:(1.,0.,0.);
  GL.glRotatev ~angle:!yrot ~vec:(0.,1.,0.);

  GL.glScalev (1., (!yScale *. !intro_yScale), 1.);
  begin if !anaglyph then 
    GL.glColorMask false true true false;
  end;
  GL.glLight 
    ~light:(GL.GL_LIGHT 0)
    ~pname:(GL.Light.GL_POSITION (!liX, !liY, !liZ, -1.));

  begin match !displayMode with
    | 0 -> GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_LINE;
    | e -> GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL;
  end;
  model#draw textures;

(* If stereomode actived, render the scene another time 
   with a translated camera *)
  if !anaglyph then
    begin
      GL.glClear [GL.GL_DEPTH_BUFFER_BIT];
      GL.glColorMask true false false false;
      if !ortho then
	begin
	  GL.glPopMatrix ();
	  GL.glPushMatrix ();
	  GL.glTranslatev (!xpos-.6., !ypos+. !yDecal +. !intro_yDecal, !zpos);
	  GL.glRotatev ~angle:!xrot ~vec:(1.,0.,0.);
	  GL.glRotatev ~angle:!yrot ~vec:(0.,1.,0.);
	  GL.glScalev (1., (!yScale *. !intro_yScale), 1.);
	end
      else
	begin
	  GL.glMatrixMode GL.GL_PROJECTION;
	  GL.glLoadIdentity ();
	  Glu.gluPerspective 60.0 (!wratio) (0.1 *. !zoom) (300000. *. !zoom);
	  Glu.gluLookAt 
	    3. 0. (!pw *. !zoom /. 100.) 
	    0. 0. (1. *. !zoom /. 100.) 
	    0. 1. 0.;
	  GL.glMatrixMode GL.GL_MODELVIEW;
    	end;
      model#draw textures;
      GL.glPopMatrix ();
      GL.glColorMask true true true true;
    end;
  GL.glPopMatrix ();

  begin (* other non time-critical objects *) 
    GL.glPushMatrix ();
    GL.glTranslatev (!xpos, !ypos, !zpos);
    GL.glRotatev ~angle:!xrot ~vec:(1.,0.,0.);
    GL.glRotatev ~angle:!yrot ~vec:(0.,1.,0.);
    
    let size = 20.*. !pw +. !ph
    and decal = float !internalTimer /. 9001. in (* OVER 9000 !! *)

    let lighting = GL.glIsEnabled GL.Enabled.GL_LIGHTING in
    GL.glDisable GL.GL_LIGHTING;

 (* Ground *)
    GL.glColor3 0. 0. 0.;
    GL.glBegin GL.GL_QUADS;
     GL.glVertex3 (-.size) (!yDecal-.10.) (-.size);
     GL.glVertex3 (-.size) (!yDecal-.10.) size;
     GL.glVertex3 size (!yDecal-.10.) size;
     GL.glVertex3 size (!yDecal-.10.) (-.size);
    GL.glEnd ();
    GL.glColor3 1. 1. 1.;

  
 (* SkyBox *)
    let p1 = 253. /. 1024. and p2 = 770. /. 1024. in 
    GL.glEnable GL.GL_TEXTURE_2D;
    enableTexture textures.(3);
    (* Top *)
    GL.glBegin GL.GL_QUADS;
     GL.glTexCoord2 p1 p2; GL.glVertex3 (-.size) size (-.size);
     GL.glTexCoord2 p2 p2; GL.glVertex3 size size (-.size);
     GL.glTexCoord2 p2 p1; GL.glVertex3 size size size;
     GL.glTexCoord2 p1 p1; GL.glVertex3 (-.size) size size;
    GL.glEnd ();

    (* Front *)
    GL.glBegin GL.GL_QUADS;
     GL.glTexCoord2 p1 p1; GL.glVertex3 (-.size) size size;
     GL.glTexCoord2 p2 p1; GL.glVertex3 size size size;
     GL.glTexCoord2 p2 0.; GL.glVertex3 size (-20.) size;
     GL.glTexCoord2 p1 0.; GL.glVertex3 (-.size) (-20.) size;
    GL.glEnd ();

    (* Back *)
    GL.glBegin GL.GL_QUADS;
     GL.glTexCoord2 p1 1.; GL.glVertex3 (-.size) (-20.) (-.size);
     GL.glTexCoord2 p2 1.; GL.glVertex3 size (-.20.) (-.size);
     GL.glTexCoord2 p2 p2; GL.glVertex3 size size (-.size);
     GL.glTexCoord2 p1 p2; GL.glVertex3 (-.size) size (-.size);
    GL.glEnd ();

    (* Right *)
    GL.glBegin GL.GL_QUADS;
     GL.glTexCoord2 p2 p2; GL.glVertex3 size size (-.size);
     GL.glTexCoord2 1. p2; GL.glVertex3 size (-.20.) (-.size);
     GL.glTexCoord2 1. p1; GL.glVertex3 size (-.20.) size;
     GL.glTexCoord2 p2 p1; GL.glVertex3 size size size;
    GL.glEnd ();

    (* Left *)
    GL.glBegin GL.GL_QUADS;
     GL.glTexCoord2 0. p2; GL.glVertex3 (-.size) (-20.) (-.size);
     GL.glTexCoord2 p1 p2; GL.glVertex3 (-.size) size (-.size);
     GL.glTexCoord2 p1 p1; GL.glVertex3 (-.size) size size;
     GL.glTexCoord2 0. p1; GL.glVertex3 (-.size) (-20.) size;
    GL.glEnd ();
 
(* Water *)
    GL.glEnable GL.GL_TEXTURE_2D;
    GL.glEnable GL.GL_BLEND;
    GL.glDisable GL.GL_CULL_FACE;
    GL.glBlendFunc GL.Sfactor.GL_ONE GL.Dfactor.GL_ONE;
    enableTexture textures.(2);

    GL.glBegin GL.GL_QUADS;
     GL.glTexCoord2 decal 0.; GL.glVertex3 (-.size) 2. (-.size);
     GL.glTexCoord2 (!pw+.decal) 0.; GL.glVertex3 (-.size) 2. size;
     GL.glTexCoord2 (!pw+.decal) !ph; GL.glVertex3 size 2. size;
     GL.glTexCoord2 decal !ph; GL.glVertex3 size 2. (-.size);
    GL.glEnd ();
    GL.glEnable GL.GL_CULL_FACE;    
    GL.glDisable GL.GL_BLEND;

    if lighting then GL.glEnable GL.GL_LIGHTING;
    GL.glDisable GL.GL_TEXTURE_2D;
    GL.glPopMatrix ();
  end;

  GL.glPopMatrix ();
  GL.glFlush ();
  Sdlgl.swap_buffers ()

let disableEverything () = 
  GL.glDepthFunc GL.GL_LESS;
  GL.glDisable GL.GL_CULL_FACE;
  GL.glDisable GL.GL_DEPTH_TEST;
  GL.glDisable GL.GL_TEXTURE_2D;
  GL.glDisable GL.GL_LIGHTING;
  GL.glDisable GL.GL_LIGHT0;
  GL.glDisable GL.GL_COLOR_MATERIAL
    

let toggleDisplayMode (n) =
  let max = 3 in
    displayMode := (if n < 0 && !displayMode = 0 then max - 1 
		    else (!displayMode + n) mod max);
    disableEverything ();
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

let wiggle max = 
  Random.self_init ();
  Random.float max

let animateIntro () = 
  begin if !intro_step <= 3 then
      begin
	begin match !intro_step with
	  | 0 -> 
	    xrot := -1.; yrot := -1.; zrot := 0.;
	    xpos := 0.; ypos := -30.; zpos := 0.;
	    zoom := !zoom /. 6.;
	    intro_yDecal := -40.;
	    intro_yScale := 0.1;
	    intro_step := !intro_step + 1
	  | 1 ->
	    if !intro_yDecal > -25. then
	      begin
		intro_wiggle := 10.;
		intro_step := !intro_step + 1
	      end
	  | 2 ->
	    if !intro_yDecal > 0. then
	      begin
		intro_wiggle := 5.;
		intro_step := !intro_step + 1
	      end
	  | _ ->
	    intro_wiggle := 0.;
	    intro_step := !intro_step + 1
	end;
	intro_xpos := wiggle (!intro_wiggle);
	intro_ypos := wiggle (!intro_wiggle/.2.);
	intro_zpos := wiggle (!intro_wiggle);
	intro_yScale := !intro_yScale *. 1.02;
	intro_yDecal := !intro_yDecal +. 0.5
      end
    else
      if !xrot > 35. then
	intro := false;
  end;
  xrot := !xrot +. 0.2


let rotateView xrel yrel =
  yrot := !yrot +. xrel;
  let newXrot = !xrot +. yrel in
    if newXrot < 90. && newXrot > -90. then xrot := newXrot

let panView xrel yrel =
  xpos := !xpos +. float xrel /. 150.0 *. !zoom;
  ypos := !ypos -. float yrel /. 150.0 *. !zoom

(* Here we handle all events *)
let rec mainLoop screen model textures =
  if !intro then animateIntro ();
  begin 
    let newTimer = Sdltimer.get_ticks () in
      if newTimer - !internalTimer > !refTimer then
	begin
	  internalTimer := newTimer;
	  drawScene screen model textures
	end
  end;

  begin match Sdlevent.poll () with
    | Some Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_ESCAPE}
    | Some Sdlevent.QUIT -> ()
    | Some event -> 
      begin match event with
	| Sdlevent.KEYDOWN k -> 
	  begin match k.Sdlevent.keysym with 
	    | Sdlkey.KEY_z -> toggleDisplayMode (1)
	    | Sdlkey.KEY_s -> toggleDisplayMode (-1)
	    | Sdlkey.KEY_q -> yScale := !yScale +. 0.1
	    | Sdlkey.KEY_w -> yScale := !yScale -. 0.1
	    | Sdlkey.KEY_f -> yDecal := !yDecal +. 1.
	    | Sdlkey.KEY_v -> yDecal := !yDecal -. 1.
	    | Sdlkey.KEY_a -> animate := not !animate
	    | Sdlkey.KEY_d -> anaglyph := not !anaglyph;
	      if !anaglyph then print_endline "IN 3 D !!!!"
	    | Sdlkey.KEY_t -> textured := not !textured;
	      toggleDisplayMode (0)
	    | Sdlkey.KEY_e -> ortho := not !ortho
	    | Sdlkey.KEY_UP -> rotateView 0. 5.
	    | Sdlkey.KEY_DOWN -> rotateView 0. (-5.)
	    | Sdlkey.KEY_LEFT -> rotateView 5. 0.
	    | Sdlkey.KEY_RIGHT -> rotateView (-5.) 0.
	    | _ -> ()
	  end;
	  intro := false
	  | Sdlevent.MOUSEMOTION e -> 
	    begin match e.Sdlevent.mme_state with
	      | [Sdlmouse.BUTTON_RIGHT] -> 
		rotateView 
		  (float e.Sdlevent.mme_xrel) 
		  (float e.Sdlevent.mme_yrel);
	      | [Sdlmouse.BUTTON_LEFT] ->
		panView 
		  e.Sdlevent.mme_xrel 
		  e.Sdlevent.mme_yrel;
	      | _ -> ();
	    end
	  | Sdlevent.MOUSEBUTTONDOWN b 
	      when b.Sdlevent.mbe_button = Sdlmouse.BUTTON_WHEELDOWN ->
	    zoom := !zoom *. 1.1;
	  | Sdlevent.MOUSEBUTTONDOWN b 
	      when b.Sdlevent.mbe_button = Sdlmouse.BUTTON_WHEELUP ->
	    zoom := !zoom /. 1.1;
	  | event -> ();
      end;
      mainLoop screen model textures

    | None -> 
      begin if !animate then
	  if not !anaglyph then
	    rotateView 0.1 0.
	  else  (* it takes two frames to animate with anaglyph *)
	    begin
	      rotateView 0.2 0.;
	      Sdltimer.delay (!refTimer/2)
	    end
      end;
      Sdltimer.delay (!refTimer);
      mainLoop screen model textures
  end

(* Setup the screen *)
let main obj tex heightmap fps =
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title:"AutoMap" ~icon:"";
  internalTimer := Sdltimer.get_ticks ();
  refTimer := 1000 / fps;
  let screen = 
    (if !fullscreen then 
	Sdlvideo.set_video_mode 0 0 [`FULLSCREEN; `OPENGL; `DOUBLEBUF]
     else
	Sdlvideo.set_video_mode 640 400 [`OPENGL; `DOUBLEBUF]) 
  in
    begin let (w, h, _) = Sdlvideo.surface_dims screen in
      wratio := (float w) /. (float h);
      ww := float w;
      wh := float h
    end;
    if !intro then
      begin
	intro_step := 0;
	displayMode := 2;
      end
    else
      displayMode := 0;
    toggleDisplayMode (0);
    let textures = loadTextures [|tex;
			       "images/DetailTex.jpg";
			       "images/water.jpg";
			       "images/SkyBox.jpg"|] in
    let filename = findObjFile obj in
      print_endline "Trying to load 3D model ...";       
      let model = new obj3D (countVerticesAndFaces (open_in filename)) in
      print_endline "Loading 3D model ...";
      model#load filename heightmap;
      print_endline "3D model successfuly loaded !";
      mainLoop screen model textures;
      model#destroy;
      disableEverything ();
      GL.glDeleteTextures textures;
      Sdl.quit ()

