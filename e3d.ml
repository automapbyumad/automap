(*------------------------------- 3D ENGINE ----------------------------------*)
(* Depencies :
#directory "+lablGL";;
#directory "+sdl";;
#load "lablgl.cma";;
#load "bigarray.cma";;
#load "sdl.cma";;
#load "sdlloader.cma";;
#load "str.cma";;
*)

let xrot = ref 0.0 and yrot = ref 0.0 and zrot = ref 0.0;;
let xpos = ref 0.0 and ypos = ref 0.0 and zpos = ref 0.0;;
let zoom = ref 1.;;
let displayMode = ref 1;;

(* Parses vertex found in the input OBJ file *)
let captureVertices inputFile nb_vertices =
  let vertices = Array.make nb_vertices (0., 0., 0.) in
  let rec captureNextVertice n =
    try
      match Str.split (Str.regexp_string " ") (input_line inputFile) with
	| ["v"; x; y; z] ->
	  Array.set vertices n ((float_of_string x),
				(float_of_string y),
				(float_of_string z));
	  captureNextVertice (n+1);
	| _ -> captureNextVertice n;
    with 
      | End_of_file -> close_in inputFile;
      | _ -> ();
  in
  captureNextVertice 0;
  vertices;
;;

(* Converts ["1"; "2"; "3"] in [1; 2; 3] 
   AND determines the maximum index number (here, 3)  *)
let rec newFace string_list newface max =
  match string_list with
    | [] -> (newface, max)
    | e::l -> 
      let i = int_of_string e in
      if i > max then
	newFace l (i::newface) i
      else
	newFace l (i::newface) max
;;

(* Parses faces found in the input OBJ file *)
(* Returns the face list and the maximum number of points of the object *)
let rec captureFaces inputFile accuList max = 
  try
    match Str.split (Str.regexp_string " ") (input_line inputFile) with
      | e::l when e = "f" -> 
	let (newface, newmax) = newFace l [] 0 in
	captureFaces
	  inputFile
	  (newface::accuList)
	  (if newmax > max then newmax else max)
      | _ -> captureFaces inputFile accuList max;
  with End_of_file -> close_in inputFile; (accuList, max);
;;

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
	exit 1;
      end
;;

class obj3D = object (self)
  val mutable faceList = []
  val mutable verticeArray = Array.make 0 (0., 0., 0.)
      
  method load filename = 
    let fname = findObjFile filename in
    let (f, nVertices) = captureFaces (open_in fname) [] 0 in
    verticeArray <- captureVertices (open_in fname) nVertices;
    faceList <- f;
  method faces = faceList
  method vertices = verticeArray
end
;;

let rec drawFace vertices = function
  | [] -> ();
  | e::l -> 
    let (x, y, z) = vertices.(e-1) in
    let c = (y+.1.0)/.2.0 in
    GlDraw.color (1., c, c);
    GlTex.coord2 (x, z);
    GlDraw.vertex3 (x, y, z);
    drawFace vertices l;
;;

(* Display scene and fill screen *)
let drawScene screen (model:obj3D) =
  GlMat.mode `projection;
  GlMat.load_identity ();

  GlMat.ortho 
    ~x:(-1.5 *. !zoom, 1.5 *. !zoom) 
    ~y:(-1.5 *. !zoom, 1.5 *. !zoom) 
    ~z:(-30.5 *. !zoom, 3.5 *. !zoom);
  GlMat.mode `modelview;
  GlMat.load_identity (); 
  GlClear.clear [ `color;`depth];

  GlMat.translate ~x:!xpos ~y:!ypos ~z:!zpos ();
  GlMat.rotate ~angle:!xrot ~x:1. ();
  GlMat.rotate ~angle:!yrot ~y:1. ();

  GlMat.scale ~x:0.5 ~y:0.5 ~z:0.5 ();
  
  List.iter (fun face -> 
	       begin match !displayMode with
		 | 0 | 1 -> GlDraw.polygon_mode `both `line;
		 | _ -> GlDraw.polygon_mode `both `fill;
	       end;
	       begin match List.length face with
		 | 3 -> GlDraw.begins `triangles;
		 | 4 -> GlDraw.begins `quads; 
		 | _ -> GlDraw.begins `polygon;
	       end;
	       drawFace model#vertices face;
	       GlDraw.ends ();
	    ) model#faces;

  Gl.flush ();
  Sdlgl.swap_buffers ();
;;

let toggleDisplayMode () =
  displayMode := (!displayMode + 1) mod 3;
  Gl.disable `cull_face;
  Gl.disable `depth_test;
  Gl.disable `texture_2d;
  print_string "Changed display mode to ";
  begin
    match !displayMode with
      | 0 -> print_endline "wireframe";
      | 1 -> 
	  GlFunc.depth_range ~near:0.1 ~far:10.0;
	  Gl.enable `texture_2d;
	  print_endline "textured wireframe";
      | 2 -> 
	  Gl.enable `cull_face;
	  GlDraw.cull_face `front; 
	  Gl.enable `depth_test;
	  GlFunc.depth_mask true;
	  GlFunc.depth_range ~near:0.1 ~far:10.0;
	  Gl.enable `texture_2d;
	  print_endline "textured solid";
      | _ -> ();
  end;
;;

(* Here we handle all events *)
let rec mainLoop screen model =
  drawScene screen model;
  match Sdlevent.wait_event () with
    | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_ESCAPE}
    | Sdlevent.QUIT -> Sdl.quit ();
    | event -> 
	begin match event with
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_z} -> toggleDisplayMode ();
	  | Sdlevent.MOUSEMOTION e 
	      when e.Sdlevent.mme_state = [Sdlmouse.BUTTON_RIGHT] ->
	      yrot := !yrot +. float e.Sdlevent.mme_xrel;
		let newXrot = !xrot +. float e.Sdlevent.mme_yrel in
		  if newXrot < 90. && newXrot > -90. then xrot := newXrot;
	  | Sdlevent.MOUSEMOTION e 
	      when e.Sdlevent.mme_state = [Sdlmouse.BUTTON_LEFT] ->
	      xpos := !xpos +. float e.Sdlevent.mme_xrel /. 150.0 *. !zoom;
		ypos := !ypos -. float e.Sdlevent.mme_yrel /. 150.0 *. !zoom;
	  | Sdlevent.MOUSEBUTTONDOWN b 
	      when b.Sdlevent.mbe_button = Sdlmouse.BUTTON_WHEELDOWN ->
	      zoom := !zoom *. 1.1;
	  | Sdlevent.MOUSEBUTTONDOWN b 
	      when b.Sdlevent.mbe_button = Sdlmouse.BUTTON_WHEELUP ->
	      zoom := !zoom /. 1.1;
	  | event -> ();
	      (* print_endline (Sdlevent.string_of_event event);*)
	end;
	mainLoop screen model;
;;

(* Setup the screen *)
let glscreen () =
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title:"AutoMap" ~icon:"";
  let screen = Sdlvideo.set_video_mode 500 500 [`OPENGL; `DOUBLEBUF] in
    
    toggleDisplayMode ();
    
    let green = GlTex.gen_texture () in
    let image = Sdlloader.load_image "temp.bmp" in
    let (w, h, _) = Sdlvideo.surface_dims image in
    let tex = Sdlgl.to_raw image in
      GlTex.bind_texture `texture_2d green;
      GlTex.image2d (GlPix.of_raw tex `rgb w h);
      
      GlTex.parameter `texture_2d (`min_filter `linear);
      GlTex.parameter `texture_2d (`mag_filter `linear);
      
      (*
	Gl.enable `lighting;
	Gl.enable `light0;
	GlLight.light ~num:0 (`position (1., -1., 1., 1.));
	GlLight.material `both (`shininess 100.);
	Gl.enable `color_material;
	GlLight.color_material `both `specular;
	GlLight.color_material `both `ambient_and_diffuse;
      *)
      
      (* let filename = (Sys.argv.(Array.length Sys.argv - 1)^".obj") in *)
      let filename = "test.obj" in
      let model = new obj3D in
	model#load filename;
	mainLoop screen model;
	
	Sdl.quit ();
;;
