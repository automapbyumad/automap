let _ = GMain.init ()

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

let fontName = "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf";;
let xrot = ref 0.0 and yrot = ref 0.0 and zrot = ref 0.0;;
let xpos = ref 0.0 and ypos = ref 0.0 and zpos = ref 0.0;;
let zoom = ref 1.0;;
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
    GlTex.coord2 (z, x);
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
    ~z:(-3.5 *. !zoom, 3.5 *. !zoom);
  GlMat.mode `modelview;
  GlMat.load_identity (); 
  GlClear.clear [ `color;`depth];

  GlMat.translate ~x:!xpos ~y:!ypos ~z:!zpos ();
  GlMat.rotate ~angle:!xrot ~x:1. ();
  GlMat.rotate ~angle:!yrot ~y:1. ();

  GlMat.scale ~x:0.5 ~y:0.5 ~z:0.5 ();
  
  List.iter (fun face -> 
	       begin match !displayMode with
		 | 0 -> GlDraw.polygon_mode `both `line;
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

let resetGlEnable () =
  Gl.disable `cull_face;
  Gl.disable `depth_test;
  Gl.disable `texture_2d;
;;

let enableShadedMode () =
  Gl.enable `cull_face;
  GlDraw.cull_face `front; 
  Gl.enable `depth_test;
  GlFunc.depth_mask true;
  GlFunc.depth_range ~near:0.1 ~far:10.0;
  Gl.enable `texture_2d;
;;

(* Here we handle all events *)
let rec mainLoop screen model =
  drawScene screen model;
  match Sdlevent.wait_event () with
    | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_ESCAPE}
    | Sdlevent.QUIT -> Sdl.quit ();
    | event -> 
	begin match event with
	  | Sdlevent.KEYDOWN {Sdlevent.keysym=Sdlkey.KEY_z} ->
	      displayMode := (!displayMode + 1) mod 2;
	      resetGlEnable ();
	      print_string "Changed display mode to ";
	      begin
		match !displayMode with
		  | 0 -> print_endline "wireframe";
		  | 1 -> enableShadedMode (); print_endline "solid";
		  | _ -> ();
	      end;
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
    
    enableShadedMode ();
    
    let green = GlTex.gen_texture () in
    let image = Sdlloader.load_image "green.jpg" in
    let (w, h, _) = Sdlvideo.surface_dims image in
    let tex = Sdlgl.to_raw image in
      GlTex.bind_texture `texture_2d green;
      GlTex.image2d (GlPix.of_raw tex `rgb w h);
      
      GlTex.parameter `texture_2d (`wrap_s `clamp);
      GlTex.parameter `texture_2d (`wrap_t `clamp);
      
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

let contour image  =
  let x, y, z = Sdlvideo.surface_dims image in
    for i = 0 to x-1 do
      for j = 0 to y-1 do
	if ((Sdlvideo.get_pixel_color image i j) <> (Sdlvideo.get_pixel_color image (i+1) j)) then
          Sdlvideo.put_pixel_color image i j Sdlvideo.black;  
	if ((Sdlvideo.get_pixel_color image i j) <>  (Sdlvideo.get_pixel_color image i (j+1) )) then
          Sdlvideo.put_pixel_color image i j Sdlvideo.black;
      done;
      done
	
let rec sdlInit image  =
  Sdl.init [`VIDEO];
  let (w, h, p) = Sdlvideo.surface_dims image in
  let screen = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
  let position = Sdlvideo.rect 0 0 w h in
    Sdlvideo.blit_surface ~dst_rect:position ~src:image ~dst:screen ();
    Sdlvideo.flip screen 
	   
let printContour image =
  contour image

let printQuad image h =
  let a, b, c = Sdlvideo.surface_dims image in
    for y=0 to (b/h)-1 do
      for x=0 to (a/h)-1 do
	for i=0 to h do
	  Sdlvideo.put_pixel_color image ((x*h)+i) (y*h) Sdlvideo.black;
	  Sdlvideo.put_pixel_color image (x*h) ((y*h)+i) Sdlvideo.black;
	  Sdlvideo.put_pixel_color image ((x*h)+i) ((y*h)+i) Sdlvideo.black;
	  Sdlvideo.put_pixel_color image ((x*h)+i) ((y*h)+h-i) Sdlvideo.black;
	done;
      done;
    done
    
let write_vertex x y z file =
  output_string file ("v " ^ 
			(string_of_float x) ^ " " ^
			(string_of_float y) ^ " " ^
			(string_of_float z) ^ "\n")
    
let write_face v1 v2 v3 file =
  output_string file ("f " ^
			(string_of_int v1) ^ " " ^
			(string_of_int v2) ^ " " ^
			(string_of_int v3) ^ "\n")
    
(* let write_face v1 v2 v3 v4 file =
   output_string file ("f " ^
   (string_of_int v1) ^ " " ^
   (string_of_int v2) ^ " " ^
   (string_of_int v3) ^ " " ^
   (string_of_int v4) ^ "\n") *)
    
let upperL x y n = 2*y*n+x-y+1
let upperR x y n = (upperL x y n)+1
let mid x y n = (2*y+1)*n+x-y+1
let bottomL x y n = (2*y+2)*n+x-y
let bottomR x y n = (bottomL x y n)+1 
  
(* let upperL x y n = y*n+x+1
   let upperR x y n = y*n+x+2
   let bottomL x y n = (y+1)*n+x+1
   let bottomR x y n = (y+1)*n+x+2 *)

let trace_points hp w h outputFile =
  let file = open_out outputFile in 
    for y=0 to (h/hp)*2 do
      if (y mod 2 == 0) then
	begin
	  for x=0 to (w/hp) do
	    write_vertex (float (x*hp)) 0. (float (y/2*hp)) file;
	  done;
	end
      else
	begin
	  for x=0 to (w/hp)-1 do
	    write_vertex 
	      (float (x*hp) +. (float hp) /. 2.) 
	      0.
	      (float (y/2*hp) +. (float hp) /. 2.) 
	      file;
	  done;
	end
    done;
    let n = (w/hp) in
      for y=0 to (h/hp)-1 do
	for x=0 to n-1 do
	  write_face (upperL x y (n+1)) (mid x y (n+1)) (upperR x y (n+1)) file;
	  write_face (upperR x y (n+1)) (mid x y (n+1)) (bottomR x y (n+1)) file;
	  write_face (bottomR x y (n+1)) (mid x y (n+1)) (bottomL x y (n+1)) file;
	  write_face (bottomL x y (n+1)) (mid x y (n+1)) (upperL x y (n+1)) file;
	done;
      done;
      close_out file
	    
let printImage image =
  sdlInit image
    
let save image =
  Sdlvideo.save_BMP image "resultat.bmp"
    
let get_string = function 
  | Some x -> x
  | _ -> raise Not_found

let window = GWindow.window
  ~resizable:false
  ~title:"AutoMap"
  ~width:800
  ~height:600 ()

let vbox = GPack.vbox
  ~border_width:15
  ~packing:window#add ()

let bbox = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~packing:(vbox#pack ~expand:false) ()

let pic_borderline = GButton.button
  ~label:"Contours"
  ~packing:bbox#add ()

let pic_cut = GButton.button
  ~label:"Quadrillage"
  ~packing:bbox#add ()

let pic_3d = GButton.button
  ~label:"Relief"
  ~packing:bbox#add ()

let open_pic = GFile.chooser_button
    ~action:`OPEN
    ~packing:bbox#add ()

let save_pic = GButton.button
  ~label:"Sauvegarder"
  ~packing:bbox#add ()

let image_pic = GMisc.image
    ~packing:vbox#add ()

let generate_obj w h =
  trace_points 20 w h "test.obj"
          
let sdlLaunch openpic picborder picquad savepic pic3d () = 
  let path = get_string openpic#filename in
  let image = Sdlloader.load_image path in
    image_pic#set_file path;
    picborder#connect#clicked ~callback:(fun _ -> printContour image;
					   save image;
					   image_pic#set_file
					     "resultat.bmp");
    picquad#connect#clicked ~callback:(fun _ -> printQuad image 20;
					 save image;
					 image_pic#set_file "resultat.bmp"); 
    savepic#connect#clicked ~callback:(fun _ -> save image);
    pic3d#connect#clicked ~callback:(fun _ -> let (w, h, _) =
				       Sdlvideo.surface_dims image in
				       generate_obj w h;
				       glscreen ());
    ()


let _ =
  window#connect#destroy ~callback:GMain.quit;
  open_pic#connect#selection_changed (sdlLaunch open_pic pic_borderline pic_cut
					save_pic pic_3d);
  window#show ();
  GMain.main ()
