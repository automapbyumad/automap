(*------------------------------ 80 characters -------------------------------*)

let _ = GMain.init ()

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

(*------------------------------ GLOBALS VARS --------------------------------*)
let step = ref 20
let list_color = ref []
let list_height = ref []
let array_tb = ref (Array.make 0 (GEdit.entry ()))
let border_clicked = ref false

(*------------------------ LABLGTK GRAPHIC INTERFACE -------------------------*)

(* Main window *)
let main_window = GWindow.window
  ~resizable:false
  ~title:"AutoMap"
  ~width:800
  ~height:600 ()

(* Vbox1 -contains- bbox1 & hbox0 *)
let vbox1 = GPack.vbox
  ~border_width:15
  ~packing:main_window#add ()

(* Bbox1 -contains- main buttons *)
let bbox1 = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~packing:(vbox1#pack ~expand:false) ()

(* Hbox0 -contains- bbox2 & image *)
let hbox0 = GPack.hbox
  ~border_width:20
  ~packing:(vbox1#pack ~expand:false) ()

(* Bbox2 -contains- filter buttons *)
let bbox2 = GPack.button_box `VERTICAL
  ~layout:`START
  ~border_width:10
  ~spacing:15
  ~packing:(hbox0#pack ~expand:false) ()

(* Main buttons *)
let btn_open = GFile.chooser_button
  ~action:`OPEN
  ~packing:bbox1#add ()

let btn_border = GButton.button
  ~label:"Border"
  ~packing:bbox1#add ()

let btn_grid = GButton.button
  ~label:"Grid"
  ~packing:bbox1#add ()

let btn_3d = GButton.button
  ~label:"Relief"
  ~packing:bbox1#add ()

let btn_save = GButton.button
  ~label:"Save"
  ~packing:bbox1#add ()
let save_image = GMisc.image
  ~stock:`SAVE
  ~packing:btn_save#set_image ()

let btn_settings = GButton.button
  ~label:"Settings"
  ~packing:bbox1#add ()
let settings_image = GMisc.image
  ~stock:`PREFERENCES
  ~packing:btn_settings#set_image ()

let btn_reset = GButton.button
  ~label:"Reset"
  ~packing:bbox1#add ()
let reset_image = GMisc.image
  ~stock:`REFRESH
  ~packing:btn_reset#set_image ()

(* Filter buttons *)
let btn_mediumx = GButton.button
  ~label:"Quick Medium"
  ~packing:bbox2#add ()

let btn_medium = GButton.button
  ~label:"Medium Blur"
  ~packing:bbox2#add ()

let btn_gaussian = GButton.button
  ~label:"Gaussian Blur"
  ~packing:bbox2#add ()

let btn_canny = GButton.button
  ~label:"Canny Border"
  ~packing:bbox2#add ()

let btn_fusion = GButton.button
  ~label:"Fusion Canny"
  ~packing:bbox2#add ()

(* Image *)
let image_box = GMisc.image
  ~packing:hbox0#add ()
  
(* Settings window *)
let settings_window = GWindow.window
  ~title:"Settings"
  ~resizable:false
  ~position:`CENTER
  ~width:280
  ~height:360 ()

(* Vbox2 -contains- hbox2, vbox3, bbox2 *)
let vbox2 = GPack.vbox
  ~packing:settings_window#add ()

(* Hbox2 -contains- Grid step input *)
let hbox2 = GPack.hbox
  ~packing:vbox2#add ()
 
let label_gridstep = GMisc.label
  ~text:"Grid step : "
  ~packing:hbox2#add ()
  
let input_gridstep = GEdit.entry 
  ~max_length:3
  ~packing:hbox2#add ()

let label_px = GMisc.label
  ~text:" px."
  ~packing:hbox2#add ()

(* Vbox3 -contains- Height <=> Color input *)
let vbox3 = GPack.vbox
  ~packing:vbox2#add ()

(* Bbox2 -contains- Ok button *)
let bbox2 = GPack.button_box `HORIZONTAL
  ~packing:vbox2#add () 

let btn_ok = GButton.button
  ~packing:bbox2#add ()
let ok_image = GMisc.image
  ~stock:`OK
  ~packing:btn_ok#set_image ()

(*------------------------------ MAIN FUNCTIONS ------------------------------*)

(* Border *)
let is_color_in_range c1 c2 range =
  let (r1, g1, b1) = c1 and (r2, g2, b2) = c2 in
    r1 <= r2 + range && r1 >= r2 - range &&
    g1 <= g2 + range && g1 >= g2 - range &&
    b1 <= b2 + range && b1 >= b2 - range

let print_border src dst =
  let seuil = 20 in
  let x, y, z = Sdlvideo.surface_dims src in
    for i = 0 to x-1 do
	for j = 0 to y-1 do
	  let pix1 = Sdlvideo.get_pixel_color src i j in
	  let pix2 = Sdlvideo.get_pixel_color src (i+1) j in
	  let pix3 = Sdlvideo.get_pixel_color src i (j+1) in
	    if pix1 <> pix2 && not(is_color_in_range pix1 pix2 seuil) then
	      begin
		if not(List.exists (fun x -> x = pix1) !list_color) then
		  list_color := (pix1::!list_color);
		Sdlvideo.put_pixel_color dst i j Sdlvideo.black  
	      end;
	    if pix1 <> pix3 && not(is_color_in_range pix1 pix3 seuil) then
	      begin
		if not(List.exists (fun x -> x = pix1) !list_color) then
		  list_color := (pix1::!list_color);
		Sdlvideo.put_pixel_color dst i j Sdlvideo.black
	      end;
	done;
      done;
    dst

(* Grid *)
(* e = step *)
let print_grid src dst e =
  let w, h, _ = Sdlvideo.surface_dims src in
    for y=0 to (h/e)-1 do
      for x=0 to (w/e)-1 do
	for i=0 to e do
	  Sdlvideo.put_pixel_color dst ((x*e)+i) (y*e) Sdlvideo.black;
	  Sdlvideo.put_pixel_color dst (x*e) ((y*e)+i) Sdlvideo.black;
	  Sdlvideo.put_pixel_color dst ((x*e)+i) ((y*e)+i) Sdlvideo.black;
	  Sdlvideo.put_pixel_color dst ((x*e)+i) (((y+1)*e)-i) Sdlvideo.black;
	  Sdlvideo.put_pixel_color dst (((x+1)*e)) ((y*e)+i) Sdlvideo.black;
	  Sdlvideo.put_pixel_color dst ((x*e)+i) ((y+1)*e) Sdlvideo.black;
	done;
      done;
    done;
  dst
    
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
    
let upperL x y n = 2*y*n+x-y+1
let upperR x y n = (upperL x y n)+1
let mid x y n = (2*y+1)*n+x-y+1
let bottomL x y n = (2*y+2)*n+x-y
let bottomR x y n = (bottomL x y n)+1 

let indexof list x =
  let rec indexofrec list x acc = match list with
    | [] -> -1
    | e::l when e=x -> acc
    | e::l -> indexofrec l x acc+1 in
    indexofrec list x 0

let get_height color = List.nth !list_height (indexof !list_color color)

let trace_points step w h output_file img =
  let file = open_out output_file in 
    for y=0 to (h/step)*2 do
      if (y mod 2 == 0) then
	for x=0 to (w/step) do
	  let height =
	    get_height (Sdlvideo.get_pixel_color img (x*step) (y/2*step)) in
	    write_vertex 
	      (float (x*step)/.float w) 
	      (float height /. 300.) 
	      (float (y/2*step)/.float h) 
	      file
	done
      else
	for x=0 to (w/step)-1 do
	  let height =
	    get_height (Sdlvideo.get_pixel_color img (x*step) (y/2*step)) in
	    write_vertex 
	      ((float (x*step) +. (float step) /. 2.) /. float w)
	      (float height /. 300.)
	      ((float (y/2*step) +. (float step) /. 2.) /. float h)
	      file;
	done;
    done;
    let n = (w/step) in
      for y=0 to (h/step)-1 do
	for x=0 to n-1 do
	  write_face
	    (upperL x y (n+1)) (mid x y (n+1)) (upperR x y (n+1)) file;
	  write_face
	    (upperR x y (n+1)) (mid x y (n+1)) (bottomR x y (n+1)) file;
	  write_face
	    (bottomR x y (n+1)) (mid x y (n+1)) (bottomL x y (n+1)) file;
	  write_face
	    (bottomL x y (n+1)) (mid x y (n+1)) (upperL x y (n+1)) file;
	done;
      done;
      close_out file

(* Misc - Filters *)
let gaussianMask = [|
  [| 2; 4; 5; 4; 2 |]; 
  [| 4; 9; 12; 9; 4 |]; 
  [| 5; 12; 15; 12; 5 |];
  [| 4; 9; 12; 9; 4 |];
  [| 2; 4; 5; 4; 2 |]
|]

let rec pow x = function
  | 0 -> 1
  | n -> x * pow x (n-1)

let multi_color color coef =
  let (r, g, b) = color in
    (r*coef, g*coef, b*coef)

let div_color color coef =
  let (r, g, b) = color in 
    (r/coef, g/coef, b/coef)

let somme_color (r1, g1, b1) (r2, g2, b2) =
  (r1+r2, g1+g2, b1+b2)

(* Gaussian blur *)
let apply_gaussian_mask src dst = 
  let x, y, z = Sdlvideo.surface_dims src in
  let color = ref (Sdlvideo.get_pixel_color src 0 0) in
    for j = 2 to (y-2) do 
      for i = 2 to (x-2) do
	color := Sdlvideo.black;
	for offsety = -2 to 2 do
	  for offsetx = -2 to 2 do
	    let colorcoef = multi_color
	      (Sdlvideo.get_pixel_color src (i+offsetx) (j+offsety)) 
	      (gaussianMask.(offsety+2)).(offsetx+2) in
	      color := somme_color !color colorcoef;
	  done
	done;
	color := div_color !color 159;
	Sdlvideo.put_pixel_color dst i j !color;
      done
    done;
    dst

(* Medium blur *)
let apply_medium src dst step =
  let x, y, z = Sdlvideo.surface_dims src in
  let color = ref (Sdlvideo.black) in
    for j = 0 to (y-1) do
      for i = 0 to (x-1) do
	color := Sdlvideo.black;
	let nb_pix = ref 0 in
	  for stepy = max (-j) (-step) to min (y-1-j) (step) do
	    for stepx = max (-i) (-step) to min (x-1-i) (step) do
	      color :=
		somme_color !color
		  (Sdlvideo.get_pixel_color src (i+stepx) (j+stepy));
	      nb_pix := !nb_pix + 1;
	    done 
	  done;
	  color := div_color !color !nb_pix;
	  Sdlvideo.put_pixel_color dst i j !color;
      done
    done;
    dst

(* Medium Quick blur *)
let apply_medium_quick src dst step = 
  let x, y, z = Sdlvideo.surface_dims src in
  let color = ref (Sdlvideo.black) in
    for j = 0 to (y-1) do
      for i = 0 to (x-1) do
	color := Sdlvideo.black;
	let nb_pix = ref 0 in
	  for stepx = max (-i) (-step) to min (x-1-i) (step) do
	    color := somme_color 
	      !color 
	      (Sdlvideo.get_pixel_color src (i+stepx) j);
	    nb_pix := !nb_pix + 1;
	  done;
	  for stepy = max (-j) (-step) to min (y-1-j) (step) do
	    color := somme_color 
	      !color 
	      (Sdlvideo.get_pixel_color src i (j+stepy));
	    nb_pix := !nb_pix + 1;
	  done;
	  color := div_color !color !nb_pix;
	  Sdlvideo.put_pixel_color dst i j !color;
      done
    done;
    dst

(* Sobel Mask  *)
let sobelMaskX = [|
  [| -1; 0; 1 |]; 
  [| -2; 0; 2 |]; 
  [| -1; 0; 1 |]
|]

let sobelMaskY = [|
  [| 1; 2; 1 |]; 
  [| 0; 0; 0 |]; 
  [| -1; -2; -1 |]
|]

let grey color =
  let (r, g, b) = color in
    (r + g + b) / 3

let neighbour_white src x y =
  let return = ref false in
    for j = -1 to 1 do
      for i = -1 to 1 do
	if i <> 0 || j <> 0 then
	  if Sdlvideo.get_pixel_color src (x+i) (y+j) = Sdlvideo.white then
	    return := true
      done
    done;
    !return 

let apply_fusion src src2 dst = 
  let x, y, z = Sdlvideo.surface_dims src in
    for j = 0 to (y-1) do
      for i = 0 to (x-1) do
	if Sdlvideo.get_pixel_color src2 i j = Sdlvideo.white then
	  Sdlvideo.put_pixel_color dst i j Sdlvideo.black
	done
      done;
      dst

let apply_sobel_mask src dst =
  let x, y, z = Sdlvideo.surface_dims src in
    for j = 0 to (y-1)  do 
      for i = 0 to (x-1) do
	let grad_v = ref 0 and grad_h = ref 0 in
	  for offsety = max (-j) (-1) to min (y-1-j) 1 do
	    for offsetx = max (-i) (-1) to min (x-1-i) 1 do
	      let color_grey = 
		grey (Sdlvideo.get_pixel_color src (i+offsetx) (j+offsety)) in
		grad_v :=
		  !grad_v + color_grey * (sobelMaskX.(offsety+1)).(offsetx+1);
		grad_h :=
		  !grad_h + color_grey * (sobelMaskY.(offsety+1)).(offsetx+1);
	    done
	  done;
	  let length = sqrt (float_of_int (pow !grad_v 2 + pow !grad_h 2)) in
	    if length < 40. then
	      Sdlvideo.put_pixel_color dst i j Sdlvideo.black
	    else
	      begin
		if length > 45. then
		  Sdlvideo.put_pixel_color dst i j Sdlvideo.white
		else
		  begin
		    if neighbour_white dst i j then 
		      Sdlvideo.put_pixel_color dst i j Sdlvideo.white
		    else
		      Sdlvideo.put_pixel_color dst i j Sdlvideo.black
		  end
	      end
      done
    done;
    dst

(* Misc Functions *)

let rec generate_height = function 
  | -1 -> []
  | n -> (n*10)::generate_height (n-1)

let save_as src path =
  Sdlvideo.save_BMP src path

let save path =
  Sdlvideo.save_BMP (Sdlloader.load_image "temp.bmp") path
    
let get_string = function
  | Some x -> x
  | _ -> raise Not_found

let recup_int input () =
  let text = input_gridstep#text in
  try
    step := int_of_string text;
  with
    | _ -> step := 20

let generate_obj w h pas img =
  trace_points pas w h "test.obj" img

(* Height <> Colors textboxes *)
let triple2string (r,g,b) =
  "(" ^ string_of_int r ^ ", " ^
    string_of_int g ^ ", " ^
    string_of_int b ^ ")"

let generate_textbox () =
  if not(!border_clicked) then
      let length = List.length !list_color in
	array_tb := Array.make length (GEdit.entry ());
      for i=0 to length-1 do
	let hboxt = GPack.hbox ~packing:vbox3#add () in
	let lablt = GMisc.label ~text:(triple2string (List.nth !list_color i))
	  ~packing:hboxt#add () in
	let entryt = GEdit.entry ~packing:hboxt#add () in
	  ignore(lablt);
	  !array_tb.(i) <- entryt;
	  border_clicked := true;
      done

(* Button functions *)
let on_reset src =
  save_as src "temp.bmp";
  image_box#set_file "temp.bmp";
  ()

let on_border src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (print_border src dst) "temp.bmp";
    image_box#set_file "temp.bmp";
    (* ignore(generate_textbox ()); *)
    list_height := generate_height ((List.length !list_color)-1);
    let fichier = open_out "cfg.txt" in
    for i = 0 to (List.length !list_color)-1 do
      output_string fichier ((triple2string(List.nth !list_color i))^" "^(string_of_int(List.nth !list_height i))^"\n");
    done;
    close_out fichier;
    ()
      
let on_grid src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (print_grid src dst (!step)) "temp_grid.bmp";
    image_box#set_file "temp_grid.bmp";
    ()

let on_relief src =
  let (w, h, _) = Sdlvideo.surface_dims src in
    generate_obj w h !step src;
    glscreen ();
    ()

let on_mediumx src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (apply_medium_quick src dst 1) "temp.bmp";
    image_box#set_file "temp.bmp";
    ()

let on_medium src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (apply_medium src dst 1) "temp.bmp";
    image_box#set_file "temp.bmp";
    ()
  
let on_gaussian src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (apply_gaussian_mask src dst) "temp.bmp";
    image_box#set_file "temp.bmp";
    ()

let on_canny src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (apply_sobel_mask src dst) "canny.bmp";
    image_box#set_file "canny.bmp";
    ()

let on_fusion src =
  let dst = Sdlloader.load_image "temp.bmp" in
  let src2 = Sdlloader.load_image "canny.bmp" in
    save_as (apply_fusion src src2 dst) "temp.bmp";
    image_box#set_file "temp.bmp";
    ()

(* ================================= MAIN =================================== *)
let sdl_launch () = 
  let path = get_string btn_open#filename in
  let src = Sdlloader.load_image path in
    image_box#set_file path;
    save_as src "temp.bmp";
    ignore(btn_reset#connect#clicked
	     ~callback:(fun _ -> on_reset src));
    ignore(btn_border#connect#clicked
	     ~callback:(fun _ -> on_border src));
    ignore(btn_grid#connect#clicked
	     ~callback:(fun _ -> on_grid src)); 
    ignore(btn_save#connect#clicked
	     ~callback:(fun _ -> save "resultat.bmp"));
    ignore(btn_3d#connect#clicked
	     ~callback:(fun _ -> on_relief src));
    ignore(btn_mediumx#connect#clicked
	     ~callback:(fun _ -> on_mediumx src));
    ignore(btn_medium#connect#clicked
	     ~callback:(fun _ -> on_medium src));
    ignore(btn_gaussian#connect#clicked
	     ~callback:(fun _ -> on_gaussian src));
    ignore(btn_canny#connect#clicked
	     ~callback:(fun _ -> on_canny src));
    ignore(btn_fusion#connect#clicked
	     ~callback:(fun _ -> on_fusion src));
    ()
      
(* Main *)
let _ =
  ignore(main_window#connect#destroy
	   ~callback:GMain.quit);
  ignore(input_gridstep#connect#changed
	   ~callback:(recup_int input_gridstep));
  ignore(settings_window#event#connect#delete
	   ~callback:(fun _ -> settings_window#misc#hide (); true));
  ignore(btn_ok#connect#clicked
	   ~callback:(fun _ -> settings_window#misc#hide ()));
  ignore(btn_settings#connect#clicked
	   ~callback:(fun _ -> settings_window#misc#show ()));
  ignore(btn_open#connect#selection_changed (fun _ -> sdl_launch ()));
  main_window#show ();
  GMain.main ()
