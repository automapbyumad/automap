(*------------------------------ 80 characters -------------------------------*)

let _ = GMain.init ()

(*------------------------------ GLOBALS VARS --------------------------------*)
let step = ref 20
let list_color = ref []
let list_height = ref (Array.make 0 0)
let image_x = ref max_int and image_y = ref max_int 
let image_w = ref max_int and image_h = ref max_int
let firstime = ref false
let temp = ref (Obj.magic 0)
let src = ref (Obj.magic 0)
let refdiv_value = ref 5
let can_paint = ref false
(*------------------------ LABLGTK GRAPHIC INTERFACE -------------------------*)

(* Main window *)
let main_window = GWindow.window
  ~resizable:true
  ~icon:(GdkPixbuf.from_file "images/icon.png")
  ~title:"AutoMap"
  ~position:`CENTER
  ~width:1100
  ~height:675 ()

(* Vbox1 -contains- bbox1 & hbox0 *)
let vbox1 = GPack.vbox
  ~border_width:0
  ~packing:main_window#add ()

(* Bbox1 -contains- main buttons *)
let bbox1 = GPack.button_box `HORIZONTAL
  ~layout:`START
  ~packing:(vbox1#pack ~expand:false) ()

(* Toolbar *)
let toolbar = GButton.toolbar
  ~orientation:`HORIZONTAL
  ~style:`BOTH
  ~tooltips:true
  ~height:60
  ~width:1100
  ~packing:(bbox1#pack ~expand:true) ()

let btn_open = GButton.tool_button
  ~label:"Open"
  ~stock:`OPEN
  ~expand:true
  ~packing:toolbar#insert ()

let open_dialog = GWindow.file_selection
  ~title:"Open a picture"
  ~parent:main_window
  ~position:`CENTER_ON_PARENT
  ~destroy_with_parent:true
  ~type_hint:`TOOLBAR
  ~width:650
  ~height:450 ()

let btn_save = GButton.tool_button
  ~label:"Save"
  ~stock:`SAVE
  ~expand:true
  ~packing:toolbar#insert ()

let btn_reset = GButton.tool_button
  ~label:"Reset"
  ~stock:`REFRESH
  ~expand:true
  ~packing:toolbar#insert ()

let btn_noise = GButton.tool_button
  ~label:"Clear Noise"
  ~stock:`CLEAR
  ~expand:true
  ~packing:toolbar#insert ()

let btn_border = GButton.tool_button
  ~label:"Border"
  ~stock:`REMOVE
  ~expand:true
  ~packing:toolbar#insert ()

let btn_canny = GButton.tool_button
  ~label:"Canny"
  ~stock:`CUT
  ~expand:true
  ~packing:toolbar#insert ()

let btn_gaussian = GButton.tool_button
  ~label:"Gaussian"
  ~stock:`FULLSCREEN
  ~expand:true
  ~packing:toolbar#insert ()

let btn_3d = GButton.tool_button
  ~label:"SimpleQuad"
  ~stock:`CONVERT
  ~expand:true
  ~packing:toolbar#insert ()

let btn_quadtree = GButton.tool_button
  ~label:"QuadTree"
  ~stock:`CONVERT
  ~expand:true
  ~packing:toolbar#insert ()

let btn_relief = GButton.tool_button
  ~label:"3D"
  ~stock:`CONVERT
  ~expand:true
  ~packing:toolbar#insert ()

let btn_about = GButton.tool_button
  ~label:"About"
  ~stock:`DIALOG_QUESTION
  ~expand:true
  ~packing:toolbar#insert ()

let dialog = GWindow.about_dialog
  ~authors:["Team UMAD"]
  ~copyright: "Copyright © 2011-2012 UMAD"
  ~version:"2.0"
  ~website:"http://umad.fr.nf"
  ~website_label:"Website"
  ~destroy_with_parent:true ()    

let btn_quit = GButton.tool_button
  ~label:"Quit"
  ~stock:`QUIT
  ~expand:true
  ~packing:toolbar#insert ()

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

(* Image *)
let image_box = GMisc.image
  ~packing:hbox0#add ()	

let canny_adjust = GData.adjustment
  ~lower:0.
  ~upper:254.
  ~step_incr:1.
  ~page_size:0. ()

let coef_adjust = GData.adjustment
  ~lower:1.
  ~upper:255.
  ~step_incr:1.
  ~page_size:0. ()
 
let hbox_canny = GPack.hbox
  ~show:false
  ~packing:vbox1#add ()
let _ = GPack.hbox
  ~packing:hbox_canny#add ()

let lbl_canny = GMisc.label
  ~text:"Gray component"
  ~show:false
  ~packing:hbox_canny#add ()
let canny_slider = GRange.scale `HORIZONTAL
  ~digits:0
  ~adjustment:canny_adjust
  ~show:false
  ~packing:hbox_canny#add ()

let auto_fill = GButton.check_button
    ~label:"Auto Fill"
    ~active:true
    ~show:false
    ~packing:vbox1#add ()

let lbl_coef = GMisc.label
  ~text:"Coefficent"
  ~show:false
  ~packing:hbox_canny#add ()
let coef_slider = GRange.scale `HORIZONTAL
  ~digits:0
  ~adjustment:coef_adjust
  ~show:false
  ~packing:hbox_canny#add ()

let hbox_canny2 = GPack.hbox
  ~packing:hbox_canny#add ()
let btn_finalize = GButton.button
  ~label:"Finalize"
  ~show:false
  ~packing:hbox_canny2#add ()
 
(*------------------------------ MAIN FUNCTIONS ------------------------------*)

(* Border *)
let is_color_in_range c1 c2 range =
  let (r1, g1, b1) = c1 and (r2, g2, b2) = c2 in
    r1 <= r2 + range && r1 >= r2 - range &&
    g1 <= g2 + range && g1 >= g2 - range &&
    b1 <= b2 + range && b1 >= b2 - range

let rec list_invert = function
  | [] -> []
  | e::l -> (list_invert l)@[e]

exception Too_many_colors
  
let print_border src =
  let seuil = 30 in
  let x, y, z = Sdlvideo.surface_dims src in
  let dst = Sdlvideo.create_RGB_surface_format src [] x y in
  let _ = list_color := [] in
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
	end
      else
	if pix1 <> pix3 && not(is_color_in_range pix1 pix3 seuil) then
	  begin
	    if not(List.exists (fun x -> x = pix1) !list_color) then
	      list_color := (pix1::!list_color);
	    Sdlvideo.put_pixel_color dst i j Sdlvideo.black
	  end
	else
	  Sdlvideo.put_pixel_color dst i j pix1
    done;
  done;
  list_color := list_invert !list_color;
  list_height := Array.init (List.length !list_color) (fun i -> i*20);
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

let get_height color = 
  if !list_color <> []  then
    !list_height.(indexof !list_color color)
  else
    let (r,g,b) = color in
    int_of_float
      (0.3 *. (float r) 
      +. 0.59 *. (float g) 
      +. 0.11 *. (float b))


let trace_points step w h output_file img =
  let file = open_out output_file in 
    for y=0 to (h/step)*2 do
      if (y mod 2 == 0) then
	for x=0 to (w/step) do
	  let height =
	    get_height (Sdlvideo.get_pixel_color img (x*step) (y/2*step)) in
	    write_vertex 
	      (float (x*step) -. (float w /.2.))
	      (float height) 
	      (float (y/2*step) -. (float h /. 2.)) 
	      file
	done
      else
	for x=0 to (w/step)-1 do
	  let height =
	    get_height (Sdlvideo.get_pixel_color img (x*step) (y/2*step)) in
	    write_vertex 
	      ((float (x*step) +. (float step) /. 2.) -. (float w /. 2.))
	      (float height)
	      ((float (y/2*step) +. (float step) /. 2.) -. (float h /. 2.))
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
let pi = 4.0 *. atan 1.0

let rec pow x = function
  | 0 -> 1
  | n -> x * pow x (n-1)

(* c'est moche mais bon pour eviter de faire bloquer tout le programme *)
let rec pow_f x = function
  | 0 -> 1.
  | n -> x *. pow_f x (n-1)

let gaussian_pixel x y sigma =
  (1. /. (2. *. pi *. (pow_f sigma 2))) *. 
    exp(-.(((pow_f x 2) +. (pow_f y 2)) /. (2. *. (pow_f sigma 2))))

let generate_gaussian step nb sigma =
  let mask = Array.make (pow nb 2) 0. in
  let pos = ref 0 in
  for j = -step to step do
    for i = -step to step do
      pos := (i + step) + (j + step) * nb;
      mask.(!pos) <- gaussian_pixel (float_of_int i) (float_of_int j) sigma;
    done
  done;
  mask

let multi_color color coef =
  let (r, g, b) = color in
  (r * coef, g * coef, b * coef)

let multi_color_f color coef =
  let (r, g, b) = color in
  ((float_of_int r) *. coef, (float_of_int g) *. coef, (float_of_int b) *.coef)

let div_color color coef =
  let (r, g, b) = color in 
  (r / coef, g / coef, b / coef)

let div_color_f color coef =
  let (r, g, b) = color in 
  (int_of_float (r /. coef), int_of_float (g /. coef), int_of_float (b /. coef))

let somme_color (r1, g1, b1) (r2, g2, b2) =
  (r1 + r2, g1 + g2, b1 + b2)

let somme_color_f (r1, g1, b1) (r2, g2, b2) =
  (r1 +. r2, g1 +. g2, b1 +. b2)

let pair_int_of_float (r, g, b) =
  (int_of_float r, int_of_float g, int_of_float b)

(* Misc Functions *)

let save_as src path =
  Sdlvideo.save_BMP src path

let save_tmp dst =
  Sdlvideo.save_BMP dst "temp.bmp"

let save path =
  Sdlvideo.save_BMP (Sdlloader.load_image "temp.bmp") path

let get_string = function
  | Some x -> x
  | _ -> raise Not_found

let recup_int input =
  let text = input#text in
  try
    step := int_of_string text;
  with
    | _ -> step := 20

let recup_float input =
  let text = input#text in
  try
    float_of_string text;
  with
    | _ -> 0.

let recup input =
  let text = input#text in
  try
    int_of_string text;
  with
    | _ -> 0

let generate_obj w h pas img =
  trace_points pas w h "test.obj" img

(* Gaussian blur *)
let apply_gaussian_mask src step sigma = 
  let nb = 2 * step + 1 in
  let mask = generate_gaussian step nb sigma in
  let x, y, z = Sdlvideo.surface_dims src in
  let dst = Sdlvideo.create_RGB_surface_format src [] x y in
  let color = ref (0., 0., 0.) in
    for j = 0 to (y-1) do 
      for i = 0 to (x-1) do
	color := (0., 0., 0.);
	let sum_coef = ref 0. in
	for offsety = max (-j) (-step) to min (y-1-j) step do
	  for offsetx = max (-i) (-step) to min (x-1-i) step do
	    let coef = mask.((offsetx + step) + (offsety + step) * nb) in
	    let colorcoef = multi_color_f
	      (Sdlvideo.get_pixel_color src (i+offsetx) (j+offsety))
	      coef in
	    color := somme_color_f !color colorcoef;
	    sum_coef := !sum_coef +. coef;
	  done
	done;
	Sdlvideo.put_pixel_color dst i j (div_color_f !color !sum_coef);
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
      
let neighbour_white src x y w h =
  let return = ref false in
  for j = -1 to 1 do
    for i = -1 to 1 do
      let n_x = (x+i) in
      let n_y = (y+j) in
      if n_x >= 0 && n_x < w && n_y >= 0 && n_y < h && (i <> 0 || j <> 0) then
	if Sdlvideo.get_pixel_color src n_x n_y = Sdlvideo.white then
	  return := true
    done
  done;
  !return

let apply_sobel_mask src low_treshold high_treshold =
  let x, y, z = Sdlvideo.surface_dims src in
  let dst = Sdlvideo.create_RGB_surface_format src [] x y in
  for j = 0 to (y-1) do 
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
      if length < low_treshold then
	Sdlvideo.put_pixel_color dst i j Sdlvideo.black
      else
	begin
	  if length > high_treshold then
	    Sdlvideo.put_pixel_color dst i j Sdlvideo.white
	  else
	    begin
	      if neighbour_white dst i j x y then
		Sdlvideo.put_pixel_color dst i j Sdlvideo.white
	      else
		Sdlvideo.put_pixel_color dst i j Sdlvideo.black
	    end
	end
    done
  done;
  dst

let find_no_mark mark =
  let no_modif = Sdlloader.load_image "mark.bmp" in
  let w, h, p = Sdlvideo.surface_dims mark in
  let c = ref Sdlvideo.black in
  let a = ref 0 and b = ref 0 and nb = ref 0 and nb_max = ref 0 in
  for j = 0 to (h-1) do
    for i = 0 to (w-1) do
      if Sdlvideo.get_pixel_color no_modif i j = Sdlvideo.white then
	begin
	  a := i;
	  while Sdlvideo.get_pixel_color no_modif !a j = Sdlvideo.white do
	    a := !a + 1;
	    nb := !nb + 1;
	  done;
	  nb_max := !nb;
	  nb := 0;
	  b := i;
	  while Sdlvideo.get_pixel_color no_modif !b j = Sdlvideo.white do
	    b := !b - 1;
	    nb := !nb + 1;
	  done;
	  if !nb_max >= !nb then
	    c := Sdlvideo.get_pixel_color no_modif !a j
	  else
	    c := Sdlvideo.get_pixel_color no_modif !b j;
	  Sdlvideo.put_pixel_color mark i j !c;
	end
    done
  done;
  mark

let build_color_areas_table src coef =
  let color_areas = (Hashtbl.create 83) in
  let canny = Sdlloader.load_image "canny.bmp" in
  save_as canny "area_buffer.bmp";
  let area_buffer = Sdlloader.load_image "area_buffer.bmp"
  and (w, h, _) = Sdlvideo.surface_dims src in  
  let rec fill_rec x y color =
    let c = Sdlvideo.get_pixel_color area_buffer x y in
    Sdlvideo.put_pixel_color area_buffer x y color;
    for j = -1 to 1 do
      for i = -1 to 1 do
	if i <> 0 || j <> 0 then
	  begin
	    if x+i > 0 && x+i < w - 1 
	      && y+j > 0 && y+j < h - 1
	      && c = Sdlvideo.get_pixel_color area_buffer (x+i) (y+j) then
	      fill_rec (x+i) (y+j) color;
	  end;
      done
    done
  in
  for y = 1 to h-2 do
    for x = 1 to w-2 do
      if Sdlvideo.get_pixel_color area_buffer x y <> Sdlvideo.white then
	begin
	  fill_rec x y Sdlvideo.white;	  
	  let (r,g,b) = Sdlvideo.get_pixel_color src x y in
	  let key = (r/coef, g/coef, b/coef) in
	  Hashtbl.add color_areas key (x,y);
	end;
    done;
  done;
  Sys.remove "area_buffer.bmp";
  color_areas

(* Noise Reduction *)

let is_in_range src tgt h =
  let (r, g, b) = src in
  let (rt, gt, bt) = tgt in
  (abs (r-rt) < h) &&
    (abs (g-gt) < h) &&
    (abs (b-bt) < h)

let max tbl = 
  let max_color = ref (0,0,0) in
  let max = ref 0 in
  let func (r,g,b) v =
    if v>(!max) then
      begin
	max_color := (r,g,b);
	max := v
      end; () in
  Hashtbl.iter func tbl;
  !max_color
    
let unnoise img step =
  let (w, h, _) = Sdlvideo.surface_dims img in
  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
  let hash_table = Hashtbl.create ((2*step+1)*(2*step+1)) in
  for y=0 to (h-1) do
    for x=0 to (w-1) do
      for j=step downto (-step) do
	for i=step downto (-step) do
	  if ( x-i >= 0 && y-j >= 0 && x-i < w && y-j < h) then 
	    begin
	      let color = Sdlvideo.get_pixel_color img (x-i) (y-j) in
	      if (Hashtbl.mem hash_table color) then
		let nb = Hashtbl.find hash_table color in
		Hashtbl.replace hash_table color (nb+1)
	      else
		Hashtbl.add hash_table color 1
	    end
	done
      done;
      Sdlvideo.put_pixel_color dst x y (max hash_table);
      Hashtbl.clear hash_table
    done
  done;
  dst

let reduct img step =
  let (w, h, _) = Sdlvideo.surface_dims img in
  let dst = Sdlvideo.create_RGB_surface_format img []
    (int_of_float (ceil ((float_of_int w) /. (float_of_int step))))
    (int_of_float (ceil ((float_of_int h) /. (float_of_int step)))) in
  let (wdst, hdst, _) = Sdlvideo.surface_dims dst in
  let hash_table = Hashtbl.create ((2*step+1)*(2*step+1)) in
  for yimg=0 to (hdst-1) do
    for ximg=0 to (wdst-1) do
      let y=(yimg*step) and x=(ximg*step) in
      for j=step downto (-step) do
	for i=step downto (-step) do
	  if ( x-i >= 0 && y-j >= 0 && x-i < w && y-j < h) then 
	    begin
	      let color = Sdlvideo.get_pixel_color img (x-i) (y-j) in
	      if (Hashtbl.mem hash_table color) then
		let nb = Hashtbl.find hash_table color in
		Hashtbl.replace hash_table color (nb+1)
	      else
		Hashtbl.add hash_table color 1
	    end
	done
      done;
      Sdlvideo.put_pixel_color dst ximg yimg (max hash_table);
      Hashtbl.clear hash_table
    done
  done;
  dst

let reform img step=
  let (w, h, _) = Sdlvideo.surface_dims img in
  let dst = Sdlvideo.create_RGB_surface_format img [] (w*step+1) (h*step+1) in
  for y=0 to (h-1) do
    for x=0 to (w-1) do
      let color = Sdlvideo.get_pixel_color img x y in
      for j=0 to (2*step+1) do
	for i=0 to (2*step+1) do
	  let ry=(y*step) and rx=(x*step) in
	  Sdlvideo.put_pixel_color dst (rx+i) (ry+j) color
	done
      done
    done
  done;
  dst

let rec contains_near color range = function
  | [] -> false
  | e::l when (is_in_range color e range) -> true
  | e::l -> contains_near color range l

let build_max_color n tbl range =
  let rec aux i n list tbl range =
    if (i=n) then list
    else
    let i_max = max tbl in
    if not(contains_near i_max range list) then
      begin
	Hashtbl.remove tbl i_max;
	aux (i+1) n (i_max::list) tbl range
      end
    else
      begin
	Hashtbl.remove tbl i_max;
	aux i n list tbl range
      end in
  aux 0 n [] tbl range
  

let n_max img step n range =
  let (w, h, _) = Sdlvideo.surface_dims img in
  let tbl = Hashtbl.create (w*h) in
  for y=0 to (h-1) do
    for x=0 to (w-1) do
      let color = Sdlvideo.get_pixel_color img x y in
      if (Hashtbl.mem tbl color) then
	let nb = Hashtbl.find tbl color in
	Hashtbl.replace tbl color (nb+1)
      else
	Hashtbl.add tbl color 1
    done
  done;
  build_max_color n tbl range

let nearest color list = 
  let rec aux min min_color (r,g,b) = function
    | [] -> min_color
    | (rt,gt,bt)::l ->
      let somme = abs(r-rt)+abs(g-gt)+abs(b-bt) in
      if (somme < min) then
	aux somme (rt, gt, bt) (r,g,b) l
      else
	aux min min_color (r,g,b) l in
  aux (255*3) (255,255,255) color list
      

let replace_near img step list =
  let (w, h, _) = Sdlvideo.surface_dims img in
  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
  for y=0 to (h-1) do
    for x=0 to (w-1) do
      begin
	let color = Sdlvideo.get_pixel_color img x y in
	let new_color = nearest color list in
	Sdlvideo.put_pixel_color dst x y new_color
      end
    done
  done;
  dst

(* 2D Quad Tree *)
let vline src dst xs ys ye black=
  for y=ys to ye do
    if (black) then
      Sdlvideo.put_pixel_color dst xs y (Sdlvideo.black)
    else
      Sdlvideo.put_pixel_color dst xs y (Sdlvideo.get_pixel_color src xs y)
  done
let hline src dst xs ys xe black =
  for x=xs to xe do
    if (black) then
      Sdlvideo.put_pixel_color dst x ys (Sdlvideo.black)
    else
      Sdlvideo.put_pixel_color dst x ys (Sdlvideo.get_pixel_color src x ys)
  done

let is_same_color img (xs, ys, xe, ye) =
  let tbl = Hashtbl.create ((xe-xs)*(xe-xs)) in
  for y=ys to ye do
    for x=xs to xe do
      let color = Sdlvideo.get_pixel_color img x y in
      if ( not(Hashtbl.mem tbl color) ) then
	Hashtbl.add tbl color 0
    done
  done;
  Hashtbl.length tbl == 1
    
      
let rec quadtree img s (x, y, xe, ye) subdiv dst invert =
  if not(subdiv = 0) && not(is_same_color img (x, y, xe, ye)) then
    begin
      let size = int_of_float ((float_of_int s) /. 2.) in
      ignore(quadtree img size (x, y, (x+size), (y+size)) (subdiv-1) dst invert);
      ignore(quadtree img size ((x+size), y, (xe), (y+size)) (subdiv-1) dst invert);
      ignore(quadtree img size (x, (y+size), (x+size), (ye)) (subdiv-1) dst invert);
      ignore(quadtree img size ((x+size), (y+size), (xe), (ye)) (subdiv-1) dst invert);
      
      let black = not(invert) in
      vline img dst x y ye black;
      vline img dst (x+size) y ye black;
      vline img dst xe y ye black;
      hline img dst x y xe black;
      hline img dst x (y+size) xe black;
      hline img dst x ye xe black;
    end;
  dst
    
(* Ma fonction fill new *)
let fill canny dst x y color =
  let mark = Sdlloader.load_image "mark.bmp" in
  let w, h, p = Sdlvideo.surface_dims canny in
  let rec fill_rec x y =
    Sdlvideo.put_pixel_color canny x y Sdlvideo.white;
    Sdlvideo.put_pixel_color mark x y color;
    Sdlvideo.put_pixel_color dst x y color;
    let a = ref 0 and b = ref 0 in
    for j = -1 to 1 do
      for i = -1 to 1 do
	if i <> 0 || j <> 0 then
	  begin
	    a := x+i;
	    b := y+j;
	    if !a > 0 && !a < w-1 && !b > 0 && !b < h-1 
	      && Sdlvideo.get_pixel_color canny !a !b = Sdlvideo.black then
	      fill_rec !a !b;
	  end;
      done
    done
  in
  fill_rec x y;
  save_as mark "mark.bmp"
(*;  dst*)

(* Height <> Colors textboxes *)
let triple2string (r,g,b) =
  "(" ^ string_of_int r ^ ", " ^
    string_of_int g ^ ", " ^
    string_of_int b ^ ")"

(* Settings Windows *)
let settings_noise src =
  let window1 = GWindow.window
    ~title:"Noise reduction settings"
    ~position:`CENTER
    ~show:true
    ~resizable:false
    ~width:400
    ~height:250 () in
  let vbox1 = GPack.vbox
    ~packing:window1#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~text:"Treshold"
    ~packing:hbox1#add () in
  let adjust = GData.adjustment
    ~lower:1.
    ~upper:8.
    ~step_incr:1.
    ~page_size:0. () in
  let treshold = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust
    ~packing:hbox1#add () in
  let hbox2 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~text:"Color depth"
    ~packing:hbox2#add () in
  let adjust2 = GData.adjustment
    ~lower:1.
    ~upper:16.
    ~step_incr:1.
    ~page_size:0. () in
  let nb_color = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust2
    ~packing:hbox2#add () in
  let hbox3 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~text:"Color range"
    ~packing:hbox3#add () in
  let adjust3 = GData.adjustment
    ~lower:0.
    ~upper:255.
    ~step_incr:1.
    ~page_size:0. () in
  let range = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust3
    ~packing:hbox3#add () in
  let bbox1 = GPack.button_box `HORIZONTAL
    ~layout:`SPREAD
    ~packing:vbox1#add () in
  let btn_cancel = GButton.button
    ~label:"Cancel"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`CLOSE
    ~packing:btn_cancel#set_image () in
  let btn_ok = GButton.button
    ~label:"Apply"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`APPLY
    ~packing:btn_ok#set_image () in
  let _ = treshold#adjustment#set_value 3. in
  let _ = nb_color#adjustment#set_value 8. in 
  let _ = range#adjustment#set_value 20. in ();
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ ->
      let treshold_value = int_of_float (treshold#adjustment#value) in
      let nb_color_value = int_of_float (nb_color#adjustment#value) in
      let range_value = int_of_float (range#adjustment#value) in
      let dst = unnoise src treshold_value in
      let color_list = n_max (reduct dst treshold_value) treshold_value nb_color_value range_value in
      save_tmp (replace_near dst treshold_value color_list);
      temp := Sdlloader.load_image "temp.bmp";
      image_box#set_file "temp.bmp";
      window1#destroy ())

let settings_gaussian src =
  let window1 = GWindow.window
    ~title:"Gaussian blur settings"
    ~position:`CENTER
    ~show:true
    ~resizable:false
    ~width:400
    ~height:250 () in
  let vbox1 = GPack.vbox
    ~packing:window1#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~width:8
    ~text:"Sigma coefficient"
    ~packing:hbox1#add () in
  let adjust = GData.adjustment
    ~lower:0.
    ~upper:100.
    ~step_incr:0.1
    ~page_size:0. () in
  let adjust2 = GData.adjustment
    ~lower:1.
    ~upper:5.
    ~step_incr:1.
    ~page_size:0. () in
  let sigma_coef = GRange.scale `HORIZONTAL
    ~digits:1
    ~adjustment:adjust
    ~packing:hbox1#add () in
  let hbox2 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~width:8
    ~text:"Step"
    ~packing:hbox2#add () in
  let step = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust2
    ~packing:hbox2#add () in
  let bbox1 = GPack.button_box `HORIZONTAL
    ~layout:`SPREAD
    ~packing:vbox1#add () in
  let btn_cancel = GButton.button
    ~label:"Cancel"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`CLOSE
    ~packing:btn_cancel#set_image () in
  let btn_ok = GButton.button
    ~label:"Apply"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`APPLY
    ~packing:btn_ok#set_image () in
  let _ = sigma_coef#adjustment#set_value 1. in
  let _ = step#adjustment#set_value 2. in ();
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ -> let sigma_value = sigma_coef#adjustment#value in
			let step_value = int_of_float (step#adjustment#value) in
			save_tmp (apply_gaussian_mask src step_value sigma_value);
			temp := Sdlloader.load_image "temp.bmp";
			image_box#set_file "temp.bmp";
			window1#destroy ())

let settings_canny src =
  let window1 = GWindow.window
    ~title:"Canny settings"
    ~position:`CENTER
    ~show:true
    ~resizable:false
    ~width:400
    ~height:250 () in
  let vbox1 = GPack.vbox
    ~packing:window1#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~width:8
    ~text:"Low Treshold"
    ~packing:hbox1#add () in
  let adjust = GData.adjustment
    ~lower:0.
    ~upper:255.
    ~step_incr:1.
    ~page_size:0. () in
  let adjust2 = GData.adjustment
    ~lower:0.
    ~upper:255.
    ~step_incr:1.
    ~page_size:0. () in
  let lowtreshold = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust
    ~packing:hbox1#add () in
  let hbox2 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~width:8
    ~text:"High Treshold"
    ~packing:hbox2#add () in
  let hightreshold = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust2
    ~packing:hbox2#add () in
  let bbox1 = GPack.button_box `HORIZONTAL
    ~layout:`SPREAD
    ~packing:vbox1#add () in
  let btn_cancel = GButton.button
    ~label:"Cancel"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`CLOSE
    ~packing:btn_cancel#set_image () in
  let btn_ok = GButton.button
    ~label:"Apply"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`APPLY
    ~packing:btn_ok#set_image () in
  ignore(lowtreshold#adjustment#set_value 20.);
  ignore(hightreshold#adjustment#set_value 45.);
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ ->	let low_treshold = lowtreshold#adjustment#value in
			let high_treshold = hightreshold#adjustment#value in
			let canny = apply_sobel_mask src low_treshold high_treshold in
			save_as canny "canny.bmp";
			let mark = Sdlloader.load_image "temp.bmp" in
			let w, h, p = Sdlvideo.surface_dims mark in
			for i = 0 to (w-1) do
			  Sdlvideo.put_pixel_color mark i 0 Sdlvideo.black;
			done;
			for i = 0 to (w-1) do
			  Sdlvideo.put_pixel_color mark i (h-1) Sdlvideo.black;
			done;
			for j = 0 to (h-1) do
			  Sdlvideo.put_pixel_color mark 0 j Sdlvideo.black;
			done;
			for j = 0 to (h-1) do
			  Sdlvideo.put_pixel_color mark (h-1) j Sdlvideo.black;
			done;
			for j = 1 to (h-2) do
			  for i = 1 to (w-2) do
			    Sdlvideo.put_pixel_color mark i j Sdlvideo.white;
			  done
			done;
			save_as mark "mark.bmp";
			image_box#set_file "canny.bmp"; can_paint := true;
			window1#destroy (); hbox_canny#misc#show ();
			lbl_canny#misc#show (); canny_slider#misc#show ();
			lbl_coef#misc#show (); coef_slider#misc#show ();
			auto_fill#misc#show ();
			hbox_canny2#misc#show (); btn_finalize#misc#show ())
    
let generate_cfg () =
  let fichier = open_out "cfg.txt" in
  for i = 0 to (List.length !list_color)-1 do
    output_string fichier ((triple2string(List.nth !list_color i)) ^ " "
			   ^ (string_of_int(!list_height.(i)))^"\n");
  done;
  close_out fichier


let settings_quadtree () =
  let window1 = GWindow.window
    ~position:`CENTER
    ~title:"QuadTree settings"
    ~show:true
    ~width:450
    ~height:(500)
    ~resizable:false () in
  let vbox00 = GPack.vbox
    ~packing:window1#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox00#add () in
  let bbox1 = GPack.button_box `HORIZONTAL
    ~layout:`SPREAD
    ~packing:vbox00#add () in
  let btn_cancel = GButton.button
    ~label:"Cancel"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`CLOSE
    ~packing:btn_cancel#set_image () in
  let btn_ok = GButton.button
    ~label:"Apply"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`APPLY
    ~packing:btn_ok#set_image () in
  let vbox2 = GPack.vbox
    ~packing:hbox1#add () in
  let frame_quadtree = GBin.frame
    ~label:"QuadTree"
    ~border_width:10
    ~packing:vbox2#add () in
  let vbox_fqt = GPack.vbox
    ~packing:frame_quadtree#add () in
  let _ = GMisc.label 
    ~text:"Subdivisions"
    ~packing:vbox_fqt#add () in
  let adjust_subdiv = GData.adjustment
    ~lower:1.
    ~upper:13.
    ~step_incr:1.
    ~page_size:0. () in
  let subdiv = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust_subdiv
    ~packing:vbox_fqt#add () in
  let invert = GButton.check_button
    ~label:"Invert colors"
    ~active:false
    ~packing:vbox_fqt#add () in
   ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
   subdiv#adjustment#set_value 6.;
   invert#set_active false;
   btn_ok#connect#clicked
     ~callback:(fun _ ->
       let invert_bool = invert#active in
       refdiv_value := (int_of_float subdiv#adjustment#value);
       let src = Sdlloader.load_image "temp.bmp" in
       let dst = Sdlloader.load_image "temp.bmp" in
       let (w, h, _) = Sdlvideo.surface_dims dst in
       if (invert_bool) then
	 for y=0 to (h-1) do
	   for x=0 to (w-1) do
	     Sdlvideo.put_pixel_color dst x y Sdlvideo.black
	   done
	 done;
       save_as (quadtree src w (0, 0, (w-1), (w-1)) !refdiv_value dst invert_bool)
"temp_qt.bmp";
       image_box#set_file "temp_qt.bmp";
       window1#destroy ();
       generate_cfg ();
      QuadTree.quadTree "test.obj" "temp.bmp" !refdiv_value 0)
       (*E3D.main "test.obj" "temp.bmp" 30*) 

let settings_3d () =
  let window1 = GWindow.window
    ~position:`CENTER
    ~title:"3D settings"
    ~show:true
    ~width:450
    ~height:(500)
    ~resizable:false () in
  let vbox00 = GPack.vbox
    ~packing:window1#add () in
  let frame_3dsc = GBin.frame
    ~label:"3D Screen"
    ~border_width:10
    ~packing:vbox00#add () in
  let vbox_3dsc = GPack.vbox
    ~packing:frame_3dsc#add () in
  let _ = GMisc.label 
    ~text:"FPS"
    ~packing:vbox_3dsc#add () in
  let adjust_fps = GData.adjustment
    ~lower:1.
    ~upper:30.
    ~step_incr:1.
    ~page_size:0. () in
  let fps_slide = GRange.scale `HORIZONTAL
    ~digits:0
    ~adjustment:adjust_fps
    ~packing:vbox_3dsc#add () in
  let shadowmap = GButton.check_button
    ~label:"Shadow map"
    ~active:(!E3D.shadowMap)
    ~packing:vbox_3dsc#add () in
  let fullscreen = GButton.check_button
    ~label:"Fullscreen"
    ~active:(!E3D.fullscreen)
    ~packing:vbox_3dsc#add () in
  let introd = GButton.check_button
    ~label:"Intro Animation"
    ~active:(!E3D.intro)
    ~packing:vbox_3dsc#add () in
  let in3D = GButton.check_button
    ~label:"Anaglyph 3D (red/cyan) - d"
    ~active:(!E3D.anaglyph)
    ~packing:vbox_3dsc#add () in
  let textured = GButton.check_button
    ~label:"Enable Textures - t"
    ~active:(!E3D.textured)
    ~packing:vbox_3dsc#add () in
  let ortho = GButton.check_button
    ~label:"Orthonormal view - e"
    ~active:(!E3D.ortho)
    ~packing:vbox_3dsc#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox_3dsc#add () in
  let _ = GMisc.label
    ~text:"Y Scale - q/w"
    ~packing:hbox1#add () in
  let yScale = GEdit.entry
    ~packing:hbox1#add () in
  let hbox2 = GPack.hbox
    ~packing:vbox_3dsc#add () in
  let _ = GMisc.label
    ~text:"Y Height - f/v"
    ~packing:hbox2#add () in
  let yDecal = GEdit.entry
    ~packing:hbox2#add () in

  let bbox1 = GPack.button_box `HORIZONTAL
    ~layout:`SPREAD
    ~packing:vbox00#add () in
  let btn_cancel = GButton.button
    ~label:"Cancel"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`CLOSE
    ~packing:btn_cancel#set_image () in
  let btn_ok = GButton.button
    ~label:"Apply"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`APPLY
    ~packing:btn_ok#set_image () in
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
   fps_slide#adjustment#set_value 30.;
   yScale#set_text (string_of_float !E3D.yScale);
   yDecal#set_text (string_of_float !E3D.yDecal);
   btn_ok#connect#clicked
     ~callback:(fun _ ->
       let fps_val = int_of_float fps_slide#adjustment#value in
       E3D.shadowMap := shadowmap#active;
       E3D.fullscreen := fullscreen#active;
       E3D.intro := introd#active;
       E3D.anaglyph := in3D#active;
       E3D.textured := textured#active;
       E3D.ortho := ortho#active;
       E3D.yScale := recup_float yScale;
       E3D.yDecal := recup_float yDecal;      
       E3D.main "test.obj" "temp.bmp" fps_val)
       
let settings_sampling src = 
  let nb_colors = List.length !list_color in
  let window1 = GWindow.window
    ~position:`CENTER
    ~title:"Sampling settings"
    ~show:true
    ~width:450
    ~height:(30 * nb_colors + 100)
    ~resizable:false () in
  let vbox00 = GPack.vbox
    ~packing:window1#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox00#add () in
  let _ = GPack.vbox
    ~packing:hbox1#add () in
  let vbox0 = GPack.vbox
    ~packing:hbox1#add () in
  let _ = GPack.vbox
    ~packing:hbox1#add () in
  let vbox1 = GPack.vbox
    ~packing:vbox0#add () in
  let hbox1 = GPack.hbox
    ~packing:vbox1#add () in
  let _ = GMisc.label
    ~text:"Grid Step "
    ~packing:hbox1#add () in
  let step_box = GEdit.entry
    ~packing:hbox1#add () in
  let _ = GMisc.label
    ~text:" px"
    ~packing:hbox1#add () in
  let textbox_array = Array.make nb_colors (GEdit.entry ())  in
  for i=0 to (nb_colors - 1) do
    let tb = GEdit.entry
      ~packing:vbox1#add () in
    let (r, g, b) = List.nth !list_color i in
    let color = Printf.sprintf "#%02x%02x%02x" r g b in
    tb#misc#modify_base [`NORMAL, `NAME color];
    textbox_array.(i) <- tb;
  done;
  for i=0 to (nb_colors - 1) do
    textbox_array.(i)#set_text (string_of_int !list_height.(i))
  done;
  let bbox1 = GPack.button_box `HORIZONTAL
    ~layout:`SPREAD
    ~packing:vbox00#add () in
  let btn_cancel = GButton.button
    ~label:"Cancel"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`CLOSE
    ~packing:btn_cancel#set_image () in
  let btn_ok = GButton.button
    ~label:"Apply"
    ~packing:bbox1#add () in
  let _ = GMisc.image
    ~stock:`APPLY
    ~packing:btn_ok#set_image () in
  step_box#set_text "20";
   ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
   btn_ok#connect#clicked
     ~callback:(fun _ ->
       let (w, h, _) = Sdlvideo.surface_dims src in
       recup_int step_box;
       let dst = Sdlloader.load_image "temp.bmp" in
       save_as (print_grid src dst (!step)) "temp_grid.bmp";
       image_box#set_file "temp_grid.bmp";
       if (nb_colors > 0) then
	 begin
	   list_height := Array.make nb_colors 0;
	   for i=0 to (nb_colors - 1) do
	     !list_height.(i) <- (recup textbox_array.(i));
	   done;
	 end;
       generate_obj w h !step src;
       window1#destroy ())

(* Button functions *)
let on_reset src =
  list_color := [];
  save_as src "temp.bmp";
  image_box#set_file "temp.bmp";
  canny_slider#misc#hide ();
  lbl_canny#misc#hide ();
  auto_fill#misc#hide ();
  coef_slider#misc#hide ();
  lbl_coef#misc#hide ();
  hbox_canny#misc#hide ();
  hbox_canny2#misc#hide ();
  btn_finalize#misc#hide ();
  can_paint := false;
  ()

let on_fill src x y =
  let dst = Sdlloader.load_image "temp.bmp"
  and canny = Sdlloader.load_image "canny.bmp"
  and cv = int_of_float canny_slider#adjustment#value
  and coef = int_of_float coef_slider#adjustment#value in
  let (r,g,b) = Sdlvideo.get_pixel_color !src x y in
  let key = (r/coef, g/coef, b/coef) in
  if auto_fill#active then
    begin
      List.iter 
	(fun (x',y') -> fill canny dst x' y' (cv,cv,cv)) 
	(Hashtbl.find_all 
	   (build_color_areas_table !src 
	      (int_of_float coef_slider#adjustment#value))
	   key)
    end
  else
    fill canny dst x y (cv,cv,cv);
  save_as dst "temp.bmp";
  image_box#set_file "temp.bmp";
  ()

let on_border src =
  save_as (print_border src) "border_tmp.bmp";
  image_box#set_file "border_tmp.bmp";
  ()
     
let on_relief src =
  let _ = settings_sampling src in ()

let on_noise src =
  let _ = settings_noise src in ()

let on_gaussian src =
  let _ = settings_gaussian src in ()

let on_canny src =
  let _ = settings_canny src in ()

let on_quadtree () =
  let _ = settings_quadtree () in ()

let on_3D () =
  let _ = settings_3d () in ()
(* ================================= MAIN =================================== *)
let sdl_launch () = 
  let path = open_dialog#filename in
  let (w, h, _) = Sdlvideo.surface_dims (Sdlloader.load_image path) in
  src := Sdlloader.load_image path;
  image_box#set_ypad 0;
  image_box#set_file path;
  save_tmp !src;
  temp := Sdlloader.load_image "temp.bmp";
  ignore(btn_border#connect#clicked
	     ~callback:(fun _ -> on_border (Sdlloader.load_image "temp.bmp")));
  image_x := (GtkBase.Widget.allocation image_box#as_widget).Gtk.x +
    ((GtkBase.Widget.allocation image_box#as_widget).Gtk.width - w)/2;
  image_w := (GtkBase.Widget.allocation image_box#as_widget).Gtk.x +
    (GtkBase.Widget.allocation image_box#as_widget).Gtk.width +
    ((GtkBase.Widget.allocation image_box#as_widget).Gtk.width - w)/2;
  image_y := (GtkBase.Widget.allocation image_box#as_widget).Gtk.y +
    ((GtkBase.Widget.allocation image_box#as_widget).Gtk.height - h)/2;
  image_h := (GtkBase.Widget.allocation image_box#as_widget).Gtk.y +
    (GtkBase.Widget.allocation image_box#as_widget).Gtk.height +
    ((GtkBase.Widget.allocation image_box#as_widget).Gtk.height - h)/2;
  main_window#event#add [`BUTTON_PRESS];
  ignore(main_window#event#connect#button_press (
    fun t -> if (!can_paint) then
	begin
	  let x = truncate (GdkEvent.Button.x t)
	  and y = truncate (GdkEvent.Button.y t) in
	  if x >= !image_x && y >= !image_y && 
	    x < !image_w && y < !image_h then
	    on_fill src (x - !image_x) (y - !image_y);
	  false
	end
      else 
	false
    ));
  ignore(btn_finalize#connect#clicked
    ~callback:(fun _ -> save_tmp (find_no_mark (Sdlloader.load_image "mark.bmp"));
      image_box#set_file "temp.bmp"; temp := Sdlloader.load_image "temp.bmp" ));
  if (not(!firstime)) then
    begin
      ignore(btn_relief#connect#clicked ~callback:(fun _ -> on_3D ()));
      ignore(btn_quadtree#connect#clicked ~callback:(fun _ -> on_quadtree ()));
      ignore(btn_reset#connect#clicked
	       ~callback:(fun _ -> on_reset !src));
      ignore(btn_save#connect#clicked
	       ~callback:(fun _ -> save "resultat.bmp"));
      ignore(btn_3d#connect#clicked
	       ~callback:(fun _ -> on_relief !temp));
      ignore(btn_gaussian#connect#clicked
	       ~callback:(fun _ -> on_gaussian !temp));
      ignore(btn_canny#connect#clicked
	       ~callback:(fun _ -> on_canny !temp));
      ignore(btn_noise#connect#clicked
	       ~callback:(fun _ -> on_noise !temp));
      firstime := true
    end;
  on_reset !src
      
(* Main *)

let man_help filename = 
  let file = open_in filename in 
  try
    while true do
      Printf.printf "%s\n" (input_line file) 
    done
  with End_of_file -> close_in file; ()
    
let _ =
  if Array.length Sys.argv >= 2 then 
    match Sys.argv.(1) with
      |"-h" | "--h" | "help" |"-help"|"--help"|"man" ->
	man_help "README"
      |_ -> print_endline "Usage: automap 
Try `./automap --help' for more information."
  else
    begin
      image_box#set_file "images/logo.png";
      image_box#set_ypad 180;
      ignore(main_window#connect#destroy
	       ~callback:GMain.quit);
      ignore(btn_quit#connect#clicked ~callback:GMain.quit);
      ignore(btn_open#connect#clicked (fun () -> ignore(open_dialog#run ())));
      ignore(btn_about#connect#clicked (fun () ->
	ignore(dialog#run ()); dialog#misc#hide ()));
      ignore(open_dialog#ok_button#connect#clicked (fun _ ->
	sdl_launch ();
	open_dialog#misc#hide ()));
      ignore(open_dialog#cancel_button#connect#clicked (fun _ ->
	open_dialog#misc#hide ()));
      main_window#show ();
      GMain.main ()
    end
