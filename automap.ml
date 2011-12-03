(*------------------------------ 80 characters -------------------------------*)

let _ = GMain.init ()

(*------------------------------- 3D ENGINE ----------------------------------*)

(*------------------------------ GLOBALS VARS --------------------------------*)
let step = ref 20
let list_color = ref []
let list_height = ref (Array.make 0 0)

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

let btn_reset = GButton.button
  ~label:"Reset"
  ~packing:bbox1#add ()
let reset_image = GMisc.image
  ~stock:`REFRESH
  ~packing:btn_reset#set_image ()

(* Filter buttons *)
let btn_gaussian = GButton.button
  ~label:"Gaussian Blur"
  ~packing:bbox2#add ()

let btn_canny = GButton.button
  ~label:"Canny Border"
  ~packing:bbox2#add ()

(* Image *)
let image_box = GMisc.image
  ~packing:hbox0#add ()		  

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

let get_height color = !list_height.(indexof !list_color color)

let trace_points step w h output_file img =
  let file = open_out output_file in 
    for y=0 to (h/step)*2 do
      if (y mod 2 == 0) then
	for x=0 to (w/step) do
	  let height =
	    get_height (Sdlvideo.get_pixel_color img (x*step) (y/2*step)) in
	    write_vertex 
	      (float (x*step))(*/.float w) *)
	      (float height)(* /. 300.) *)
	      (float (y/2*step))(*/.float h) *)
	      file
	done
      else
	for x=0 to (w/step)-1 do
	  let height =
	    get_height (Sdlvideo.get_pixel_color img (x*step) (y/2*step)) in
	    write_vertex 
	      ((float (x*step) +. (float step) /. 2.) )(*/. float w)*)
	      (float height) (*/. 300.)*)
	      ((float (y/2*step) +. (float step) /. 2.) )(*/. float h)*)
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

let save_as src path =
  Sdlvideo.save_BMP src path

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

let recup input =
  let text = input#text in
  try
    int_of_string text;
  with
    | _ -> 0

let generate_obj w h pas img =
  trace_points pas w h "test.obj" img

(* Height <> Colors textboxes *)
let triple2string (r,g,b) =
  "(" ^ string_of_int r ^ ", " ^
    string_of_int g ^ ", " ^
    string_of_int b ^ ")"

(* Settings Windows *)
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
  let sigma_value = sigma_coef#adjustment#value in
  let step_value = step#adjustment#value in
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ -> let dst = Sdlloader.load_image "temp.bmp" in
			save_as (apply_gaussian_mask src dst) "temp.bmp";
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
  let low_treshold = lowtreshold#adjustment#value in
  let high_treshold = hightreshold#adjustment#value in
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ -> let dst = Sdlloader.load_image "temp.bmp" in
			save_as (apply_sobel_mask src dst) "canny.bmp";
			image_box#set_file "canny.bmp";
			window1#destroy ())

let generate_cfg () =
  let fichier = open_out "cfg.txt" in
  for i = 0 to (List.length !list_color)-1 do
    output_string fichier ((triple2string(List.nth !list_color i)) ^ " "
			   ^ (string_of_int(!list_height.(i)))^"\n");
  done;
  close_out fichier


let settings_sampling src = 
  let nb_colors = List.length !list_color in
  let window1 = GWindow.window
    ~title:"Sampling settings"
    ~width:250
    ~height:(30 * nb_colors + 100)
    ~show:true
    ~resizable:false () in
  let vbox1 = GPack.vbox
    ~packing:window1#add () in
  let textbox_array = Array.make nb_colors (GEdit.entry ())  in
  for i=0 to (nb_colors - 1) do
    let tb = GEdit.entry
      ~packing:vbox1#add () in
    let (r, g, b) = List.nth !list_color i in
    let color = Printf.sprintf "#%02x%02x%02x" r g b in
    tb#misc#modify_base [`NORMAL, `NAME color];
    textbox_array.(i) <- tb
  done;
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
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ ->
      list_height := Array.make (List.length !list_color) 0;
      for i=0 to ((List.length !list_color) - 1) do
	!list_height.(i) <- (recup textbox_array.(i));
	Printf.printf "%d\n" !list_height.(i)
      done;
      window1#destroy ();
      generate_cfg ();
      let (w, h, _) = Sdlvideo.surface_dims src in
      generate_obj w h !step src;
      E3D.main "test" "carte.bmp" )

let settings_grid src =
  let window1 = GWindow.window
    ~title:"Grid settings"
    ~width:320
    ~height:120
    ~show:true
    ~resizable:false () in
  let vbox1 = GPack.vbox
    ~packing:window1#add () in
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
  ignore(btn_cancel#connect#clicked ~callback:window1#destroy);
  btn_ok#connect#clicked
    ~callback:(fun _ -> recup_int step_box;
	       let dst = Sdlloader.load_image "temp.bmp" in
	       save_as (print_grid src dst (!step)) "temp_grid.bmp";
	       image_box#set_file "temp_grid.bmp";
	       window1#destroy ())

(* Button functions *)
let on_reset src =
  save_as src "temp.bmp";
  image_box#set_file "temp.bmp";
  ()

let on_border src =
  let dst = Sdlloader.load_image "temp.bmp" in
    save_as (print_border src dst) "temp.bmp";
    image_box#set_file "temp.bmp";
    ()
      
let on_grid src =
  let _ = settings_grid src in ()

let on_relief src =
  let _ = settings_sampling src in ()

let on_gaussian src =
  let _ = settings_gaussian src in ()

let on_canny src =
  let _ = settings_canny src in ()

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
    ignore(btn_gaussian#connect#clicked
	     ~callback:(fun _ -> on_gaussian src));
    ignore(btn_canny#connect#clicked
	     ~callback:(fun _ -> on_canny src));
    ()
      
(* Main *)
let _ =
  ignore(main_window#connect#destroy
	   ~callback:GMain.quit);
  ignore(btn_open#connect#selection_changed (fun _ -> sdl_launch ()));
  main_window#show ();
  GMain.main ()
