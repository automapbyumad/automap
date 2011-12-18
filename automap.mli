val step : int ref
val list_color : Sdlvideo.color list ref
val list_height : int array ref
val image_x : int ref
val image_y : int ref
val image_w : int ref
val image_h : int ref
val firstime : bool ref
val temp : Sdlvideo.surface ref
val src : Sdlvideo.surface ref
val refdiv_value : int ref
val can_paint : bool ref
val main_window : GWindow.window
val vbox1 : GPack.box
val bbox1 : GPack.button_box
val toolbar : GButton.toolbar
val btn_open : GButton.tool_button
val open_dialog : GWindow.file_selection
val btn_save : GButton.tool_button
val btn_reset : GButton.tool_button
val btn_noise : GButton.tool_button
val btn_border : GButton.tool_button
val btn_canny : GButton.tool_button
val btn_gaussian : GButton.tool_button
val btn_3d : GButton.tool_button
val btn_quadtree : GButton.tool_button
val btn_relief : GButton.tool_button
val btn_about : GButton.tool_button
val dialog : GWindow.about_dialog
val btn_quit : GButton.tool_button
val hbox0 : GPack.box
val bbox2 : GPack.button_box
val image_box : GMisc.image
val canny_adjust : GData.adjustment
val coef_adjust : GData.adjustment
val hbox_canny : GPack.box
val lbl_canny : GMisc.label
val canny_slider : GRange.scale
val auto_fill : GButton.toggle_button
val lbl_coef : GMisc.label
val coef_slider : GRange.scale
val hbox_canny2 : GPack.box
val btn_finalize : GButton.button
val is_color_in_range : int * int * int -> int * int * int -> int -> bool
val list_invert : 'a list -> 'a list
exception Too_many_colors
val print_border : Sdlvideo.surface -> int -> Sdlvideo.surface
val print_grid :
  Sdlvideo.surface -> Sdlvideo.surface -> int -> Sdlvideo.surface
val write_vertex : float -> float -> float -> out_channel -> unit
val write_face : int -> int -> int -> out_channel -> unit
val upperL : int -> int -> int -> int
val upperR : int -> int -> int -> int
val mid : int -> int -> int -> int
val bottomL : int -> int -> int -> int
val bottomR : int -> int -> int -> int
val indexof : 'a list -> 'a -> int
val get_height : Sdlvideo.color -> int
val trace_points : int -> int -> int -> string -> Sdlvideo.surface -> unit
val pi : float
val pow : int -> int -> int
val pow_f : float -> int -> float
val gaussian_pixel : float -> float -> float -> float
val generate_gaussian : int -> int -> float -> float array
val multi_color : int * int * int -> int -> int * int * int
val multi_color_f : int * int * int -> float -> float * float * float
val div_color : int * int * int -> int -> int * int * int
val div_color_f : float * float * float -> float -> int * int * int
val somme_color : int * int * int -> int * int * int -> int * int * int
val somme_color_f :
  float * float * float -> float * float * float -> float * float * float
val pair_int_of_float : float * float * float -> int * int * int
val save_as : Sdlvideo.surface -> string -> unit
val save_tmp : Sdlvideo.surface -> unit
val save : string -> unit
val get_string : 'a option -> 'a
val recup_int : < text : string; .. > -> unit
val recup_float : < text : string; .. > -> float
val recup : < text : string; .. > -> int
val generate_obj : int -> int -> int -> Sdlvideo.surface -> unit
val apply_gaussian_mask :
  Sdlvideo.surface -> int -> float -> Sdlvideo.surface
val sobelMaskX : int array array
val sobelMaskY : int array array
val grey : int * int * int -> int
val neighbour_white : Sdlvideo.surface -> int -> int -> int -> int -> bool
val apply_sobel_mask : Sdlvideo.surface -> float -> float -> Sdlvideo.surface
val find_no_mark : Sdlvideo.surface -> Sdlvideo.surface
val build_color_areas_table :
  Sdlvideo.surface -> int -> unit
val is_in_range : int * int * int -> int * int * int -> int -> bool
val max : (int * int * int, int) Hashtbl.t -> int * int * int
val unnoise : Sdlvideo.surface -> int -> Sdlvideo.surface
val reduct : Sdlvideo.surface -> int -> Sdlvideo.surface
val reform : Sdlvideo.surface -> int -> Sdlvideo.surface
val contains_near : int * int * int -> int -> (int * int * int) list -> bool
val build_max_color :
  int -> (int * int * int, int) Hashtbl.t -> int -> (int * int * int) list
val n_max : Sdlvideo.surface -> 'a -> int -> int -> (int * int * int) list
val nearest : int * int * int -> (int * int * int) list -> int * int * int
val replace_near :
  Sdlvideo.surface -> 'a -> (int * int * int) list -> Sdlvideo.surface
val fill :
  Sdlvideo.surface ->
  Sdlvideo.surface -> int -> int -> Sdlvideo.color -> unit
val triple2string : int * int * int -> string
val settings_noise : Sdlvideo.surface -> GtkSignal.id
val settings_gaussian : Sdlvideo.surface -> GtkSignal.id
val settings_canny : Sdlvideo.surface -> GtkSignal.id
val generate_cfg : unit -> unit
val settings_quadtree : unit -> GtkSignal.id
val settings_3d : unit -> GtkSignal.id
val settings_sampling : Sdlvideo.surface -> GtkSignal.id
val on_reset : Sdlvideo.surface -> unit
val on_fill : Sdlvideo.surface ref -> int -> int -> unit
val on_border : Sdlvideo.surface -> unit
val on_relief : Sdlvideo.surface -> unit
val on_noise : Sdlvideo.surface -> unit
val on_gaussian : Sdlvideo.surface -> unit
val on_canny : Sdlvideo.surface -> unit
val on_quadtree : unit -> unit
val on_3D : unit -> unit
val sdl_launch : unit -> unit
val man_help : string -> unit
