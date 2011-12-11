(* {{{ COPYING *(

  +-----------------------------------------------------------------------+
  |  This file belongs to glMLite, an OCaml binding to the OpenGL API.    |
  +-----------------------------------------------------------------------+
  |  Copyright (C) 2006, 2007, 2008  Florent Monnier                      |
  |  Contact:  <fmonnier@linux-nantes.org>                                |
  +-----------------------------------------------------------------------+
  |  This program is free software: you can redistribute it and/or        |
  |  modify it under the terms of the GNU General Public License          |
  |  as published by the Free Software Foundation, either version 3       |
  |  of the License, or (at your option) any later version.               |
  |                                                                       |
  |  This program is distributed in the hope that it will be useful,      |
  |  but WITHOUT ANY WARRANTY; without even the implied warranty of       |
  |  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        |
  |  GNU General Public License for more details.                         |
  |                                                                       |
  |  You should have received a copy of the GNU General Public License    |
  |  along with this program.  If not, see <http://www.gnu.org/licenses/> |
  +-----------------------------------------------------------------------+

)* }}} *)


external glutInit: argv:string array -> string array = "ml_glutinit"

external glutInitWindowPosition: x:int -> y:int -> unit = "ml_glutinitwindowposition"
external glutPositionWindow: x:int -> y:int -> unit = "ml_glutpositionwindow"
external glutInitWindowSize: width:int -> height:int -> unit = "ml_glutinitwindowsize"
external glutReshapeWindow: width:int -> height:int -> unit = "ml_glutreshapewindow"

type window_id
external glutCreateWindow: title:string -> window_id = "ml_glutcreatewindow"
external glutSetWindow: win:window_id -> unit = "ml_glutsetwindow"
external glutGetWindow: unit -> window_id = "ml_glutgetwindow"
external glutCreateSubWindow: win:window_id -> x:int -> y:int -> width:int -> height:int -> window_id = "ml_glutcreatesubwindow"
external glutDestroyWindow: win:window_id -> unit = "ml_glutdestroywindow"

external glutSwapBuffers: unit -> unit = "ml_glutswapbuffers"
external glutPostRedisplay: unit -> unit = "ml_glutpostredisplay"
external glutFullScreen: unit -> unit = "ml_glutfullscreen"
external glutMainLoop: unit -> unit = "ml_glutmainloop"
external glutLeaveMainLoop: unit -> unit = "ml_glutleavemainloop"

external glutSetWindowTitle: name:string -> unit = "ml_glutsetwindowtitle"
external glutSetIconTitle: name:string -> unit = "ml_glutseticontitle"
external glutPopWindow: unit -> unit = "ml_glutpopwindow"
external glutPushWindow: unit -> unit = "ml_glutpushwindow"
external glutShowWindow: unit -> unit = "ml_glutshowwindow"
external glutHideWindow: unit -> unit = "ml_gluthidewindow"
external glutIconifyWindow: unit -> unit = "ml_gluticonifywindow"


# 1 "enums/cursor_type.inc.ml" 1
type cursor_type =
  | GLUT_CURSOR_RIGHT_ARROW
  | GLUT_CURSOR_LEFT_ARROW
  | GLUT_CURSOR_INFO
  | GLUT_CURSOR_DESTROY
  | GLUT_CURSOR_HELP
  | GLUT_CURSOR_CYCLE
  | GLUT_CURSOR_SPRAY
  | GLUT_CURSOR_WAIT
  | GLUT_CURSOR_TEXT
  | GLUT_CURSOR_CROSSHAIR
  | GLUT_CURSOR_UP_DOWN
  | GLUT_CURSOR_LEFT_RIGHT
  | GLUT_CURSOR_TOP_SIDE
  | GLUT_CURSOR_BOTTOM_SIDE
  | GLUT_CURSOR_LEFT_SIDE
  | GLUT_CURSOR_RIGHT_SIDE
  | GLUT_CURSOR_TOP_LEFT_CORNER
  | GLUT_CURSOR_TOP_RIGHT_CORNER
  | GLUT_CURSOR_BOTTOM_RIGHT_CORNER
  | GLUT_CURSOR_BOTTOM_LEFT_CORNER
  | GLUT_CURSOR_FULL_CROSSHAIR
  | GLUT_CURSOR_NONE
  | GLUT_CURSOR_INHERIT
# 56 "Glut.ml.pp" 2
external glutSetCursor: cursor:cursor_type -> unit = "ml_glutsetcursor"


# 1 "enums/init_mode.inc.ml" 1
type init_mode =
  | GLUT_RGBA
  | GLUT_RGB
  | GLUT_INDEX
  | GLUT_SINGLE
  | GLUT_DOUBLE
  | GLUT_ACCUM
  | GLUT_ALPHA
  | GLUT_DEPTH
  | GLUT_STENCIL
  | GLUT_MULTISAMPLE
  | GLUT_STEREO
  | GLUT_LUMINANCE
# 60 "Glut.ml.pp" 2
external glutInitDisplayMode: mode:init_mode list -> unit = "ml_glutinitdisplaymode"



external glutDisplayFunc: unit -> unit = "ml_glutdisplayfunc"
let glutDisplayFunc ~display =
  Callback.register "GL callback display" display;
  glutDisplayFunc();
;;

external glutReshapeFunc: unit -> unit = "ml_glutreshapefunc"
let glutReshapeFunc ~reshape =
  Callback.register "GL callback reshape" reshape;
  glutReshapeFunc();
;;

external glutKeyboardFunc: unit -> unit = "ml_glutkeyboardfunc"
let glutKeyboardFunc ~keyboard =
  Callback.register "GL callback keyboard" keyboard;
  glutKeyboardFunc();
;;

external glutKeyboardUpFunc: unit -> unit = "ml_glutkeyboardupfunc"
let glutKeyboardUpFunc ~keyboard_up =
  Callback.register "GL callback keyboard-up" keyboard_up;
  glutKeyboardUpFunc();
;;

external glutPassiveMotionFunc: unit -> unit = "ml_glutpassivemotionfunc"
let glutPassiveMotionFunc ~passive =
  Callback.register "GL callback passive" passive;
  glutPassiveMotionFunc();
;;

external glutMotionFunc: unit -> unit = "ml_glutmotionfunc"
let glutMotionFunc ~motion =
  Callback.register "GL callback motion" motion;
  glutMotionFunc();
;;

# 1 "enums/mouse_button_state.inc.ml" 1
type mouse_button_state =
  | GLUT_DOWN
  | GLUT_UP
# 101 "Glut.ml.pp" 2

external glutMouseFunc: unit -> unit = "ml_glutmousefunc"
let glutMouseFunc ~mouse =
  Callback.register "GL callback mouse" mouse;
  glutMouseFunc();
;;

# 1 "enums/visibility_state.inc.ml" 1
type visibility_state =
  | GLUT_NOT_VISIBLE
  | GLUT_VISIBLE
# 109 "Glut.ml.pp" 2

external glutVisibilityFunc: unit -> unit = "ml_glutvisibilityfunc"
let glutVisibilityFunc ~visibility =
  Callback.register "GL callback visibility" visibility;
  glutVisibilityFunc();
;;

# 1 "enums/entry_state.inc.ml" 1
type entry_state =
  | GLUT_LEFT
  | GLUT_ENTERED
# 117 "Glut.ml.pp" 2

external glutEntryFunc: unit -> unit = "ml_glutentryfunc"
let glutEntryFunc ~entry =
  Callback.register "GL callback entry" entry;
  glutEntryFunc();
;;

# 1 "enums/special_key.inc.ml" 1
type special_key =
  | GLUT_KEY_F1
  | GLUT_KEY_F2
  | GLUT_KEY_F3
  | GLUT_KEY_F4
  | GLUT_KEY_F5
  | GLUT_KEY_F6
  | GLUT_KEY_F7
  | GLUT_KEY_F8
  | GLUT_KEY_F9
  | GLUT_KEY_F10
  | GLUT_KEY_F11
  | GLUT_KEY_F12
  | GLUT_KEY_LEFT
  | GLUT_KEY_UP
  | GLUT_KEY_RIGHT
  | GLUT_KEY_DOWN
  | GLUT_KEY_PAGE_UP
  | GLUT_KEY_PAGE_DOWN
  | GLUT_KEY_HOME
  | GLUT_KEY_END
  | GLUT_KEY_INSERT
# 125 "Glut.ml.pp" 2

external glutSpecialFunc: unit -> unit = "ml_glutspecialfunc"
let glutSpecialFunc ~special =
  Callback.register "GL callback special" special;
  glutSpecialFunc();
;;

external glutSpecialUpFunc: unit -> unit = "ml_glutspecialupfunc"
let glutSpecialUpFunc ~special_up =
  Callback.register "GL callback special-up" special_up;
  glutSpecialUpFunc();
;;

external glutIdleFunc: unit -> unit = "ml_glutidlefunc"
let glutIdleFunc ~idle =
  Callback.register "GL callback idle" idle;
  glutIdleFunc();
;;

external glutRemoveIdleFunc: unit -> unit = "ml_glutremoveidlefunc"


type menu_id = int

external glutCreateMenu: unit -> menu_id = "ml_glutcreatemenu"
let glutCreateMenu ~menu =
  Callback.register "GL callback menu" menu;
  glutCreateMenu();
;;



external init_glutTimerFunc : (int -> unit) -> unit = "init_gluttimerfunc_cb"

let timer_hashtbl = Hashtbl.create 32 ;;

let real_call_back i = (Hashtbl.find timer_hashtbl i) () ;;
      
let () = init_glutTimerFunc real_call_back ;;
    

external _glutTimerFunc : int -> int -> unit = "ml_gluttimerfunc"

let timer_count = ref 0 ;;

let glutTimerFunc ~msecs ~timer:(cb:(value:'a -> unit)) ~value =
  let i = !timer_count in
  incr timer_count;
  Hashtbl.add timer_hashtbl i (fun () ->
      Hashtbl.remove timer_hashtbl i;
      cb value);
  _glutTimerFunc msecs i
;;



external glutAddSubMenu: name:string -> menu:menu_id -> unit = "ml_glutaddsubmenu"
external glutAddMenuEntry: name:string -> value:int -> unit = "ml_glutaddmenuentry"
external glutGetMenu: unit -> menu_id = "ml_glutgetmenu"
external glutSetMenu: menu:menu_id -> unit = "ml_glutsetmenu"
external glutDestroyMenu: menu:menu_id -> unit = "ml_glutdestroymenu"

# 1 "enums/mouse_button.inc.ml" 1
type mouse_button =
  | GLUT_LEFT_BUTTON
  | GLUT_MIDDLE_BUTTON
  | GLUT_RIGHT_BUTTON
  | GLUT_WHEEL_UP
  | GLUT_WHEEL_DOWN
# 188 "Glut.ml.pp" 2
external glutAttachMenu: button:mouse_button -> unit = "ml_glutattachmenu"
external glutDetachMenu: button:mouse_button -> unit = "ml_glutdetachmenu"

external glutChangeToMenuEntry: entry:int -> name:string -> value:int -> unit = "ml_glutchangetomenuentry"
external glutChangeToSubMenu: entry:int -> name:string -> menu:menu_id -> unit = "ml_glutchangetosubmenu"
external glutRemoveMenuItem: entry:int -> unit = "ml_glutremovemenuitem"


external glutInitDisplayString: string -> unit = "ml_glutinitdisplaystring"


(* State Retrieval *)

# 1 "enums/get_state.inc.ml" 1
type get_state =
  | GLUT_WINDOW_X
  | GLUT_WINDOW_Y
  | GLUT_WINDOW_WIDTH
  | GLUT_WINDOW_HEIGHT
  | GLUT_WINDOW_BUFFER_SIZE
  | GLUT_WINDOW_STENCIL_SIZE
  | GLUT_WINDOW_DEPTH_SIZE
  | GLUT_WINDOW_RED_SIZE
  | GLUT_WINDOW_GREEN_SIZE
  | GLUT_WINDOW_BLUE_SIZE
  | GLUT_WINDOW_ALPHA_SIZE
  | GLUT_WINDOW_ACCUM_RED_SIZE
  | GLUT_WINDOW_ACCUM_GREEN_SIZE
  | GLUT_WINDOW_ACCUM_BLUE_SIZE
  | GLUT_WINDOW_ACCUM_ALPHA_SIZE
  | GLUT_WINDOW_DOUBLEBUFFER
  | GLUT_WINDOW_RGBA
  | GLUT_WINDOW_PARENT
  | GLUT_WINDOW_NUM_CHILDREN
  | GLUT_WINDOW_COLORMAP_SIZE
  | GLUT_WINDOW_NUM_SAMPLES
  | GLUT_WINDOW_STEREO
  | GLUT_WINDOW_CURSOR
  | GLUT_SCREEN_WIDTH
  | GLUT_SCREEN_HEIGHT
  | GLUT_SCREEN_WIDTH_MM
  | GLUT_SCREEN_HEIGHT_MM
  | GLUT_MENU_NUM_ITEMS
  | GLUT_DISPLAY_MODE_POSSIBLE
  | GLUT_INIT_DISPLAY_MODE
  | GLUT_INIT_WINDOW_X
  | GLUT_INIT_WINDOW_Y
  | GLUT_INIT_WINDOW_WIDTH
  | GLUT_INIT_WINDOW_HEIGHT
  | GLUT_ELAPSED_TIME
# 202 "Glut.ml.pp" 2
external glutGet: state:get_state -> int = "ml_glutget"

# 1 "enums/glut_device.inc.ml" 1
type glut_device =
  | GLUT_HAS_KEYBOARD
  | GLUT_HAS_MOUSE
  | GLUT_HAS_SPACEBALL
  | GLUT_HAS_DIAL_AND_BUTTON_BOX
  | GLUT_HAS_TABLET
  | GLUT_NUM_MOUSE_BUTTONS
  | GLUT_NUM_SPACEBALL_BUTTONS
  | GLUT_NUM_BUTTON_BOX_BUTTONS
  | GLUT_NUM_DIALS
  | GLUT_NUM_TABLET_BUTTONS
# 205 "Glut.ml.pp" 2
external glutDeviceGet: device:glut_device -> int = "ml_glutdeviceget"

type active_modifier =
  | GLUT_ACTIVE_SHIFT
  | GLUT_ACTIVE_CTRL
  | GLUT_ACTIVE_ALT

external glutGetModifiers: unit -> active_modifier list = "ml_glutgetmodifiers"
external glutGetModifiersB: unit -> bool * bool * bool = "ml_glutgetmodifiers_t"

external glutExtensionSupported: extension:string -> bool = "ml_glutextensionsupported"


(* Game Mode *)

external glutGameModeString: mode:string -> unit = "ml_glutgamemodestring"

external glutEnterGameMode: unit -> unit = "ml_glutentergamemode"
external glutLeaveGameMode: unit -> unit = "ml_glutleavegamemode"

# 1 "enums/game_mode.inc.ml" 1
type game_mode =
  | GLUT_GAME_MODE_ACTIVE
  | GLUT_GAME_MODE_POSSIBLE
  | GLUT_GAME_MODE_WIDTH
  | GLUT_GAME_MODE_HEIGHT
  | GLUT_GAME_MODE_PIXEL_DEPTH
  | GLUT_GAME_MODE_REFRESH_RATE
  | GLUT_GAME_MODE_DISPLAY_CHANGED
# 226 "Glut.ml.pp" 2
external glutGameModeGet: game_mode:game_mode -> int = "ml_glutgamemodeget"


(* Font Rendering *)

type stroke_font =
  | GLUT_STROKE_ROMAN
  | GLUT_STROKE_MONO_ROMAN

type bitmap_font =
  | GLUT_BITMAP_9_BY_15
  | GLUT_BITMAP_8_BY_13
  | GLUT_BITMAP_TIMES_ROMAN_10
  | GLUT_BITMAP_TIMES_ROMAN_24
  | GLUT_BITMAP_HELVETICA_10
  | GLUT_BITMAP_HELVETICA_12
  | GLUT_BITMAP_HELVETICA_18

external glutBitmapCharacter: font:bitmap_font -> c:char -> unit = "ml_glutbitmapcharacter"
external glutBitmapWidth: font:bitmap_font -> c:char -> int = "ml_glutbitmapwidth"

external glutStrokeCharacter: font:stroke_font -> c:char -> unit = "ml_glutstrokecharacter"
external glutStrokeWidth: font:stroke_font -> c:char -> int = "ml_glutstrokewidth"

external glutBitmapHeight: font:bitmap_font -> int = "ml_glutbitmapheight"
external glutStrokeHeight: font:stroke_font -> float = "ml_glutstrokeheight"

external glutBitmapLength: font:bitmap_font -> str:string -> int = "ml_glutbitmaplength"
external glutStrokeLength: font:stroke_font -> str:string -> int = "ml_glutstrokelength"


(* Geometric Object Rendering *)

external glutWireSphere: radius:float -> slices:int -> stacks:int -> unit = "ml_glutwiresphere"
external glutSolidSphere: radius:float -> slices:int -> stacks:int -> unit = "ml_glutsolidsphere"
external glutWireCone: base:float -> height:float -> slices:int -> stacks:int -> unit = "ml_glutwirecone"
external glutSolidCone: base:float -> height:float -> slices:int -> stacks:int -> unit = "ml_glutsolidcone"
external glutWireCube: size:float -> unit = "ml_glutwirecube"
external glutSolidCube: size:float -> unit = "ml_glutsolidcube"
external glutWireTorus: innerRadius:float -> outerRadius:float -> sides:int -> rings:int -> unit = "ml_glutwiretorus"
external glutSolidTorus: innerRadius:float -> outerRadius:float -> sides:int -> rings:int -> unit = "ml_glutsolidtorus"
external glutWireDodecahedron: unit -> unit = "ml_glutwiredodecahedron"
external glutSolidDodecahedron: unit -> unit = "ml_glutsoliddodecahedron"
external glutWireTeapot: size:float -> unit = "ml_glutwireteapot"
external glutSolidTeapot: size:float -> unit = "ml_glutsolidteapot"
external glutWireOctahedron: unit -> unit = "ml_glutwireoctahedron"
external glutSolidOctahedron: unit -> unit = "ml_glutsolidoctahedron"
external glutWireTetrahedron: unit -> unit = "ml_glutwiretetrahedron"
external glutSolidTetrahedron: unit -> unit = "ml_glutsolidtetrahedron"
external glutWireIcosahedron: unit -> unit = "ml_glutwireicosahedron"
external glutSolidIcosahedron: unit -> unit = "ml_glutsolidicosahedron"

external glutWireRhombicDodecahedron: unit -> unit = "ml_glutwirerhombicdodecahedron"
external glutSolidRhombicDodecahedron: unit -> unit = "ml_glutsolidrhombicdodecahedron"


(* Color Index *)

external glutSetColor: cell:int -> r:float -> g:float -> b:float -> unit = "ml_glutsetcolor"
(** to use with [glIndex] *)

external glutGetColor: cell:int -> float * float * float = "ml_glutgetcolor"


(* vim: et fdm=marker filetype=ocaml
 *)
