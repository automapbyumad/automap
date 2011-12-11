val fun_glut :
  display:('a -> unit) ->
  ?reshape:(width:int -> height:int -> unit) ->
  ?keyboard:('a -> key:char -> x:int -> y:int -> 'a) ->
  ?keyboard_up:('a -> key:char -> x:int -> y:int -> 'a) ->
  ?special:('a -> key:Glut.special_key -> x:int -> y:int -> 'a) ->
  ?special_up:('a -> key:Glut.special_key -> x:int -> y:int -> 'a) ->
  ?mouse:('a ->
          button:Glut.mouse_button ->
          state:Glut.mouse_button_state -> x:int -> y:int -> 'a) ->
  ?motion:('a -> x:int -> y:int -> 'a) ->
  ?passive:('a -> x:int -> y:int -> 'a) ->
  ?visibility:('a -> state:Glut.visibility_state -> 'a) ->
  ?entry:('a -> state:Glut.entry_state -> 'a) ->
  ?timer:(('a -> 'a) * int) list ->
  ?idle:('a -> 'a) ->
  ?full_screen:bool ->
  ?window_size:int * int ->
  ?title:string ->
  ?display_mode:Glut.init_mode list ->
  ?init_gl:(unit -> unit) -> init:'a -> unit -> unit
val post_redisplay : ('a -> 'b) -> 'a -> 'b
