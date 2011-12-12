type 'a vector = { mutable size : int; max : int; tab : 'a array; }
val make : int -> 'a -> 'a vector
val is_full : 'a vector -> bool
val is_empty : 'a vector -> bool
val add_last : 'a -> 'a vector -> unit
val add_first : 'a -> 'a vector -> unit
val get : int -> 'a vector -> 'a
val gettable : 'a vector -> 'a array
val size : 'a vector -> int
val max : 'a vector -> int
val iter : ('a -> unit) -> 'a vector -> unit
val search : ('a -> bool) -> 'a vector -> int
val search2 : ('a -> 'b -> bool) -> 'a -> 'b vector -> int
