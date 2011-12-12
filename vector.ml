type 'a vector =
    {
      mutable size : int;
      max  : int;
      tab  : 'a array;
    }

let make n base =
  {
    size = 0;
    max = n;
    tab = Array.make n base
  }

(* renvoie vrai si le vecteur est plein *)
let is_full v =
  v.size = v.max

(* renvoie vrai si le vecteur est vide *)
let is_empty v =
  v.size = 0

(* ajoute un élément en fin de vecteur *)
let add_last e v =
  if not (is_full v) then
    begin
      v.tab.(v.size) <- e;
      v.size <- v.size + 1
    end

(* ajoute un élément en tête de vecteur *)
let add_first e v =
  if not (is_full v) then
    begin
      for i = v.size downto 0 do
	v.tab.(i+1) <- v.tab.(i)
      done;
      v.size <- v.size + 1;
      v.tab.(0) <- e
    end

(* renvoie l'élément à la position n *)
let get i v = v.tab.(i)
let gettable v = v.tab
let size v = v.size
let max v = v.max

let iter f v =
  Array.iter f (v.tab)

let search f v =
  let place = ref (-1) in
    for i = 0 to v.size do
      if f v.tab.(i) then
	place := i
    done;
    !place

let search2 f a v =
  let i = ref 0 in
    while !i < v.size && not (f a v.tab.(!i)) do
      i := !i + 1
    done;
    if !i < v.size then
      (!i)
    else
      (-1)
