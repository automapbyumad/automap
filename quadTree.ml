(*#use "vector.ml";;*)

type point3 = { x:int; y:int; z:int }
let int = int_of_float

(* calcule Somme 0->n de (5*4^n)
   en version assez optimale
   (se souvient de la précédente puissance de 4) *)
let rec sumFiveTimesFourPowN acc = function 
  |(-1) -> 4
  | y -> acc + sumFiveTimesFourPowN (acc*4) (y-1)

(* Détermine si une couleur donnée est dans
   le champ d'action d'une autre *)
let is_in_range c1 c2 range = 
  let (r1, g1, b1) = c1 and (r2, g2, b2) = c2 in
    (r1 >= r2 - range && r1 <= r2 + range)
&& (g1 >= g2 - range && g1 <= g2 + range)
&& (b1 >= b2 - range && b1 <= b2 + range)

(* Génération du quadrillage optimisé *)
let quadTree outName picName dims range remDouble = 
  let startTime = Unix.gettimeofday () in
  let numFaces = ref 0 in
  let pic = Sdlloader.load_image picName in
  let (w, h, _) = Sdlvideo.surface_dims pic in
  let wDiv2 = w/2 and hDiv2 = h/2 in
  let searchDouble p np = p.x = np.x && p.y = np.y && p.z = np.z in
  let addPoint x y vec =
    let (r,g,b) = Sdlvideo.get_pixel_color pic x y in
    let level =  0.3 *. (float r) +. 0.59 *. (float g) +. 0.11 *. (float b) in
    let np = {x=x; y=(int level); z=y} in
      if remDouble then
	(*Remove doubles : time critical*)
	let i = Vector.search2 searchDouble np vec in
	  if i >= 0 then
	    i
	  else
	    begin
	      Vector.add_last np vec;
	      Vector.size vec - 1
	    end
      else
	begin
	  Vector.add_last np vec;
	  Vector.size vec - 1
	end
  in
  let check x0 x1 y0 y1 =
    let v = Sdlvideo.get_pixel_color pic x0 y0 in
    let b = ref false in
      for x = x0 - 1 to x1 + 1 do
	for y = y0 - 1 to y1 + 1 do
	  if not (is_in_range v (Sdlvideo.get_pixel_color pic x y) range) then
	    b := true;
	done
      done;
      !b
  in
  let out = open_out outName in
  output_string out "# Generated with Automap by UMAD team\n# http://umad.fr.nf\n";
  let rec buildTree i0 i1 i2 i3 vec dims = 
    (*  i0 - i3-->
        |     |
        i1 - i2
        |
        v          *)
    let p0 = Vector.get i0 vec and
	p2 = Vector.get i2 vec in
      if dims > 0 && p0.x + 1 < p2.x && (check p0.x p2.x p2.z p0.z) then
	begin
	  let x12 = (p2.x - p0.x) / 2 and
	      z12 = (p2.z - p0.z) / 2 in
	  let i4 = addPoint p0.x          (z12 + p0.z)  vec and
	      i5 = addPoint (x12 + p0.x)  p0.z          vec and
	      i6 = addPoint p2.x          (z12 + p0.z)  vec and
	      i7 = addPoint (x12 + p0.x)  p2.z          vec and
	      i8 = addPoint (x12 + p0.x)  (z12 + p0.z)  vec in
	    if not (Vector.is_full vec) then
	      begin
		(* 0 - 7 - 3
		   |[1]|[2]|
		   4 - 8 - 6
		   |[3]|[4]|
		   1 - 5 - 2 *)
		buildTree i4 i8 i7 i3 vec (dims-1);
		buildTree i8 i6 i2 i7 vec (dims-1);
		buildTree i0 i5 i8 i4 vec (dims-1);
		buildTree i5 i1 i6 i8 vec (dims-1)
	      end
	    else
	      begin
		numFaces := !numFaces + 1;
		Printf.fprintf out "f %d %d %d %d\n" (i0+1) (i1+1) (i2+1) (i3+1)
	      end
	end
      else
	begin
	  numFaces := !numFaces + 1;
	  Printf.fprintf out "f %d %d %d %d\n" (i0+1) (i1+1) (i2+1) (i3+1)
	end
  in
  let vec = Vector.make 
    (sumFiveTimesFourPowN 5 (dims-1)) 
    {x=(-1); y=(-1); z=(-1)}
  in
    ignore(addPoint 1     1     vec);
    ignore(addPoint 1     (h-2) vec);
    ignore(addPoint (w-2) (h-2) vec);
    ignore(addPoint (w-2) 1     vec);
    buildTree 1 2 3 0 vec dims;
 
    Vector.iter
      (fun x -> 
	 if x.x >= 0 || x.y >= 0 || x.z >= 0 then
	   Printf.fprintf out "v %d %d %d\n" (x.x - wDiv2) x.y (x.z - hDiv2)
      )
      vec;
    Printf.printf "max size : %d\n" (Vector.max vec);
    Printf.printf "real size : %d\n" (Vector.size vec);
    Printf.fprintf out "# %d %d\n" (Vector.size vec) (!numFaces);
    Printf.printf "time spent : %f sec.\n" (Unix.gettimeofday() -. startTime);
    close_out out

let _ =
  if Array.length Sys.argv >= 4 then
    quadTree Sys.argv.(1) Sys.argv.(2) (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(4)) false
  else
    quadTree "test.obj" "/home/levak/CarteGenerator/carte2.jpg" 8 0 false
