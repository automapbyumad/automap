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
let quadTree outName picName dims range invert =
  let startTime = Unix.gettimeofday () and
      numFaces = ref 0 and
      max_points = (sumFiveTimesFourPowN 5 (dims-1)) in
  let vec = Vector.make max_points {x=(-1); y=(-1); z=(-1)} and
      htbl = Hashtbl.create max_points and
      out = open_out outName and
      pic = Sdlloader.load_image picName and
      outPic = Sdlloader.load_image picName in
  let (w, h, _) = Sdlvideo.surface_dims pic in
    if invert then
      for y = 0 to h - 1 do
	for x = 0 to w - 1 do
	  Sdlvideo.put_pixel_color outPic x y Sdlvideo.black
	done
      done;
  let wDiv2 = w/2 and hDiv2 = h/2 in
  let vline xs ys ye =
    for y = ys to ye do
      if not invert then
	Sdlvideo.put_pixel_color outPic xs y (Sdlvideo.black)
      else
	Sdlvideo.put_pixel_color outPic xs y (Sdlvideo.get_pixel_color pic xs y)
    done
  and hline xs ys xe =
    for x = xs to xe do
      if not invert then
	Sdlvideo.put_pixel_color outPic x ys (Sdlvideo.black)
      else
	Sdlvideo.put_pixel_color outPic x ys (Sdlvideo.get_pixel_color pic x ys)
    done
  in
  let addPoint x y =
    let (r,g,b) = Sdlvideo.get_pixel_color pic x y in
    let level =  0.3 *. (float r) +. 0.59 *. (float g) +. 0.11 *. (float b) in
    let np = {x=x; y=(int level); z=y} in
	try Hashtbl.find htbl (x, y)
	with Not_found ->
	  Vector.add_last np vec;
	  Hashtbl.add htbl (x, y) (Vector.size vec - 1);
	  Vector.size vec - 1
  and addFace i0 i1 i2 i3 = 
    let p0 = Vector.get i0 vec and
	p2 = Vector.get i2 vec in
      numFaces := !numFaces + 1;
      Printf.fprintf out "f %d %d %d %d\n" (i0+1) (i1+1) (i2+1) (i3+1);
      vline p0.x p2.z p0.z;
      vline p2.x p2.z p0.z;
      hline p0.x p2.z p2.x;
      hline p0.x p0.z p2.x
  and check x0 x1 y0 y1 =
    let b = ref false and
	xs = if x0 > 0 then x0 - 1 else 0 and
	xe = if x1 < w - 1 then x1 + 1 else w - 1 and
	ys = if y0 > 0 then y0 - 1 else 0 and
	ye = if y1 < h - 1 then y1 + 1 else h - 1 in
    let v = Sdlvideo.get_pixel_color pic xs ys in
      for x = xs to xe do
	for y = ys to ye do
	  if not (is_in_range v (Sdlvideo.get_pixel_color pic x y) range) then
	    b := true;
	done
      done;
      !b
  in
    output_string out "# Generated with Automap by UMAD team\n";
    output_string out "# http://umad.fr.nf\n";
    let rec buildTree i0 i1 i2 i3 dims =
      (*  i0 - i3-->
          |     |
          i1 - i2
          |
          v          *)
      let p0 = Vector.get i0 vec and
	  p2 = Vector.get i2 vec in
	if dims > 0
	  && p2.x - p0.x > 1
	  && p0.z - p2.z > 1
	  && (check p0.x p2.x p2.z p0.z) then
	    begin
	      let x12 = (p2.x - p0.x) / 2 and
		  z12 = (p2.z - p0.z) / 2 in
	      let i4 = addPoint p0.x          (z12 + p0.z) and
		  i5 = addPoint (x12 + p0.x)  p0.z         and
		  i6 = addPoint p2.x          (z12 + p0.z) and
		  i7 = addPoint (x12 + p0.x)  p2.z         and
		  i8 = addPoint (x12 + p0.x)  (z12 + p0.z) in
		if not (Vector.is_full vec) then
		  begin
		    (* 0 - 4 - 3
		       |[1]|[2]|
		       5 - 8 - 7
		       |[3]|[4]|
		       1 - 6 - 2 *)
		    buildTree i4 i8 i7 i3 (dims-1);
		    buildTree i8 i6 i2 i7 (dims-1);
		    buildTree i0 i5 i8 i4 (dims-1);
		    buildTree i5 i1 i6 i8 (dims-1)
		  end
		else
		  addFace i0 i1 i2 i3
	    end
	else
	  addFace i0 i1 i2 i3
    in
      ignore(addPoint 0     0    );
      ignore(addPoint 0     (h-1));
      ignore(addPoint (w-1) (h-1));
      ignore(addPoint (w-1) 0    );
      buildTree 1 2 3 0 dims;
      Vector.iter
	(fun x ->
	   if x.x >= 0 || x.y >= 0 || x.z >= 0 then
	     Printf.fprintf out "v %d %d %d\n" (x.x - wDiv2) x.y (x.z - hDiv2)
	)
	vec;
      Printf.printf "max size : %d\n%!" (Vector.max vec);
      Printf.printf "real size : %d\n%!" (Vector.size vec);
      Printf.fprintf out "# %d %d\n" (Vector.size vec) (!numFaces);
      Printf.printf "time spent : %f sec.\n%!" (Unix.gettimeofday() -. startTime);
      close_out out;
      outPic
