#load "graphics.cma";;
open Graphics;;
#use "examples.ml";;
exception Quit;;

let last_x=ref 0;;
let last_y=ref 0;;

let rec minimum tab = match tab with
    [] -> failwith "Vide"
	| [x] -> x
	| x::y -> let res=(minimum y) in if x<res then x else res;;

let rec indexOf tab x=match tab with
  [] -> failwith "Non trouvé"
  | h::t -> if h=x then 0 else 1+indexOf t x

let euclide p1 p2=
let res=float_of_int ((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y)) in
sqrt res;;

let taxicab p1 p2=
 let a=p1.x-p2.x in
 let b=p1.y-p2.y in
 let c=float_of_int a in
 let d=float_of_int b in
 abs_float c+.abs_float d;;

let indice distance p v=
  let l=Array.length v.seeds in
  let a=Array.make l 0 in
  for i=0 to l-1 do
  	let r=v.seeds.(i) in
  	let t=distance p r in
    a.(i)<-int_of_float t;
  done;
  let tab=Array.to_list a in
  let tab2=Array.to_list v.seeds in
  let m=minimum tab in
  let res=indexOf tab m in
  indexOf tab2 v.seeds.(res);;

let regions_voronoi distance v=
  let n=fst v.dim in
  let m=Array.make_matrix n n 0 in
  for i=0 to n-1 do
    for j=0 to n-1 do
      let p={c=None;x=i;y=j} in
      let res=indice distance p v in
      m.(i).(j)<-res;
    done
  done;
  m;;

let getColor c=match c with
  Some c-> c;;


let draw_voronoi m v=
let n=Array.length m.(0) in
let h=fst v.dim in
set_color black;
draw_rect 0 0 h h;
auto_synchronize false;
for i=0 to n-1 do
	for j=0 to n-1 do
		let r=m.(i).(j) in
		let col=v.seeds.(r).c in
		let p={c=col;x=i;y=j} in
    if p.c!=None then let col2=getColor p.c in set_color col2; else set_color white;
    if i>0 && j>0 && j+1<=n-1 && i+1<=n-1 then
    begin
    let test=m.(i).(j)!=m.(i-1).(j) || m.(i).(j)!=m.(i+1).(j) in
    let test2=m.(i).(j)!=m.(i).(j+1) || m.(i).(j)!=m.(i).(j-1) in
		if test || test2 then set_color black;
    end;
    plot i j;
	done
done;
synchronize();
;;

let palette t=
auto_synchronize false;
let c=[|red;blue;green;yellow|] in
let y=ref 0 in
for i=0 to 3 do
set_color c.(i);
fill_rect 800 !y 100 200;
y:=!y+200;
done;
set_color white;
fill_rect 950 450 100 100;
moveto 980 500;
set_color black;
draw_string "effacer";
draw_rect 950 450 100 100;
synchronize();
;;

let check m h k=
try
let n=Array.length m.(0) in
for i=0 to n-1 do
  for j=0 to n-1 do
  if m.(i).(j)=h then begin
  if i>0 && j>0 && j+1<=n-1 && i+1<=n-1 then begin
  let test= k=m.(i-1).(j) || k=m.(i+1).(j) in
  let test2= k=m.(i).(j+1) || k=m.(i).(j-1) in
  if test || test2 then raise Quit;
  end; 
  end;
  done
done;
false
with Quit -> true;;

let adjacences_voronoi m v=
  let n=Array.length v.seeds in
  let b=Array.make_matrix n n false in
  for h=0 to n-1 do
    for k=0 to n-1 do
      if h!=k then begin
       let test=check m h k in
      if test then b.(h).(k)<-true;
      end;
    done
  done;
b;;

open_graph " 1100x900";;
set_window_title "Projet";;

let replace_germe distance p v col2=
  let res=indice distance p v in
  let z=v.seeds.(res) in
  if col2=Some white && z.c!=None then 
  begin
  let germe={c=None;x=z.x;y=z.y} in
  v.seeds.(res)<-germe;
  end

  else 
  begin 
  let germe={c=p.c;x=z.x;y=z.y} in
  if z.c=None then v.seeds.(res)<-germe else 
  begin
    if z.c!=col2 then print_endline "Cette région est déjà coloriée d'une couleur différente.";
  end
end;;


let verification v=
try
  let n=Array.length v.seeds in
  for i=0 to n-1 do
    let wh=Some white in
    if v.seeds.(i).c=None || v.seeds.(i).c=wh then raise Quit;
  done;
  true
with Quit -> false;;


let difference b distance p v col=
  try
  let res=indice distance p v in
  let n=Array.length b in
  for i=0 to n-1 do
    if b.(res).(i) && v.seeds.(i).c=col then raise Quit;
  done;
  true
  with Quit -> false;;

Random.self_init();;
let tableau=[|v1;v2;v3;v4|];;
let lg=Array.length tableau;;
let res=Random.int lg;;
let v=tableau.(res);;
let m=regions_voronoi euclide v1;;
draw_voronoi m v1;;
palette();;
let regions=adjacences_voronoi m v1;;

while true do


(*Cas où l'on clique sur la palette de couleurs*)
let eve=wait_next_event [Button_down] in
if eve.button && eve.mouse_x<900 && eve.mouse_x>=800 then 
    begin
    last_x:=eve.mouse_x;
    last_y:=eve.mouse_y;
    print_endline "Couleur sélectionnée !";
    end
else 
    begin 

    (*Cas où l'on clique sur le bouton effacer*)
    if eve.button && eve.mouse_x<1050 && eve.mouse_x>=950 && eve.mouse_y>=450 && eve.mouse_y<550 then
    begin
    last_x:=eve.mouse_x;
    last_y:=eve.mouse_y;
    print_endline "Mode effaçage sélectionné !";
    end

  else begin


    (*Cas où on décide de colorier une région*)
    if eve.button && eve.mouse_x>0 && eve.mouse_x<800 then 
    begin
    let col=point_color !last_x !last_y in
    let col2=Some col in
    let p={c=col2;x=eve.mouse_x;y=eve.mouse_y} in
    let possible=difference regions euclide p v col2 in
    if possible then begin 
    replace_germe euclide p v1 col2;
    clear_graph();
    draw_voronoi m v1;
    palette();
    let test=verification v1 in
    if test then print_endline "Jeu réussi. Félicitations.";
    end
    end
    
    end
    end

;


done;;