(** Zadanie: Origami, Autor: Miłosz Piekutowski, Code Review: Jakub Panasiuk **)

(** Punkty są reprezentowane zamiennie z wektorami *)
type point = float * float 

type kartka = point -> int

(** Porównanie z epsilonem *)
let eps = 1e-9
let (<=) a b = (a -. eps) <= b
let (==) a b = (a -. eps <= b) && (a +. eps >= b)
	
(** Suma wektorów *)
let suma (x, y) (t, u) = (x +. t, y +. u)

(** Różnica wektorów *)
let roznica (x, y) (t, u) = (x -. t, y -. u)

(** Mnożenie wektora przez skalar *)
let mnozenie c (x, y) = (c *. x, c *. y)

(** Iloczyn skalarny *)	
let il_skal (x, y) (t, u) = x *. t +. y *. u

(** Długość wektora *)
let len (x, y) =  sqrt (il_skal (x, y) (x, y)) 
	
(** Iloczyn wektorowy *)
let il_wekt (a, b) (c, d) =  a *. d -. b *. c

(** Odbicie wektora q względem wektora p *)
let reflect p q = roznica (mnozenie (2. *. (il_skal p q) /. (il_skal p p)) p) q

let prostokat (a, b) (c, d) (x, y) = 
	if a <= x && x <= c && b <= y && y <= d then 1 
	else 0

let kolko m r p = 
	if len (roznica p m) <= r then 1 
	else 0 
	
(** Funkcja składa kartkę k wzdłuż prostej przechodzącej przez punkty p1 i p2 *)
let zloz p1 p2 k q =
	match il_wekt (roznica p2 p1) (roznica q p1) with
    | d when d == 0. -> k q
    | d when d <= 0. -> 0
    | _ -> let q' = suma p1 (reflect (roznica p2 p1) (roznica q p1)) in k q + k q'

(** Wynikiem funkcji jest złożenie kartki k kolejno wzdłuż wszystkich prostych z listy l *)
let skladaj l k = List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l


