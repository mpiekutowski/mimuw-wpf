(*	
	Zadanie: Drzewa lewicowe
	Autor: Miłosz Piekutowski
	Code review: Hubert Grochowski

*)

type 'a queue = (* Reprezentacja drzewa *)
	| Node of 'a queue * 'a queue * 'a * int
	| Null

let empty = Null
	
let is_empty q =
	match q with
	| Null -> true
	| _ -> false

let height q = (* Zwraca wysokość wierzchołka*)
	match q with
	| Null -> 0
	| Node (_,_,_,h) -> h
	
let rec join l r =
	match l, r with
	| _, Null -> l 											(* Przy łączeniu pustego z innym podrzewem zwraca te podrzewo *)
	| Null, _ -> r
	| Node (l_left, l_right, l_value, l_height), Node(r_left, r_right, r_value, r_height) ->
		if r_value < l_value then
			join r l 											(* Jeżeli wartość na górze drugiej kolejki jest mniejsza, wywołuję funkcję jeszcze raz z odwróconymi argumentami*)
		else let m = join l_right r 						(* Łączę prawe poddrzewo pierwszej kolejki z drugą kolejką*)
		in if height l_left < height m then 			(* Lewym poddrzewem zostaje te o większej wysokości *)
			Node (m, l_left, l_value, l_height + 1) 	(*Tutaj dwa połączone stają się lewym, lewe staje się prawym, bo lewe ma mniejszą wysokość *)
		else
			Node(l_left, m, l_value, height m + 1) 	(* Tutaj na odwrót *)

exception Empty

let delete_min q =
	match q with
	| Null -> raise Empty 																  (* Wyjątek *)
	| Node (q_left, q_right, q_value, _) -> (q_value, join q_left q_right) (* Zwraca wartość na szczycie kolejki i łączy dwa poddrzewa*)
	
let add e q =
	join (Node (Null, Null, e, 1)) q
