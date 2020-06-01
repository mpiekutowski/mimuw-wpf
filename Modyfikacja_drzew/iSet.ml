(** Zadanie: Modyfikacja drzewa, Autor: Miłosz Piekutowski, Code Review: Jakub Zacharczuk**)

(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Reprezentacja drzewa **)
type t =
	| Node of t * (int * int) * t * int * int 
	| Empty

(** Zwraca długość ścieżki do najdalszego liścia **)
let height = function 
	| Node (_, _, _, h, _) -> h
	| Empty -> 0 

(** Zwraca liczbę liczb w drzewie **)
let size = function
	| Node (_, _, _, _, s) -> s
	| Empty -> 0

(** Zwraca wartość minimalną z (max_int, a + b) **)
let plus a b =
	if a + b < 0 then max_int
	else a + b

(** Zwraca wartość minimalną z (max_int, długość przedziału [a,b]) **)
let lng (a, b) =
	if b - a + 1 <= 0 then max_int 
	else b - a + 1

(** Zwraca wierzchołek o danych poddrzewach i elemencie **)
let make l k r =
	Node (l, k, r, max (height l) (height r) + 1, plus (plus (size l) (size r)) (lng k))

(** Balansuje drzewo używając rotacji **) 
let bal l k r =
	let hl = height l in
	let hr = height r in
	if hl > hr + 2 then
		match l with
		| Node (ll, lk, lr, _, _) ->
			if height ll >= height lr then make ll lk (make lr k r)
			else
				(match lr with
				| Node (lrl, lrk, lrr, _, _) ->
					make (make ll lk lrl) lrk (make lrr k r)
				| Empty -> assert false)
		| Empty -> assert false
	else if hr > hl + 2 then
		match r with
		| Node (rl, rk, rr, _, _) ->
			if height rr >= height rl then make (make l k rl) rk rr 
			else
			 (match rl with
			 | Node (rll, rlk, rlr, _, _) ->
			 	make (make l k rll) rlk (make rlr rk rr)
			 | Empty -> assert false)
		| Empty -> assert false
	else make l k r

(** Zwraca minimalny element w drzewie **)
let rec min_elem = function
	| Node (Empty, k, _, _, _) -> k
	| Node (l, _, _, _, _) -> min_elem l
	| Empty -> raise Not_found

(** Zwraca drzewo bez minimalnego elementu **)
let rec remove_min_elem = function
	| Node (Empty, _, r, _, _) -> r
	| Node (l, k, r, _, _) -> bal (remove_min_elem l) k r 
	| Empty -> invalid_arg "PSet.remove_min_elem"

(** Zwraca maksymalny element w drzewie **)
let rec max_elem = function
	| Node (_, k, Empty, _, _) -> k
	| Node (_, _, r, _, _) -> max_elem r
	| Empty -> raise Not_found

(** Zwraca drzewo bez maksymalnego elementu **)
let rec remove_max_elem = function
	| Node (l, _, Empty, _, _) -> l
	| Node (l, k, r, _, _) -> bal l k (remove_max_elem r)
	| Empty -> invalid_arg "PSet.remove_max_elem"

let empty = Empty

let is_empty s =
 s = Empty

(** Dodaje przedział (l, r) do drzewa przy założeniu, że przecięcie (l - 1, r + 1) z elementami drzewa jest puste **)
let rec add_one x = function
	| Node (l, k, r, h, s) ->
		if x = k then Node (l, x, r, h, s)
		else if x < k then
			let nl = add_one x l in
			bal nl k r
		else
			let nr = add_one x r in
			bal l k nr
	| Empty -> make Empty x Empty 

(** Zwraca drzewo AVL będące połączeniem dwóch innych i jednego przedziału przy założeniu, że przecięcie (l - 1, r + 1) z elementami poddrzew jest puste **)
let rec join l v r =
	match l, r with
	| Empty, _ -> add_one v r
	| _, Empty -> add_one v l
	| Node (ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _) ->
		if lh > rh + 2 then 
			bal ll lv (join lr v r )
		else if rh > lh + 2 then 
			bal (join l v rl) rv rr
		else
			make l v r
			
(** Zwraca drzewo AVL o elementach mniejszych od x; prawdę jeżeli x należy do s; drzewo AVL o elementach większych od x **)
let split x s =
	let rec loop x = function 
	| Empty -> (Empty, false, Empty)
	| Node (l, (vl, vr), r, _, _) ->
		if (x >= vl) && (x <= vr) then
			let left = 
				if (vl < x) then
					add_one (vl, x - 1) l 
				else l
			and right = 
				if (vr > x) then
					add_one (x + 1, vr) r
				else r 
			in
			(left, true, right)
		else if (x, x) < (vl, vr) then
			let (ll, pres, rl) = loop x l in
			(ll, pres, join rl (vl, vr) r)
		else
			let (lr, pres, rr) = loop x r in
			(join l (vl, vr) lr, pres ,rr)
	in
	loop x s

(** Łączy dwa drzewa, przy czym pierwsze ma wszystkie elementy mniejsze od wszystkich drugiego **)
let merge s1 s2 =
	match s1, s2 with 
	| Empty, _ -> s2
	| _, Empty -> s1
	| _ ->
		let k = min_elem s2 in
		join s1 k (remove_min_elem s2)
		
(** Zwraca true jeżeli x należy do drzewa i false w przeciwnym wypadku **)
let mem x s =
	let (_, ans, _) = split x s in
	ans

(** Zwraca drzewo s z usuniętym przedziałem [x, y] **)
let remove (x, y) s =
	let (l, _, nr) = split x s in
	let (_, _, r) = split y nr in
	merge l r 

(** Zwraca drzewo AVL z dodanym przedziałem [x, y] **)
let add (x, y) s =
	let (l, _, nr) = split x s in
	let (_, _, r) = split y nr in
	let (l, (x, _)) =
		if mem (x - 1) l then
			(remove_max_elem l, max_elem l)
		else 
			(l, (x, y)) in
	let ((_, y), r) =
		if mem (y + 1) r then
			(min_elem r, remove_min_elem r)
		else
			((x, y), r) in
	join l (x, y) r

(** Zwraca liczbę liczb w drzewie <= x **)
let below x s =
	if x = max_int then 
		size s
	else
		let (l, _, _) = split (x + 1) s in 
		size l

(** Zwraca listę elementów drzewa w kolejności infiksowej **)
let elements s = 
    let rec loop acc = function
        | Empty -> acc
        | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
    loop [] s 

(** Wykonuje funkcję f na wierzchołkach drzewa w kolejności infiksowej **)
let iter f s =
	 List.iter f (elements s)

(** Składa funkcję f na wierchołkach drzewa w kolejności infiksowej **)
let fold f s acc =
	let f a b = f b a in 
	List.fold_left f acc (elements s)	
