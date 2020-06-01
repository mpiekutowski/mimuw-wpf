(** Zadanie: Sortowanie Topologiczne, Autor: Miłosz Piekutowski, Code Review: Paweł Putra*)

open PMap

exception Cykliczne

type case = Pre | In | Post

type 'a node = {mutable case: case; mutable list: 'a list}

(** Funkcja zwraca mapę będącą reprezentacją grafu zadanego przez listę l *)
let make_graph l =

	(** Najpierw do grafu dodawane są wszystkie wierzchołki mające stopień wejścia większy od 0 *)
	let rec make_nodes map = function
		| [] -> map
		| (a, lst)::t ->
			let rec loop acc = function
				| [] -> acc
				| h::t -> loop (add h {case = Pre; list = []} acc) t in
					make_nodes (loop map lst) t in
					
	let graph = make_nodes empty l in

	(** Następnie dodawane są krawędzie i wierzchołki o stopniu wejścia równym 0 *)
	let rec make_edges map = function
		| [] -> map
		| (a, lst)::t ->
			if exists a map then
				let v = find a map in
				v.list <- lst @ v.list;
				make_edges map t
			else 
				make_edges (add a {case = Pre; list = lst} map) t in
	
	make_edges graph l 
    
(** Sortowanie topologiczne grafu przy użyciu algorytmu DFS *)
let topol l =
	let graph = make_graph l in
	let res = ref [] in
	let rec dfs a =
		let v = find a graph in
		match v.case with
		| Pre ->
			v.case <- In;
			List.iter dfs v.list;
			res := a::(!res);
			v.case <- Post
		| In -> raise Cykliczne
		| Post -> () in
	List.iter (fun (a, _) -> dfs a) l;
	!res
