(** Zadanie: Przelewanka, Autor: Miłosz Piekutowski, Code Review: Michał Leszczyński **)

(** Funkcja obliczająca największy wspólny dzielnik **)
let rec gcd a b = 
	if b = 0 then a else gcd b (a mod b)

(** Funkcja sprawdzająca warunki konieczne uzyskania oczekiwanego wyniku **)
let check a = 
	let vol = List.exists (fun (l, r) ->  l = r || r = 0) (Array.to_list a)
	and gcd_l = Array.fold_left (fun acc (l, _) -> gcd acc l) (a.(0) |> fst) a
	and gcd_r = Array.fold_left (fun acc (_, r) -> gcd acc r) (a.(0) |> snd) a in
	vol && (gcd_r mod gcd_l = 0)
	
let przelewanka a =

	(** Usunięcie elementów (0,0) z tablicy **)
	let a = Array.of_list (List.filter (fun (l, _) -> l > 0) (Array.to_list a)) in
	let n = Array.length a in
	if n = 0 then 0 
	else if not (check a) then -1
	else (
		
		(** Inicjalizacja zmiennej wynikowej, kolejki, hashmapy oraz tablic pomocniczych **)
		let q = Queue.create ()
		and res = ref (-1)
		and tab = Hashtbl.create 424242
		and first = Array.make n 0 
		and volume = Array.map (fun (l, _) -> l) a 
		and last = Array.map (fun (_, r) -> r) a in
		
		(** Funkcja dodająca do kolejki stan, który nie został jeszcze przetworzony **)
		let add pos moves =
			if not (Hashtbl.mem tab pos) then
				Queue.add (pos, moves) q in
		
		(** Funkcja dodająca stany, w których jedna ze szklanek została wypełniona **)
		let pour_in pos moves =
			for i = 0 to n - 1 do 
				if pos.(i) < volume.(i) then (
					let next = Array.copy pos in
					next.(i) <- volume.(i);
					add next (moves + 1)	 
				)
			done in
		
		(** Funkcja dodająca stany, w których z jednej ze szklanek została wylana woda **)	
		let pour_out pos moves = 
			for i = 0 to n - 1 do
				if pos.(i) <> 0 then (
					let next = Array.copy pos in
					next.(i) <- 0;
					add next (moves + 1)				
				)
			done in
		
		(** Funkcja dodająca stany, w których woda została przelana z jednej szklanki do drugiej **)
		let pour_bet pos moves =
			for i = 0 to n - 1 do 
				for j = 0 to n - 1 do 
					if i <> j then (
						let amount = min pos.(i) (volume.(j) - pos.(j)) in
						if amount <> 0 then (
							let next = Array.copy pos in						
							next.(i) <- next.(i) - amount;
							next.(j) <- next.(j) + amount;
							add next (moves + 1)
						)
					)
				done
			done in
		
		(** Algorytm wyszukiwania wszerz w grafie tworzonym na bieżąco z powstających stanów **)
		Queue.add (first, 0) q;
		while !res = -1 do
			let (pos, moves) = Queue.pop q in
			if not (Hashtbl.mem tab pos) then (
				Hashtbl.add tab pos moves;
				if pos = last then 
					res := moves
				else (
					pour_in pos moves;
					pour_out pos moves;
					pour_bet pos moves				
				)
			)
		done;
		!res	
	)
