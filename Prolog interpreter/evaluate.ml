open Type;;
open Unifier;;
exception Cant_evaluate;;

let c = ref 0;; (* to be used to create unique variables*)

let get_unique_var() = (c:= !c + 1;"__VAR_"^(string_of_int !c));;


let rec justhead(l)=
	match l with [] -> [];
	|hd::tl -> extractHead(hd)::justhead(tl);;

let find_subs_for_unique_vars(a) =
	let rho = ref [] in
	let rec loop(a)= match a with Z(_) -> ();
		| V(x) -> if (List.exists (equal (x)) (justhead !rho)) then (();) else
					rho:= !rho@[(x,V(get_unique_var()))]
		| NZ(_,l) -> let rec loop1(l) = match l with [] -> ();
							| hd::tl -> loop(hd);loop1(tl);
					in loop1(l);
	in loop(a);!rho;;

let find_subs_for_unique_vars_alist(alist,sigma) =
	let rec loop(a) = match a with [] -> [];
		| hd::tl -> compose(find_subs_for_unique_vars(hd),loop(tl),sigma);
	in loop(alist);;

(*making clse unique given a sigma(signature) *)
let make_unique_vars(clse,sigma) =
	match clse with F(Head(a)) -> let rho = find_subs_for_unique_vars(a) 
								 in F(Head(subs(a,sigma,rho)));
			|R(Headbody(a,al)) -> 
			let rho = compose(find_subs_for_unique_vars(a),find_subs_for_unique_vars_alist(al,sigma),sigma) in
				R(Headbody(subs(a,sigma,rho),subslist(al,sigma,rho)));;




(* we call this function when we are sure that the list of atoms queried are all well formed*)
(* we will get an atom to evluate (from command line),
against a LIST of clauses-pllist that havent been checked yet 
substituion rho that is evaluated yet 
cont is to be evaluated	
fc is a higher order fun*)

let rec eval at pllist plcomplprog sigma rho sc fc cont = 
	 let at' =  subs(at,sigma,rho) in
		match pllist with [] -> sc (false,[]) fc;
		|hd::tl -> 
			let hdclause = make_unique_vars(hd,sigma) in
				(match hdclause with 
					F(Head(a)) -> (try (
							let uni = compose(rho,mgu(at',a,sigma),sigma) in sc (true,uni) (fun() -> eval at' tl plcomplprog sigma rho sc fc cont);
									) with NotUnif ->  
									eval at' tl plcomplprog sigma rho sc fc cont);
					| R(Headbody(a,al)) -> (try(
							let uni = compose(rho,mgu(at',a,sigma),sigma) in 
							evallist al plcomplprog plcomplprog sigma uni 
							(fun vt fc' -> sc vt fc') (fun() -> eval at' tl plcomplprog sigma rho sc fc cont) fc ;
							) with NotUnif ->  
									eval at' tl plcomplprog sigma rho sc fc cont);
					| _ -> raise Cant_evaluate;
			)
and
evallist al pllist plcomplprog sigma rho sc fc cont = 
	match al with [] -> sc (true,rho) fc;
	| hd::tl ->eval hd pllist plcomplprog sigma rho(* evaluate first term *)
	  	(fun vt1 fc1 ->
	    	 if fst vt1 then
	    	   evallist tl pllist plcomplprog sigma (compose(rho,snd vt1,sigma))
				 (fun vt2 fc2 -> sc vt2 fc2) fc1 cont (* if first term returns true in evaluation then the other one will be tried to be evaluated *)
	    	 else sc (false,[]) fc1) fc cont;;
	
let filter (al,rho) =
    	let rec all_vars = function([]) -> []
    		| hd::tl -> vars(hd)@all_vars(tl);
    	in List.filter (fun (var,_) -> List.exists (fun v -> v = var) (all_vars(al))) rho ;;

let interprete_atom (al, plprog,sigma) =
	let more() =
    	print_string "More? ";
    	if read_line() = ";" then  true 
    	else  false 
    in 
    	evallist al plprog plprog sigma [] 
    	(fun vt fc -> if fst vt then
	 		(print_string "Yes\n"; print_mgu (filter (al,snd vt)); print_endline "";
	  			if more() then fc() else ())
       		else
	 	fc())
     	(fun () -> print_string "No\n") (fun () -> ());; 





