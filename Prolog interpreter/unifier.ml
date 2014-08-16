open Type;;

exception Invalid_signature;;
exception Invalid_symbol_in_term;;
exception Invalid_arity;;
exception NotUnif;;

type sigma = (string*int) list;;
(*preterm sigma (var) ,V and Z are the leaf nodes*)
type substitution = (name*atom) list;;
(*The substitution function identified by its list of non identity mappings*)

let extractHead(s) = match s with (a,_) -> a;;
let extractTail(s) = match s with (_,i) -> i;;
let length(s) = let rec loop(s,i) = match s with [] -> i 
                                | hd::tl -> loop(tl,i+1);
                in loop(s,0);;

(* Checks if a given signature is valid or not*)
let validSig(s) = let checkhash = Hashtbl.create(111) in
  let rec loop(s)=
  match s with [] -> true;
  |hd :: tl -> try (Hashtbl.find checkhash (extractHead(hd));false;) with Not_found
          ->( Hashtbl.add checkhash (extractHead(hd)) (extractTail(hd));
          if extractTail(hd) < 0 then false else loop(tl));
  in loop(s);;

(* Creats a hash table for a given signature so as to efficiently find arity of any symbol*)
let createSigmaHash(s) = 
  if validSig(s)=false then raise(Invalid_signature)
  else
     let hash = Hashtbl.create(111) in
      let rec loop(s) =
          match s with [] -> hash;
          |hd :: tl -> Hashtbl.add hash (extractHead(hd)) (extractTail(hd)) ; loop(tl);
      in loop(s);;
      
(*Checks if a given term satisifes its arity for a given hashtable of a signature*)
let rec check(pl,sigHash) = 
  match pl with Z(a) -> (try ((Hashtbl.find sigHash a)=0) with Not_found -> raise(Invalid_symbol_in_term));
    |NZ(a,l) -> (try ((Hashtbl.find sigHash a)=length(l)) with Not_found -> raise(Invalid_symbol_in_term));
    | V(_) -> true;
    |Not(a) -> check(a,sigHash);
    | _ -> raise(Invalid_symbol_in_term);;


let rec makeSig_atom(at,s_) = 
  let s = ref s_ in
  let rec loop1(pl) = let h=createSigmaHash(!s) in
  match pl with Z(a)-> (try (if check(Z(a),h)=false then raise(Invalid_arity) else (();)); 
                        with Invalid_symbol_in_term -> s:= !s@[(a,0)]);
  | V(_) -> ((););
  | NZ(a,l) -> (try (if (check(pl,h)=false) then raise(Invalid_arity)
                else (();)); with Invalid_symbol_in_term -> s:= !s @[(a,length(l))]);
                let rec loop2(l) = 
                   match l with [] -> ((););
                        | hd::tl -> (loop1(hd));loop2(tl);
                    in loop2(l);
  | Not(a) -> makeSig_atom(a,s_);
    in loop1(at);!s;;

(*Makes a signature from a clause*)
 
 let makeSig_clause(c,s_) =
    let s = ref s_ in
    match c with R(Headbody(a,al)) -> s := (makeSig_atom(a,!s));
                                    let rec loop(l) = 
                                            match l with [] -> ((););
                                          | hd::tl -> s:=makeSig_atom(hd,!s);loop(tl)
                                    in loop(al);!s
      |F(Head(a)) ->  s:=makeSig_atom(a,!s); !s;;

(* Final program LIST to construct a signature from given prolog program*)
let makeSig_plprog(pl)=
  let s = ref [] in 
  let rec loop(l)=
    match l with [] -> ();
              |hd::tl -> s:=makeSig_clause(hd,!s);loop(tl)
    in loop(pl); !s;; 

(*Checks if a given term of a sigma is well-formed, it raises exception if there is any invalid symbol 
or if it does not satisfy its arity *)
let rec wf(pl,s) = let h=createSigmaHash(s) in
let t = ref true in
  let rec loop1(pl) = 
  match pl with Z(a)->(try (if check(Z(a),h)= false then raise(Invalid_arity) else ();) 
						with Invalid_symbol_in_term -> t:=false;)
  | V(_) -> ();
  | NZ(_,l) -> (try (if (check(pl,h)=false) then raise(Invalid_arity)
                	else 
               			let rec loop2(l) = 
                  			 match l with [] -> ();
                      			  | hd::tl -> if (check(hd,h)=false) then raise(Invalid_arity)
                                     else loop1(hd);loop2(tl);
                    	in loop2(l); ) with Invalid_symbol_in_term -> t:=false;)
  |Not(a) -> wf(a,s);
    in loop1(pl);!t;;
                    
 
(*Creates a hash table of a given subsitution for a sigma,gives appropriate error messages if the substitution
is not well defined, i.e contains illegal terms for a given sigma*)
let createSubsHash(sl,sigma) =
   if validSig(sigma)=false then raise(Invalid_signature)
  else
     let hash = Hashtbl.create(1111) in
      let rec loop(sl) =
          match sl with [] -> hash;
          |hd :: tl -> (try (wf(extractTail(hd),sigma);Hashtbl.add hash (extractHead(hd)) (extractTail(hd)))
                         with e -> (if e = Invalid_arity then print_string("Check arity of the terms\n")
                                    else print_string("Invalid symbol in term\n")));loop(tl);
      in loop(sl);;

(*Performs a given substitution on a given term of a sigma *)
let subs(term,sigma,rho) = 
  
  let subh=createSubsHash(rho,sigma) and v = wf(term,sigma) in  
  let rec loop1(pl) = 
  match pl with Z(a)->[Z(a)];
  | V(a) -> (try [Hashtbl.find subh a] with Not_found -> [V(a)]);
  | NZ(a,l) -> let rec loop2(l) = 
                   match l with [] -> [];
                        | hd::tl ->loop1(hd)@loop2(tl) ;
                    in [NZ(a,loop2(l))];
    in 
    match loop1(term) with hd::tl -> hd;;

let subslist(tl,sigma,rho) = 
  let rec loop(tl) = match tl with [] -> [];
              | hd::t -> subs(hd,sigma,rho)::loop(t);
  in loop(tl);;

let subsclasue(clse,sigma,rho)=
	 match clse with F(Head(at)) -> F(Head(subs(at,sigma,rho)));
				| R(Headbody(at,al)) -> R(Headbody(subs(at,sigma,rho),subslist(al,sigma,rho)));;

(*Composes two given substitution on a term sigma, and gives the composed subsititution as output*)
let compose(rho1,rho2,sigma) =
    let h1 = createSubsHash(rho1,sigma) in
    let rec loop1 = function([]) -> [];
                  | hd::tl -> (extractHead(hd),subs(extractTail(hd),sigma,rho2))::loop1(tl);
    in let rec loop2 = function([]) -> [];
                  | hd::tl -> (try (Hashtbl.find h1 (extractHead(hd));loop2(tl)) with Not_found -> hd::loop2(tl));
    in loop1(rho1)@loop2(rho2);;


(*Lists down all the variables in a given term. Helper function for the "Occures check" in mgu algo*)
let rec vars = function(V(a)) -> [a];
              |Z(_) -> [];
              |NZ(a,l) -> let rec loop(l) = match l with [] -> [] 
                                  | hd::tl -> vars(hd)@loop(tl)
                                in loop(l);;

let equal a b = if a=b then true else false;;

(*Finds a mgu of two given terms of a sigma*)
let mgu(t1,t2,sigma) = 
  let rec loop = function(Z(a),Z(b)) -> if (a=b) then [] else raise(NotUnif);
                |(Z(_),NZ(_)) -> raise(NotUnif);
                |(NZ(_),Z(_)) -> raise(NotUnif);
                |(V(a),V(b)) -> if (a=b) then [] else [a,V(b)];
                |(V(a),Z(b)) -> [a,Z(b)];
                |(Z(b),V(a)) -> [a,Z(b)];
                |(V(a),NZ(b,l)) -> if (List.exists (equal (a)) (vars(NZ(b,l)))) then raise(NotUnif) 
                                      else [a,NZ(b,l)] ;
                |(NZ(b,l),V(a)) -> if (List.exists (equal (a)) (vars(NZ(b,l)))) then raise(NotUnif) 
                                      else [a,NZ(b,l)] ;
                |(NZ(a,l1),NZ(b,l2)) -> if a<>b then raise(NotUnif) else 
                                     let rec loop2 = function([],[],s) ->[];
                                      | (hd1::tl1,hd2::tl2,s) -> 
                                        let s_ = loop(subs(hd1,sigma,s),subs(hd2,sigma,s)) in
                                        s_@loop2(tl1,tl2,compose(s,s_,sigma))
                                        in loop2(l1,l2,[]);
  in loop(t1,t2);;

(*Finds a mgu for the list of given terms of a sigma*)
  let mguOfList(l,sigma) =
    let rec loop = function([],mg) -> mg;
                | (hd1::hd2::tl,mg) -> let mg_ = mgu(subs(hd1,sigma,mg),subs(hd2,sigma,mg),sigma) in
                                        loop(hd2::tl,compose(mg,mg_,sigma));  
                |(hd::tl,mg) -> mg 
    in loop(l,[]);;



let rec string_of_atom(a) = 
  let rec string_of_atomlist(al) =
    match al with [] ->"";
    |[hd] -> string_of_atom(hd);
    |hd::tl -> string_of_atom(hd)^","^string_of_atomlist(tl);
  in
  match a with V(z) -> z;
  |Z(z) -> z;
  |NZ(z,zl) -> z ^ "(" ^ string_of_atomlist(zl) ^ ")";
  |_ -> "";;

let rec print_mgu(rho) = 
  match rho with [] -> print_string("");
  | (a,b)::tl -> print_string(a^" = "^(string_of_atom b)^"\n");
      print_mgu(tl);;