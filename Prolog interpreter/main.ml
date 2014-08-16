open Evaluate;;
open Tokenparse;;
open Unifier;;
exception Improper_query;;


let rec wf_queryterms(al,sigma)=
  match al with [] -> ();
  | hd::tl -> if wf(hd,sigma) then wf_queryterms(tl,sigma)
             else raise(Improper_query);;

(* Sigma of the program against which the query is run*)
let rec convert_query(sigma) =
  try(let line = read_line() in 
      if (line="Q") then exit 0 else
    	                     let lexbuf = Lexing.from_string line in
    		                       let  plquery= Parser.query Lexer.token lexbuf in
                                  wf_queryterms(plquery,sigma);plquery;)
                           with Lexer.Eof ->
                                   exit 0
                            | Improper_query->
                              print_string("No\nThe query terms seems to be ill-formed. Try again.\n");
                              print_string(" ?- ");flush stdout;convert_query(sigma);
                            | Invalid_arity ->
                              print_string("No\nThe query terms seems to have correct predicate but with wrong arity. Try again.\n");
                              print_string(" ?- ");flush stdout;convert_query(sigma);
                            | Lexer.Invalid_character ->
                              print_string("No\nThe query seems to have illegal tokens. Try again\n");
                              print_string(" ?- ");flush stdout;convert_query(sigma);
                            | _ -> (*This exception is Parser.Invalid_grammar *)
                              print_string("No\nThe query seems to have invalid grammar. Try again\n");
                              print_string(" ?- ");flush stdout;convert_query(sigma);;

(*The filename of the plprogram*)
let main(plfile) = 
  let cin = open_in plfile in
    (try (let plprog = convert_into_plprog(cin) in
      let sigma = makeSig_plprog(plprog) in 
      while true do
          print_string(" ?- ");flush stdout;
          let myquery = convert_query(sigma) in
          interprete_atom (myquery, plprog,sigma); 
      done;)
       with Invalid_arity ->
         print_string("The program code contains ill formed terms.Check if there are multiple clauses with same predicate and different arities.\n");
        | Lexer.Invalid_character ->
        print_string("The program code seems to have illegal tokens.\n");
        | _ -> (*This exception is Parser.Invalid_grammar *)
        print_string("The program code seems to have invalid grammar.\n"));;

main(Sys.argv.(1));;

