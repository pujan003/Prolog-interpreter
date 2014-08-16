open Lexer;;
open Parser;;
open Type;;

let convert_into_plprog(inchannelfile) =
	let plprog  = ref [] in
		let c = inchannelfile in
        try(
    	let lexbuf = Lexing.from_channel c in
    		
            while true do
              plprog := (!plprog) @ [Parser.cl Lexer.token lexbuf];
            done;
            close_in(c);!plprog)
         with Lexer.Eof ->
           close_in(c);( !plprog );;
           
          
