{
open Parser        
exception Eof
exception Invalid_character
}
let var = ['A'-'Z']['a'-'z' '0'-'9' 'A'-'Z' ''' '_']* 
let predicate = ['a'-'z']['a'-'z' '0'-'9' 'A'-'Z' ''' '_']*

rule token = parse
            [' ' '\t' '\n' '\r']     { token lexbuf }     (* skip blanks *)
          | '.'            { DOT }
          | ":-"           { IF }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | ','            { COMMA }
          | "\+"           {NOT}
          | var as v       {VAR(v)}
          | predicate  as p{PREDICATE(p)}
          | eof            { raise Eof }
          | _ as c 		{ print_char(c);print_string("\n");raise Invalid_character}
          