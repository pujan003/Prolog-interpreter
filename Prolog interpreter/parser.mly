
%{
	open Type;;

	exception Invalid_grammar
	
%}

%token DOT 
%token IF 
%token COMMA 
%token NOT
%token LPAREN RPAREN
%token <string> VAR PREDICATE 
%token EOL 

%start cl 
%type <Type.clause> cl
%start query
%type <Type.atom list> query 
%%
cl :
	rule 	{R($1)}
	|fact     {F($1)}
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};
fact :
	atom DOT 	{Head($1)}
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};
rule :
	atom IF body DOT {Headbody($1,$3)}
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};
body :
	atom 			{[$1]}
	|atom COMMA body {$1 :: $3 }
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};
atom:
	PREDICATE {Z($1)}
	|PREDICATE LPAREN termlist RPAREN {NZ($1,$3)}
	| NOT atom {Not($1)}
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};
termlist:
	|term {[$1]}
	|term COMMA termlist {$1 :: $3}
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};
term:
	|atom {$1}
	|VAR {V($1)}
	|error    {print_string("Grammar is invalid\n "); raise Invalid_grammar};

query : body DOT {$1};