%{
open Ast
%}

%token <int> NUMBER
%token <string> STRING
%token <string> UNOP
%token <string> BINOP

%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token FOR
%token FROM
%token TO
%token STEP
%token UNTIL
%token BREAK
%token CONTINUE
%token LET
%token EQUALS
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token RETURN
%token EOF

// %token <int> INT
// %token <string> ID
// %token TRUE
// %token FALSE
// %token LEQ
// %token TIMES  
// %token PLUS
// %token LPAREN
// %token RPAREN
// %token LET
// %token EQUALS
// %token IN
// %token IF
// %token THEN
// %token ELSE
// %token EOF

// %nonassoc IN
// %nonassoc ELSE
// %left LEQ
// %left PLUS
// %left TIMES  

// %start <Ast.jsonLang> prog
%start <Ast.statement> prog

%%

prog:
	// | e = jsonLang; EOF { e }
	| e = statement; EOF { e }
	;

jsonLang:
	| e = topStatement { e }
	//TODO: Add array of topStatements
	;

topStatement: 
 	| e = statement { e }
 	| e = declarationStatement { e }
 	;

statement:
	//TODO: make else optional
	| IF; e = expression; THEN; s1 = statement; ELSE; s2 = statement { If (e, s1, s2) }
	| WHILE; e = expression; DO; s = statement { While (e, s) }
	//TODO: Add array of statements
	//TODO: make step optional
	| FOR; x = STRING; FROM; e1 = expression; TO; e2 = expression; STEP; e3 = expression; DO; s = statement; { Iterator(x, e1, e2, e3, s) }
	| DO; s = statement; UNTIL; e = expression { Do (s, e) }
	| LET; x = STRING; EQUALS; e = expression { Declare (x, e) }
	| x = STRING; EQUALS; e = expression { Set (x, e) }
	//TODO: | x = STRING; LPAREN; eArray = expression array; RPAREN { Call(x, eArray) }
	| x = STRING; LPAREN; eArray = expression; RPAREN { Call(x, eArray) }
	| RETURN; e = expression { Return(e) }
	| BREAK { Break }
	| CONTINUE { Continue }
	;

expression:
	| u = UNOP; e = expression { Unop(u, e) }
	| e1 = expression; b = BINOP; e2 = expression { Binop(b, e1, e2) }
	//TODO: | x = STRING; LPAREN; eArray = expression array; RPAREN { Call(x, eArray) }
	| x = STRING; LPAREN; eArray = expression; RPAREN { Call(x, eArray) }
	| s = STRING { String(s) }
	| n = NUMBER { Number(n) }
	;

declarationStatement:
	//TODO: | x = STRING; LPAREN; sArray = string array; RPAREN; LBRACE; s = statement; RBRACE { DeclarationStatement(x, sArray, s) }
	| x = STRING; LPAREN; sArray = STRING; RPAREN; LBRACE; s = statement; RBRACE { DeclarationStatement(x, sArray, s) }
	;

printable:
	//TODO: make else optional
	| IF; e = printable; THEN; s1 = printable; ELSE; s2 = printable { If (e, s1, s2) }
	| WHILE; e = printable; DO; s = printable { While (e, s) }
	//TODO: Add array of statements
	//TODO: make step optional
	| FOR; x = STRING; FROM; e1 = printable; TO; e2 = printable; STEP; e3 = printable; DO; s = printable; { Iterator(x, e1, e2, e3, s) }
	| DO; s = printable; UNTIL; e = printable { Do (s, e) }
	| LET; x = STRING; EQUALS; e = printable { Declare (x, e) }
	| x = STRING; EQUALS; e = printable { Set (x, e) }
	//TODO: | x = STRING; LPAREN; eArray = expression array; RPAREN { Call(x, eArray) }
	| x = STRING; LPAREN; eArray = printable; RPAREN { Call(x, eArray) }
	| RETURN; e = printable { Return(e) }
	| BREAK { Break }
	| CONTINUE { Continue }
	| u = UNOP; e = printable { Unop(u, e) }
	| e1 = printable; b = BINOP; e2 = printable { Binop(b, e1, e2) }
	//TODO: | x = STRING; LPAREN; eArray = expression array; RPAREN { Call(x, eArray) }
	| x = STRING; LPAREN; eArray = printable; RPAREN { Call(x, eArray) }
	| s = STRING { String(s) }
	| n = NUMBER { Number(n) }
	;

// expr:
// 	| i = INT { Int i }
// 	| x = ID { Var x }
// 	| TRUE { Bool true }
// 	| FALSE { Bool false }
// 	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
// 	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
// 	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
// 	| LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
// 	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
// 	| LPAREN; e=expr; RPAREN {e} 
// 	;
	
// lenguaje inventado
// ------------------

// IF 								if ... then ... else ...
// WHILE 							while ... do ...
// FOR 								for ... form ... to ... step ... do ...
// DO 								do ... until
// BREAK 							break
// CONTINUE 					continue
// DECLARE 						let ... = ...
// SET 								... = ...
// CALL FUNCTION 			... (...)
// RETURN 						return ...

// DECLARE FUNCTION 	... (...){}
