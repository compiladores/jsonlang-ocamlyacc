%{
open Ast
%}

%token <int> NUMBER
%token <string> STRING
%token <string> STRINGLIST
%token <string> MINUS
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
%token LBRACKET
%token RBRACKET
%token RETURN
%token FUNCTION
%token SEMICOLON
%token COMMA
%token QUOTE
%token EOF

%start <Ast.statement list> prog

%%

prog:
	| e = statement+; EOF { e }
	;

statement:
	| LBRACKET; sArray = separated_list(COMMA, statement); RBRACKET; SEMICOLON; {StatementBlock(sArray)}
	| IF; e = expression; THEN; s1 = statement; ELSE; s2 = statement; SEMICOLON; { If (e, s1, s2) }
	| IF; e = expression; THEN; s = statement; SEMICOLON; { If_no_else (e, s) }
	| WHILE; e = expression; DO; s = statement; SEMICOLON; { While (e, s) }
	| FOR; x = STRING; FROM; e1 = expression; TO; e2 = expression; STEP; e3 = expression; DO; s = statement; SEMICOLON; { Iterator(x, e1, e2, e3, s) }
	| FOR; x = STRING; FROM; e1 = expression; TO; e2 = expression; DO; s = statement; SEMICOLON; { Iterator_no_step(x, e1, e2, s) }
	| DO; s = statement; UNTIL; e = expression; SEMICOLON; { Do (s, e) }
	| LET; x = STRING; EQUALS; e = expression; SEMICOLON; { Declare (x, e) }
	| x = STRING; EQUALS; e = expression; SEMICOLON; { Set (x, e) }
	| FUNCTION; x1 = STRING; LPAREN; x2 = STRINGLIST; RPAREN; LBRACE; s = statement; RBRACE; SEMICOLON; { DeclarationStatement(x1, x2, s) }
	| x = STRING; LPAREN; eArray = separated_list(COMMA, expression); RPAREN; SEMICOLON; { Call(x, eArray) }
	| RETURN; e = expression; SEMICOLON; { Return(e) }
	| BREAK; SEMICOLON; { Break }
	| CONTINUE; SEMICOLON; { Continue }
	;

expression:
	| u = MINUS; e = expression { Unop(u, e) }
	| u = UNOP; e = expression { Unop(u, e) }
	| e1 = expression; b = MINUS; e2 = expression { Binop(b, e1, e2) }
	| e1 = expression; b = BINOP; e2 = expression { Binop(b, e1, e2) }
	| QUOTE; s = STRING; QUOTE;{ Literal(s) }
	| x = STRING; LPAREN; eArray = separated_list(COMMA, expression); RPAREN { Call(x, eArray) }
	| s = STRING { String(s) }
	| n = NUMBER { Number(n) }
	;

	
// LucasLang
// ------------------

// IF 								if ... then ...;
// IF 								if ... then ... else ...;
// WHILE 							while ... do ...;
// FOR 								for ... form ... to ... step ... do ...;
// FOR 								for ... form ... to ... do ...;
// DO 								do ... until ...;
// BREAK 							break;
// CONTINUE 					continue;
// DECLARE 						let ... = ...;
// SET 								... = ...;
// CALL FUNCTION 			... (...);
// RETURN 						return ...;

// DECLARE FUNCTION 	func ... ([... , ...]){};
