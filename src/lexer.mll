{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let number = digit+
let letter = ['a'-'z' 'A'-'Z']
let string = letter+
let stringComma = string white* ","?
let minus = ['-']
let unop = ['-' '!' '~']
let binopSingle = ['+' '-' '*' '/' '^' '%' '&' '|' '<' '>']

rule read = 
  parse
  | white { read lexbuf }

  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "for" { FOR }
  | "from" { FROM }
  | "to" { TO }
  | "step" { STEP }
  | "until" { UNTIL }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "let" { LET }
  | ">>" {BINOP (Lexing.lexeme lexbuf)}
  | "<<" {BINOP (Lexing.lexeme lexbuf)}
  | "<=" {BINOP (Lexing.lexeme lexbuf)}
  | ">=" {BINOP (Lexing.lexeme lexbuf)}
  | "==" {BINOP (Lexing.lexeme lexbuf)}
  | "~=" {BINOP (Lexing.lexeme lexbuf)}
  | "and" {BINOP (Lexing.lexeme lexbuf)}
  | "or" {BINOP (Lexing.lexeme lexbuf)}
  | "=" { EQUALS }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" white* stringComma+ white* "]" { STRINGLIST (Lexing.lexeme lexbuf) }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "return" { RETURN }
  | "func" { FUNCTION }

  | number { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (Lexing.lexeme lexbuf) }
  (* Separate minus case because can be unop or binop *)
  | minus { MINUS (Lexing.lexeme lexbuf) }
  | unop { UNOP (Lexing.lexeme lexbuf) }
  | binopSingle { BINOP (Lexing.lexeme lexbuf) }

  | eof { EOF }