{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let number = digit+
let letter = ['a'-'z' 'A'-'Z']
let string = letter+
let minus = ['-']
let unop = ['-' '!' '~']
(* let binop = ["+" "-" "*" "/" "^" "%" "&" "|" ">>" "<<" "<" "<=" ">" ">=" "==" "~=" "and" "or"] *)
let binopSingle = ['+' '-' '*' '/' '^' '%' '&' '|' '<' '>']
(* let binopDouble = [">>" "<<" "<=" ">=" "==" "~=" "and" "or"] *)

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
  (* Separate multi-character binops because above rule is sintax error *)
  | ">>" {BINOP (Lexing.lexeme lexbuf)}
  | "<<" {BINOP (Lexing.lexeme lexbuf)}
  | "<=" {BINOP (Lexing.lexeme lexbuf)}
  | ">=" {BINOP (Lexing.lexeme lexbuf)}
  | "==" {BINOP (Lexing.lexeme lexbuf)}
  | "~=" {BINOP (Lexing.lexeme lexbuf)}
  | "and" {BINOP (Lexing.lexeme lexbuf)}
  | "or" {BINOP (Lexing.lexeme lexbuf)}

  | "=" { EQUALS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "return" { RETURN }

  | number { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (Lexing.lexeme lexbuf) }
  (* Separate minus case because can be unop or binop *)
  | minus { MINUS (Lexing.lexeme lexbuf) }
  | unop { UNOP (Lexing.lexeme lexbuf) }
  (* | binopDouble { BINOP (Lexing.lexeme lexbuf) } *)
  | binopSingle { BINOP (Lexing.lexeme lexbuf) }


  (* | "true" { TRUE }
  | "false" { FALSE }

  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "in" { IN } *)

  | eof { EOF }