  (** The type of binary operators. *)
  (* type bop = 
  | Add
  | Mult
  | Leq *)
  
  (** The type of the abstract syntax tree (AST). *)
  (* type expr =
  | Var of string
  | Int of int
  | Bool of bool  
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr *)
  
  (* type binops =
  | Add
  | Substract
  | Multiply
  | Divide
  | Elevate
  | Module
  | Ampersand
  | Pipe
  (* | ">>"
  | "<<" *)
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  | Equal
  (* | "~=" *)
  | And
  | Or *)
  
  type expression = 
  | Unop of string * expression
  | Binop of string * expression * expression
  | String of string
  (* | Call of string * expression array *)
  | Call of string * expression list
  | Number of int

  type statement = 
  | If of expression * statement * statement
  | If_no_else of expression * statement
  | While of expression * statement
  (* | Statement of statement array *)
  | StatementBlock of statement
  | Iterator of string * expression * expression * expression * statement
  | Iterator_no_step of string * expression * expression * statement
  | Do of statement * expression
  | Break
  | Continue
  | Declare of string * expression
  | Set of string * expression
  (* | Call of string * expression array *)
  | Call of string * expression list
  | Return of expression
  (* Add string to be able to print *)
  | String of string
  | DeclarationStatement of string * string * statement

  (* type declarationStatement =
  (* | DeclarationStatement of string * string array * statement *)
  | DeclarationStatement of string * string * statement *)

  type topStatement =
  | Statement of statement
  | DeclarationStatement of string * string * statement

  type jsonLang = 
  (* | TopStatement of topStatement array *)
  | TopStatement of topStatement

  (* type printable = 
  | Unop of string * expression
  | Binop of string * expression * expression
  | String of string
  (* | Call of string * expression array *)
  | Call of string * expression
  | Number of int
  | If of expression * statement * statement
  | While of expression * statement
  (* | Statement of statement array *)
  | Statement of statement
  | Iterator of string * expression * expression * expression * statement
  | Do of statement * expression
  | Break
  | Continue
  | Declare of string * expression
  | Set of string * expression
  | Return of expression
  
type customPrint = 
  | Expression of expression
  | Statement of statement *)