  type expression = 
  | Unop of string * expression
  | Binop of string * expression * expression
  | String of string
  | Call of string * expression list
  | Number of int
  | Literal of string
  | Array of expression list
  | Dictionary of expression list
  (* Add record to be able to handle dictionarys*)
  | Record of string * expression
  (* Add printable to be able to print jsonLang*)
  | Printable of string

  type statement = 
  | If of expression * statement * statement
  | If_no_else of expression * statement
  | While of expression * statement
  | StatementBlock of statement list
  | Iterator of string * expression * expression * expression * statement
  | Iterator_no_step of string * expression * expression * statement
  | Do of statement * expression
  | Break
  | Continue
  | Declare of string * expression
  | Set of string * expression
  | Call of string * expression list
  | Return of expression
  | DeclarationStatement of string * string * statement
  (* Add printable to be able to print jsonLang*)
  | Printable of string
 