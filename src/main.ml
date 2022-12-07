open Ast

let parse (s : string) : statement list=
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let is_printable_e : expression -> bool = function
   | Printable _ -> true
   | String _ | Number _ | Unop _ | Binop _ | Call _  -> false

let rec is_printable_e_list : expression list -> bool = function
  | [] -> true
  | x :: sx ->
  match x with
   | Printable _  -> is_printable_e_list sx
   | String _ | Number _ | Unop _ | Binop _ | Call _  -> false

let rec is_printable_s_list : statement list -> bool = function
  | [] -> true
  | x :: sx ->
  match x with
   | Printable _ -> is_printable_s_list sx
   | Break | Continue | If _ | If_no_else _ | While _ | StatementBlock _ | Iterator _ | Iterator_no_step _ | Do _ | Declare _ | Set _ | Call _ | Return _ | DeclarationStatement _ -> false

let is_printable_s : statement -> bool = function
   | Printable _-> true
   | Break | Continue | If _ | If_no_else _ | While _ | StatementBlock _ | Iterator _ | Iterator_no_step _ | Do _ | Declare _ | Set _ | Call _ | Return _ | DeclarationStatement _ -> false

let string_of_value_e (e : expression) : string =
  match e with
  | Printable e -> e
  | _ -> failwith "Cannot get string of expression"

let string_of_value_s (s : statement) : string =
  match s with
  | Printable e -> e
  | _ -> failwith "Cannot get string of statement"

let rec stepStatement (e : statement) : statement=
  match e with
  | Printable e -> Printable e

  | Break -> Printable "'break'" 

  | Continue -> Printable "'continue'" 

  | If (e, s1, s2) when is_printable_e e && is_printable_s s1 && is_printable_s s2 -> 
      Printable ("{'if': [{'cond': " ^ string_of_value_e e ^ ", 'then': " ^ string_of_value_s s1 ^ "}], 'else': "^ string_of_value_s s2 ^ "}")
  | If (e, s1, s2) when is_printable_e e && is_printable_s s1 -> If(e, s1, stepStatement s2)
  | If (e, s1, s2) when is_printable_e e -> If(e, stepStatement s1, s2)
  | If (e, s1, s2) ->  If(stepExpression e, s1, s2)

  | If_no_else (e, s) when is_printable_e e && is_printable_s s -> 
      Printable ("{'if': [{'cond': " ^ string_of_value_e e ^ ", 'then': " ^ string_of_value_s s ^ "}]}")
  | If_no_else (e, s) when is_printable_e e -> If_no_else(e, stepStatement s)
  | If_no_else (e, s) ->  If_no_else(stepExpression e, s)
  
  | While (e, s) when is_printable_e e && is_printable_s s -> 
      Printable ("{'while': " ^ string_of_value_e e ^ ", 'do': " ^ string_of_value_s s ^ "}")
  | While (e, s) when is_printable_e e -> While(e, stepStatement s)
  | While (e, s) -> While(stepExpression e, s)
  
  | StatementBlock s when is_printable_s_list s ->
      Printable ("["^ translateStatementList s ^ "]")
  | StatementBlock s -> StatementBlock(resolveStatementList s)
  
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 && is_printable_e e2 && is_printable_e e3 && is_printable_s s -> 
      Printable("{'iterator': '" ^ x ^ "', 'from': " ^ string_of_value_e e1 ^ ", 'to': " ^ string_of_value_e e2 ^ ", 'step': " ^ string_of_value_e e3 ^ ", 'do': " ^ string_of_value_s s ^"}")
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 && is_printable_e e2 && is_printable_e e3 -> Iterator(x, e1, e2, e3, stepStatement s)
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 && is_printable_e e2 -> Iterator(x, e1, e2, stepExpression e3, s)
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 -> Iterator(x, e1, stepExpression e2, e3, s)
  | Iterator (x, e1, e2, e3, s) -> Iterator(x, stepExpression e1, e2, e3, s)
  
  | Iterator_no_step (x, e1, e2, s) when is_printable_e e1 && is_printable_e e2 && is_printable_s s -> 
      Printable("{'iterator': '" ^ x ^ "', 'from': " ^ string_of_value_e e1 ^ ", 'to': " ^ string_of_value_e e2 ^ ", 'do': " ^ string_of_value_s s ^"}")
  | Iterator_no_step (x, e1, e2, s) when is_printable_e e1 && is_printable_e e2 -> Iterator_no_step(x, e1, e2, stepStatement s)
  | Iterator_no_step (x, e1, e2, s) when is_printable_e e1 -> Iterator_no_step(x, e1, stepExpression e2, s)
  | Iterator_no_step (x, e1, e2, s) -> Iterator_no_step(x, stepExpression e1, e2, s)

  | Do (s, e) when is_printable_s s && is_printable_e e -> 
      Printable ("{'do': " ^ string_of_value_s s ^ ", 'until': " ^ string_of_value_e e ^ "}")
  | Do (s, e) when is_printable_s s -> Do(s, stepExpression e)
  | Do (s, e) -> Do(stepStatement s, e)

  | Declare (x, e) when is_printable_e e -> 
      Printable ("{'declare': '" ^ x ^ "', 'value': " ^ string_of_value_e e ^ "}")
  | Declare (x, e) -> Declare (x, stepExpression e)

  | Set (x, e) when is_printable_e e -> 
      Printable ("{'set': '" ^ x ^ "', 'value': " ^ string_of_value_e e ^ "}")
  | Set (x, e) -> Set (x, stepExpression e)

  | Call (x, e) when is_printable_e_list e -> 
      Printable ("{'call': '" ^ x ^ "', 'args': [" ^ translateExpressionList e ^ "]}")
  | Call (x, e) -> Call (x, resolveExpressionList e)
  
  | Return (e) when is_printable_e e ->
      Printable("{'return': " ^ string_of_value_e e ^ "}")
  | Return (e) -> Return(stepExpression e)

  | DeclarationStatement (x1, x2, s) when is_printable_s s -> 
      Printable ("{'function': '" ^ x1 ^ "', 'args': " ^ translateArgumentsList x2 ^ ", 'block': " ^ string_of_value_s s ^ "}")
  | DeclarationStatement (x1, x2, s) -> DeclarationStatement (x1, x2, stepStatement s)

and stepExpression (e: expression) : expression=
  match e with
  | Printable e -> Printable e

  | Number e -> Printable (string_of_int e)

  | String e -> Printable ("\'" ^ e ^ "\'")
  
  | Unop (s, e) when is_printable_e e -> 
      Printable ("{'unop': '" ^ s ^ "', 'arg': " ^ string_of_value_e e ^ "}")
  | Unop (s, e) -> Unop (s, stepExpression e)
  
  | Binop (s, e1, e2) when is_printable_e e1 && is_printable_e e2 -> 
      Printable ("{'binop': '" ^ s ^ "', 'argl': " ^ string_of_value_e e1 ^ ", 'argr': " ^ string_of_value_e e2 ^ "}")
  | Binop (s, e1, e2) when is_printable_e e1 -> Binop (s, e1, stepExpression e2)
  | Binop (s, e1, e2) -> Binop (s, stepExpression e1, e2)

  | Call (x, e) when is_printable_e_list e -> 
      Printable ("{'call': '" ^ x ^ "', 'args': [" ^ translateExpressionList e ^ "]}")
  | Call (x, e) -> Call (x, resolveExpressionList e)

and resolveExpressionList (e : expression list) : expression list =
  List.map stepExpression e 

and translateExpressionList (e : expression list) : string =
  let x = List.map string_of_value_e e in 
  String.concat ", " x

and resolveStatementList (s : statement list) : statement list =
  List.map stepStatement s 

and translateStatementList (s : statement list) : string =
  let x = List.map string_of_value_s s in 
  String.concat ", " x

and translateArgumentsList (s : string) : string =
  (* Delete first character '[' *)
  let a = String.sub s 1 (String.length s - 1) in 
  (* Delete last character ']' *)
  let b = String.sub a 0 (String.length a -1) in 
  (* Split string into a list *)
  let c = String.split_on_char ',' b in 
  (* Remove white spaces *)
  let d = List.map (fun (x : string) -> String.trim x) c in 
  (* Add '' to every string *)
  let e = List.map (fun (y : string) -> "\'" ^ y ^ "\'") d in
  (* Transform list into string divided with , *)
  let f = String.concat ", " e in
  (* Add brackets *)
  "[" ^ f ^ "]"

let rec evalStatement (s : statement) : statement = 
  if is_printable_s s then s else
    s |> stepStatement |> evalStatement

let rec evalExpression (e : expression) : expression =
  if is_printable_e e then e else
    e |> stepExpression |> evalExpression

let parse_jsonLang (s : string) : string =
  let a = parse s in
  let b = List.map evalStatement a in
  let c = List.map string_of_value_s b in
  "[" ^ (String.concat ", " c) ^ "]";
