open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : statement list=
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let is_printable_e : expression -> bool = function
  | String _ | Number _ -> true
  | Unop _ | Binop _ | Call _  -> false

let rec is_printable_e_list : expression list -> bool = function
  | [] -> true
  | x :: sx ->
  match x with
  | String _ | Number _ -> is_printable_e_list sx
  | Unop _ | Binop _ | Call _  -> false

let rec is_printable_s_list : statement list -> bool = function
  | [] -> true
  | x :: sx ->
  match x with
  | String _ | Break | Continue -> is_printable_s_list sx
  | If _ | If_no_else _ | While _ | StatementBlock _ | Iterator _ | Iterator_no_step _ | Do _ | Declare _ | Set _ | Call _ | Return _ | DeclarationStatement _ -> false

let is_printable_s : statement -> bool = function
  | String _ | Break | Continue -> true
  | If _ | If_no_else _ | While _ | StatementBlock _ | Iterator _ | Iterator_no_step _ | Do _ | Declare _ | Set _ | Call _ | Return _ | DeclarationStatement _ -> false

let string_of_value_e (e : expression) : string =
  match e with
  | Number e -> string_of_int e
  | String e -> e
  | _ -> failwith "Cannot get string of expression"

(* let string_of_value_e_list (e : expression list) : string =
  match e with
  | [] -> ""
  | x :: sx ->
  match x with
  | String _ | Number _ -> string_of_int sx
  | Unop _ | Binop _ | Call _  -> false *)
  

let string_of_value_s (s : statement) : string =
  match s with
  | String e -> e
  | Break -> "'break'"
  | Continue -> "'continue'"
  | _ -> failwith "Cannot get string of statement"

let rec stepStatement (e : statement) : statement=
print_endline ("\n\n Step Statement \n\n");  
  match e with
  | String e -> String e
  | Break -> e
  | Continue -> e

  | If (e, s1, s2) when is_printable_e e && is_printable_s s1 && is_printable_s s2 -> 
      String ("{'if': [{'cond': " ^ string_of_value_e e ^ ", 'then': " ^ string_of_value_s s1 ^ "}], 'else': "^ string_of_value_s s2 ^ "}")
  | If (e, s1, s2) when is_printable_e e && is_printable_s s1 -> If(e, s1, stepStatement s2)
  | If (e, s1, s2) when is_printable_e e -> If(e, stepStatement s1, s2)
  | If (e, s1, s2) ->  If(stepExpression e, s1, s2)

  | If_no_else (e, s) when is_printable_e e && is_printable_s s -> 
      String ("{'if': [{'cond': " ^ string_of_value_e e ^ ", 'then': " ^ string_of_value_s s ^ "}]}")
  | If_no_else (e, s) when is_printable_e e -> If_no_else(e, stepStatement s)
  | If_no_else (e, s) ->  If_no_else(stepExpression e, s)
  
  | While (e, s) when is_printable_e e && is_printable_s s -> 
      String ("{'while': " ^ string_of_value_e e ^ ", 'do': " ^ string_of_value_s s ^ "}")
  | While (e, s) when is_printable_e e -> While(e, stepStatement s)
  | While (e, s) -> While(stepExpression e, s)
  
  | StatementBlock s when is_printable_s_list s ->
      String ("["^ translateStatementList s ^ "]")
  | StatementBlock s -> StatementBlock(resolveStatementList s)
  
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 && is_printable_e e2 && is_printable_e e3 && is_printable_s s -> 
      String("{'iterator': " ^ x ^ ", 'from': " ^ string_of_value_e e1 ^ ", 'to': " ^ string_of_value_e e2 ^ ", 'step': " ^ string_of_value_e e3 ^ ", 'do': " ^ string_of_value_s s ^"}")
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 && is_printable_e e2 && is_printable_e e3 -> Iterator(x, e1, e2, e3, stepStatement s)
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 && is_printable_e e2 -> Iterator(x, e1, e2, stepExpression e3, s)
  | Iterator (x, e1, e2, e3, s) when is_printable_e e1 -> Iterator(x, e1, stepExpression e2, e3, s)
  | Iterator (x, e1, e2, e3, s) -> Iterator(x, stepExpression e1, e2, e3, s)
  
  | Iterator_no_step (x, e1, e2, s) when is_printable_e e1 && is_printable_e e2 && is_printable_s s -> 
      String("{'iterator': " ^ x ^ ", 'from': " ^ string_of_value_e e1 ^ ", 'to': " ^ string_of_value_e e2 ^ ", 'do': " ^ string_of_value_s s ^"}")
  | Iterator_no_step (x, e1, e2, s) when is_printable_e e1 && is_printable_e e2 -> Iterator_no_step(x, e1, e2, stepStatement s)
  | Iterator_no_step (x, e1, e2, s) when is_printable_e e1 -> Iterator_no_step(x, e1, stepExpression e2, s)
  | Iterator_no_step (x, e1, e2, s) -> Iterator_no_step(x, stepExpression e1, e2, s)

  | Do (s, e) when is_printable_s s && is_printable_e e -> 
      String ("{'do': " ^ string_of_value_s s ^ ", 'until': " ^ string_of_value_e e ^ "}")
  | Do (s, e) when is_printable_s s -> Do(s, stepExpression e)
  | Do (s, e) -> Do(stepStatement s, e)

  | Declare (x, e) when is_printable_e e -> 
      String ("{'declare': " ^ x ^ ", 'value': " ^ string_of_value_e e ^ "}")
  | Declare (x, e) -> Declare (x, stepExpression e)

  | Set (x, e) when is_printable_e e -> 
      String ("{'set': " ^ x ^ ", 'value': " ^ string_of_value_e e ^ "}")
  | Set (x, e) -> Set (x, stepExpression e)
  
  (* | Call (x, e) -> 
      String ("{'call': " ^ x ^ ", 'args': [" ^ translateExpressionList (resolveExpressionList e) ^ "]}") *)

  | Call (x, e) when is_printable_e_list e -> 
      String ("{'call': " ^ x ^ ", 'args': [" ^ translateExpressionList e ^ "]}")
  | Call (x, e) -> Call (x, resolveExpressionList e)
  
  | Return (e) when is_printable_e e ->
      String("{'return': " ^ string_of_value_e e ^ "}")
  | Return (e) -> Return(stepExpression e)

  | DeclarationStatement (x1, x2, s) when is_printable_s s -> 
      String ("{'function': " ^ x1 ^ ", 'args': " ^ x2 ^ ", 'block': " ^ string_of_value_s s ^ "}")
  | DeclarationStatement (x1, x2, s) -> DeclarationStatement (x1, x2, stepStatement s)
and stepExpression (e: expression) : expression=
  print_endline ("\n\n Step expression \n\n");
  match e with
  | Number e -> Number e
  | String e -> String e
  
  | Unop (s, e) when is_printable_e e -> 
      String ("{'unop': " ^ s ^ ", 'arg': " ^ string_of_value_e e ^ "}")
  | Unop (s, e) -> Unop (s, stepExpression e)
  
  | Binop (s, e1, e2) when is_printable_e e1 && is_printable_e e2 -> 
      String ("{'binop': " ^ s ^ ", 'argl': " ^ string_of_value_e e1 ^ ", 'argr': " ^ string_of_value_e e2 ^ "}")
  | Binop (s, e1, e2) when is_printable_e e1 -> Binop (s, e1, stepExpression e2)
  | Binop (s, e1, e2) -> Binop (s, stepExpression e1, e2)

    (* | Call of string * expression array *)
  (* | Call (x, e) when is_printable_e e -> 
      String ("{'call': " ^ x ^ ", 'args': [" ^ string_of_value_e e ^ "]}")
  | Call (x, e) -> Call(x, stepExpression e) *)
  | Call (x, e) when is_printable_e_list e -> 
    String ("{'call': " ^ x ^ ", 'args': [" ^ translateExpressionList e ^ "]}")
  | Call (x, e) -> Call (x, resolveExpressionList e)

and resolveExpressionList (e : expression list) : expression list =
  (* match e with
  | [] -> []
  | h :: t -> stepExpression h :: resolveExpressionList t *)
  print_endline ("\n\n Resolve Expression List \n\n");
  List.map stepExpression e 

and translateExpressionList (e : expression list) : string =
print_endline ("\n\n Translate Expression List \n\n");
  let x = List.map string_of_value_e e in 
  String.concat ", " x

and resolveStatementList (s : statement list) : statement list =
  (* match e with
  | [] -> []
  | h :: t -> stepExpression h :: resolveExpressionList t *)
  print_endline ("\n\n Resolve Statement List \n\n");
  List.map stepStatement s 

and translateStatementList (s : statement list) : string =
print_endline ("\n\n Translate Statement List \n\n");
  let x = List.map string_of_value_s s in 
  String.concat ", " x

let rec evalStatement (s : statement) : statement = 
  if is_printable_s s then s else
    s |> stepStatement |> evalStatement

let rec evalExpression (e : expression) : expression =
  if is_printable_e e then e else
    e |> stepExpression |> evalExpression

(* let rec eval (s: statement) : statement = match s with
  | While (e, s) -> eval_while e s
  | _ -> failwith "Todo"

let evalExpr (e: expression) : expression = match e with
  | Unop (s, e) -> 
      String ("{'unop':" ^ s ^ ", 'arg':" ^ string_of_value_e e ^ "}")
  | _ -> failwith "Todo"

let rec eval_while e s =
  String("{'while': " ^ 
  string_of_value_e (evalExpr e) ^ ", do:" ^ 
  string_of_value_s s ^ "}") *)
  

let parse_jsonLang (s : string) : string =
  (* let e = parse s in
  to_jsonLang e *)
  let a = parse s in
  let b = List.map evalStatement a in
  let c = List.map string_of_value_s b in
  "[" ^ (String.concat ", " c) ^ "]";

  (* s |> parse |> evalStatement |> string_of_value_s *)

  (* let to_jsonLang : statement -> string = function
  | Return e -> "{return: " ^ e ^ "}"
  | _ -> failwith "NotImplemented to_JsonLang" *)


(*   
(** [typ] represents the type of an expression. *)
type typ =
  | TInt
  | TBool

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if binary operators and their
    operands do not have the correct types. *)
let bop_err = "Operator and operand type mismatch"

(** The error message produced if the [then] and [else] branches
    of an [if] do not have the same type. *)
let if_branch_err = "Branches of if must have same type"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

(** A [Context] is a mapping from variable names to
    types, aka a symbol table, aka a typing environment. *)
module type Context = sig

  (** [t] is the type of a context. *)
  type t

  (** [empty] is the empty context. *)
  val empty : t

  (** [lookup ctx x] gets the binding of [x] in [ctx]. 
      Raises: [Failure unbound_var_err] if [x] is
      not bound in [ctx]. *) 
  val lookup : t -> string -> typ

  (** [extend ctx x ty] is [ctx] extended with a binding
      of [x] to [ty]. *)
  val extend : t -> string -> typ -> t
end

(** The [Context] module implements the [Context] signature 
    with an association list. *)
module Context : Context = struct
  type t = (string * typ) list

  let empty = []

  let lookup ctx x =
    try List.assoc x ctx
    with Not_found -> failwith unbound_var_err

  let extend ctx x ty =
    (x, ty) :: ctx
end

open Context

(** [typeof ctx e] is the type of [e] in context [ctx]. 
    Raises: [Failure] if [e] is not well typed in [ctx]. *)
let rec typeof ctx = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> lookup ctx x
  | Let (x, e1, e2) -> typeof_let ctx x e1 e2
  | Binop (bop, e1, e2) -> typeof_bop ctx bop e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3

(** Helper function for [typeof]. *)
and typeof_let ctx x e1 e2 = 
  let t1 = typeof ctx e1 in
  let ctx' = extend ctx x t1 in
  typeof ctx' e2

(** Helper function for [typeof]. *)
and typeof_bop ctx bop e1 e2 =
  let t1, t2 = typeof ctx e1, typeof ctx e2 in
  match bop, t1, t2 with
  | Add, TInt, TInt 
  | Mult, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> failwith bop_err

(** Helper function for [typeof]. *)
and typeof_if ctx e1 e2 e3 =
  if typeof ctx e1 = TBool 
  then begin
    let t2 = typeof ctx e2 in
    if t2 = typeof ctx e3 then t2
    else failwith if_branch_err
  end
  else failwith if_guard_err

(** [typecheck e] checks whether [e] is well typed in
    the empty context. Raises: [Failure] if not. *)
let typecheck e =
  ignore (typeof empty e)

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

(** [subst e v x] is [e] with [v] substituted for [x], that
    is, [e{v/x}]. *)
let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | If (e1, e2, e3) -> 
    If (subst e1 v x, subst e2 v x, subst e3 v x)

(** [step] is the [-->] relation, that is, a single step of 
    evaluation. *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** [eval_small e] is the [e -->* v] relation.  That is,
    keep applying [step] until a value is produced.  *)
let rec eval_small (e : expr) : expr = 
  if is_value e then e
  else e |> step |> eval_small

(** [interp_small s] interprets [s] by parsing, type-checking,
    and evaluating it with the small-step model. *)
let interp_small (s : string) : expr =
  let e = parse s in
  typecheck e;
  eval_small e

(** [eval_big e] is the [e ==> v] relation. *)
let rec eval_big (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith unbound_var_err
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
  | If (e1, e2, e3) -> eval_if e1 e2 e3

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop e1 e2 = match bop, eval_big e1, eval_big e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 = match eval_big e1 with
  | Bool true -> eval_big e2
  | Bool false -> eval_big e3
  | _ -> failwith if_guard_err

(** [interp_big s] interprets [s] by parsing, type-checking,
    and evaluating it with the big-step model. *)
let interp_big (s : string) : expr =
  let e = parse s in
  typecheck e;
  eval_big e

 *)