open OUnit2
open Interp
(* open Ast *)
open Main

(* (** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  [n >:: (fun _ -> assert_equal (Int i) (interp_small s));
   n >:: (fun _ -> assert_equal (Int i) (interp_big s))]

(** [make_b n b s] makes an OUnit test named [n] that expects
    [s] to evalute to [Bool b]. *)
let make_b n b s =
  [n >:: (fun _ -> assert_equal (Bool b) (interp_small s));
   n >:: (fun _ -> assert_equal (Bool b) (interp_big s))]

(** [make_t n s] makes an OUnit test named [n] that expects
    [s] to fail type checking with error string [s']. *)
let make_t n s' s =
  [n >:: (fun _ -> assert_raises (Failure s') (fun () -> interp_small s));
   n >:: (fun _ -> assert_raises (Failure s') (fun () -> interp_big s))] *)

(** [make_test n c s] makes an OUnit test named [n] that expects
  [s] to evalute to [c]. *)

let make_test n c s =
  (* [n >:: (fun _ -> assert_equal (c) ( parse s))] *)
  [
    n >:: (fun _ -> assert_equal (c) ( parse_jsonLang s));
    n >:: (fun _ -> print_endline ("\n\n -----> Expected: \"" ^ c ^ "\" Got: \"" ^ parse_jsonLang s ^"\" \n\n"));
  ]

let tests = [
  make_test "Break" "'break'" "break";
  
  make_test "Continue" "'continue'" "continue";

  make_test "Return number" "{'return': 2}" "return 2";
  make_test "Return string" "{'return': a}" "return a";
  make_test "Return unop" "{'return': {'unop': -, 'arg': 2}}" "return -2";
  make_test "Return binop" "{'return': {'binop': +, 'argl': 1, 'argr': 2}}" "return 1+2";
  make_test "Return call" "{'return': {'call': function, 'args': [s]}}" "return function(s)";
  make_test "Return call with binop" "{'return': {'call': function, 'args': [{'binop': *, 'argl': 2, 'argr': 3}]}}" "return function(2*3)";
  make_test "Return call with multiple args" "{'return': {'call': function, 'args': [{'binop': *, 'argl': 2, 'argr': 3}, x]}}" "return function(2*3, x)";
  
  make_test "Call function" "{'call': someFunction, 'args': [0]}" "someFunction(0)";
  make_test "Call function with number" "{'call': function, 'args': [3]}" "function(3)";
  make_test "Call function with string" "{'call': function, 'args': [j]}" "function(j)";
  make_test "Call function with unop" "{'call': function, 'args': [{'unop': !, 'arg': a}]}" "function(!a)";
  make_test "Call function with binop" "{'call': function, 'args': [{'binop': /, 'argl': 1, 'argr': 2}]}" "function(1/2)";
  make_test "Call function with call" "{'call': function, 'args': [{'call': otherFunction, 'args': [e]}]}" "function(otherFunction(e))";
  make_test "Call function with multiple paramaters" "{'call': someFunction, 'args': [0, 2]}" "someFunction(0,2)";
  make_test "Call function with multiple paramaters" "{'call': someFunction, 'args': [{'binop': /, 'argl': 1, 'argr': 2}, a]}" "someFunction(1/2,a)";
  
  make_test "Set number" "{'set': x, 'value': 1}" "x=1";
  make_test "Set string" "{'set': x, 'value': y}" "x=y";
  make_test "Set unop" "{'set': x, 'value': {'unop': ~, 'arg': 7}}" "x=~7";
  make_test "Set binop" "{'set': x, 'value': {'binop': -, 'argl': 6, 'argr': 8}}" "x = 6-8";
  make_test "Set call" "{'set': x, 'value': {'call': function, 'args': [77]}}" "x=function(77)";
  
  make_test "Declare number" "{'declare': x, 'value': 899}" "let x=899";
  make_test "Declare string" "{'declare': x, 'value': something}" "let x=something";
  make_test "Declare unop" "{'declare': x, 'value': {'unop': !, 'arg': value}}" "let x=!value";
  make_test "Declare binop" "{'declare': x, 'value': {'binop': <, 'argl': 22, 'argr': 200}}" "let x = 22<200";
  make_test "Declare call" "{'declare': x, 'value': {'call': function, 'args': [argument]}}" "let x=function(argument)";
  
  make_test "Do with set and string" "{'do': {'set': x, 'value': 1}, 'until': somethingHappens}" "do x=1 until somethingHappens";
  make_test "Do with set and binop" "{'do': {'declare': x, 'value': {'binop': ^, 'argl': x, 'argr': 2}}, 'until': {'binop': >=, 'argl': x, 'argr': 1000}}" "do let x=x^2 until x>=1000";
  
  make_test "If with string and set" "{'if': [{'cond': s, 'then': {'set': x, 'value': 1}}], 'else': {'set': x, 'value': 2}}" "if s then x=1 else x=2";
  make_test "If with binop, call and break" "{'if': [{'cond': {'binop': ==, 'argl': x, 'argr': 5}, 'then': 'break'}], 'else': {'set': x, 'value': {'binop': +, 'argl': x, 'argr': 1}}}" "if x==5 then break else x=x+1";
  make_test "If with no else" "{'if': [{'cond': s, 'then': {'set': x, 'value': 1}}]}" "if s then x=1";

  make_test "Iterator" "{'iterator': i, 'from': 0, 'to': 10, 'step': 2, 'do': {'set': x, 'value': 1}}" "for i from 0 to 10 step 2 do x=1";
  make_test "Iterator with no step" "{'iterator': i, 'from': 0, 'to': 10, 'do': {'set': x, 'value': 1}}" "for i from 0 to 10 do x=1";

  make_test "While" "{'while': s, 'do': {'declare': x, 'value': 4}}" "while s do let x=4";
  make_test "While with binop" "{'while': {'binop': ~=, 'argl': x, 'argr': 5}, 'do': {'set': x, 'value': {'binop': %, 'argl': x, 'argr': 2}}}" "while x ~= 5 do x=x%2";
  
  make_test "Declare statement" "{'function': function, 'args': [argument], 'block': {'return': nothing}}" "func function([argument]){return nothing}";
  make_test "Declare statement with multiple arguments" "{'function': function, 'args': [arga,argb,argc], 'block': {'return': nothing}}" "func function([arga,argb,argc]){return nothing}";

  (* Other tests *)
  make_test "Remove spaces spaces" "{'set': x, 'value': 1}" " x   =            1 ";

  (* make_test "return" (Return(Number 2)) "return 2"; *)
  (* make_test "return" (Return(Binop("+", Number 2, Number 2))) "return 2+2";
  make_test "return" (Return(Unop("-", Number 2))) "return 2+2"; *)
  (* make_test "if true" (If (Bool true, Int 22, Int 0)) "if true then 22 else 0"; *)
  (* make_i "add" 22 "11+11";
  make_i "adds" 22 "(10+1)+(5+6)";
  make_i "let" 22 "let x=22 in x";
  make_i "lets" 22 "let x = 0 in let x = 22 in x";
  make_i "mul1" 22 "2*11";
  make_i "mul2" 22 "2+2*10";
  make_i "mul3" 14 "2*2+10";
  make_i "mul4" 40 "2*2*10";
  make_i "if1" 22 "if true then 22 else 0";
  make_b "true" true "true";
  make_b "leq" true "1<=1";
  make_i "if2" 22 "if 1+2 <= 3+4 then 22 else 0";
  make_i "if3" 22 "if 1+2 <= 3*4 then let x = 22 in x else 0";
  make_i "letif" 22 "let x = 1+2 <= 3*4 in if x then 22 else 0";
  make_t "ty plus" bop_err "1 + true";
  make_t "ty mult" bop_err "1 * false";
  make_t "ty leq" bop_err "true <= 1";
  make_t "if guard" if_guard_err "if 1 then 2 else 3";
  make_t "if branch" if_branch_err "if true then 2 else false";
  make_t "unbound" unbound_var_err "x"; *)
]

let _ = run_test_tt_main ("suite" >::: List.flatten tests)
