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
  make_test "Return call" "{'return': {'call': function, 'args': s}}" "return function(s)";
  make_test "Return call with binop" "{'return': {'call': function, 'args': {'binop': *, 'argl': 2, 'argr': 3}}}" "return function(2*3)";
  
  make_test "Call function" "{'call': function, 'args': 3}" "function(3)";
  make_test "Call function with unop" "{'call': function, 'args': {'unop': !, 'arg': a}}" "function(!a)";
  make_test "Call function with binop" "{'call': function, 'args': {'binop': /, 'argl': 1, 'argr': 2}}" "function(1/2)";
  
  make_test "Set" "{'set': x, 'value': 1}" "x=1";
  make_test "Set" "{'set': x, 'value': 1}" "x=1";
  
  (* Other tests *)
  make_test "Remove spaces spaces" "{'set': x, 'value': 1}" " x   =            1 ";


  (* make_test "While" "{'while': {'binop': <, 'argl': x, 'argr': 3}, 'do': {'binop': +, 'argl': x, 'argr': 1}}" "while x < 3 do x + 1"; *)

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
