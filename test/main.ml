open OUnit2
open Interp
open Main

let make_test n c s =
  [
    n >:: (fun _ -> assert_equal (c) ( parse_jsonLang s));
    n >:: (fun _ -> print_endline ("\n\n -----> Expected: \"" ^ c ^ "\" Got: \"" ^ parse_jsonLang s ^"\" \n\n"));
  ]

let tests = [
(* Break *)
  make_test "Break"
  "['break']"
  "break;";

(* Continue *)
  make_test "Continue"
  "['continue']" 
  "continue;";

(* Return *) 
  make_test "Return number" 
  "[{'return': 2}]" 
  "return 2;";
  make_test "Return string" 
  "[{'return': 'a'}]" 
  "return a;";
  make_test "Return unop" 
  "[{'return': {'unop': '-', 'arg': 2}}]" 
  "return -2;";
  make_test "Return binop" 
  "[{'return': {'binop': '+', 'argl': 1, 'argr': 2}}]" 
  "return 1+2;";
  make_test "Return call" 
  "[{'return': {'call': 'function', 'args': ['s']}}]" 
  "return function(s);";
  make_test "Return call with binop"
  "[{'return': {'call': 'function', 'args': [{'binop': '*', 'argl': 2, 'argr': 3}]}}]"
  "return function(2*3);";
  make_test "Return call with multiple args"
  "[{'return': {'call': 'function', 'args': [{'binop': '*', 'argl': 2, 'argr': 3}, 'x']}}]"
  "return function(2*3, x);";

(* Call *)  
  make_test "Call function" 
  "[{'call': 'someFunction', 'args': [0]}]" 
  "someFunction(0);";
  make_test "Call function with number" 
  "[{'call': 'function', 'args': [3]}]" 
  "function(3);";
  make_test "Call function with string" 
  "[{'call': 'function', 'args': ['j']}]" 
  "function(j);";
  make_test "Call function with unop" 
  "[{'call': 'function', 'args': [{'unop': '!', 'arg': 'a'}]}]" 
  "function(!a);";
  make_test "Call function with binop" 
  "[{'call': 'function', 'args': [{'binop': '/', 'argl': 1, 'argr': 2}]}]" 
  "function(1/2);";
  make_test "Call function with call" 
  "[{'call': 'function', 'args': [{'call': 'otherFunction', 'args': ['e']}]}]" 
  "function(otherFunction(e));";
  make_test "Call function with multiple paramaters" 
  "[{'call': 'someFunction', 'args': [0, 2]}]" 
  "someFunction(0,2);";
  make_test "Call function with multiple paramaters" 
  "[{'call': 'someFunction', 'args': [{'binop': '/', 'argl': 1, 'argr': 2}, 'a']}]" 
  "someFunction(1/2,a);";

(* Set *)  
  make_test "Set number" 
  "[{'set': 'x', 'value': 1}]" 
  "x=1;";
  make_test "Set string" 
  "[{'set': 'x', 'value': 'y'}]" 
  "x=y;";
  make_test "Set unop" 
  "[{'set': 'x', 'value': {'unop': '~', 'arg': 7}}]" 
  "x=~7;";
  make_test "Set binop" 
  "[{'set': 'x', 'value': {'binop': '-', 'argl': 6, 'argr': 8}}]" 
  "x = 6-8;";
  make_test "Set call" 
  "[{'set': 'x', 'value': {'call': 'function', 'args': [77]}}]" 
  "x=function(77);";

(* Declare *)  
  make_test "Declare number" 
  "[{'declare': 'x', 'value': 899}]" 
  "let x=899;";
  make_test "Declare string" 
  "[{'declare': 'x', 'value': 'something'}]" 
  "let x=something;";
  make_test "Declare unop" 
  "[{'declare': 'x', 'value': {'unop': '!', 'arg': 'value'}}]" 
  "let x=!value;";
  make_test "Declare binop" 
  "[{'declare': 'x', 'value': {'binop': '<', 'argl': 22, 'argr': 200}}]" 
  "let x = 22<200;";
  make_test "Declare call" 
  "[{'declare': 'x', 'value': {'call': 'function', 'args': ['argument']}}]" 
  "let x=function(argument);";

(* Do *)  
  make_test "Do with set and string" 
  "[{'do': {'set': 'x', 'value': 1}, 'until': 'somethingHappens'}]" 
  "do x=1; until somethingHappens;";
  make_test "Do with set and binop" 
  "[{'do': {'declare': 'x', 'value': {'binop': '^', 'argl': 'x', 'argr': 2}}, 'until': {'binop': '>=', 'argl': 'x', 'argr': 1000}}]" 
  "do let x=x^2; until x>=1000;";

(* If *)
  make_test "If with string and set" 
  "[{'if': [{'cond': 's', 'then': {'set': 'x', 'value': 1}}], 'else': {'set': 'x', 'value': 2}}]" 
  "if s then x=1; else x=2;;";
  make_test "If with binop, call and break" 
  "[{'if': [{'cond': {'binop': '==', 'argl': 'x', 'argr': 5}, 'then': 'break'}], 'else': {'set': 'x', 'value': {'binop': '+', 'argl': 'x', 'argr': 1}}}]"
  "if x==5 then break; else x=x+1;;";
  make_test "If with no else" 
  "[{'if': [{'cond': 's', 'then': {'set': 'x', 'value': 1}}]}]" 
    "if s then x=1;;";

(* Iterator *)
  make_test "Iterator" 
  "[{'iterator': 'i', 'from': 0, 'to': 10, 'step': 2, 'do': {'set': 'x', 'value': 1}}]"
  "for i from 0 to 10 step 2 do x=1;;";
  make_test "Iterator with no step" 
  "[{'iterator': 'i', 'from': 0, 'to': 10, 'do': {'set': 'x', 'value': 1}}]" 
  "for i from 0 to 10 do x=1;;";

(* While *)
  make_test "While" 
  "[{'while': 's', 'do': {'declare': 'x', 'value': 4}}]" 
  "while s do let x=4;;";
  make_test "While with binop" 
  "[{'while': {'binop': '~=', 'argl': 'x', 'argr': 5}, 'do': {'set': 'x', 'value': {'binop': '%', 'argl': 'x', 'argr': 2}}}]" 
  "while x ~= 5 do x=x%2;;";

(* Declare function *)  
  make_test "Declare function" 
  "[{'function': 'function', 'args': ['argument'], 'block': {'return': 'nothing'}}]" 
  "func function([argument]){return nothing;};";
  make_test "Declare function with multiple arguments" 
  "[{'function': 'function', 'args': ['arga', 'argb', 'argc'], 'block': {'return': 'nothing'}}]" 
  "func function([arga,argb,argc]){return nothing;};";

(* Block of statements *)  
  make_test "Block of statements" 
  "[[{'return': 2}, {'return': 3}]]" 
  "[return 2;, return 3;];";
  make_test "Block of statements with declare and set" 
  "[[{'declare': 'x', 'value': 2}, {'set': 'x', 'value': 3}]]" 
  "[let x=2;, x=3;];";
  make_test "Block of statements with if else"
    "[[{'if': [{'cond': {'binop': '==', 'argl': 'x', 'argr': 3}, 'then': {'return': 'x'}}]}, {'if': [{'cond': {'binop': '==', 'argl': 'x', 'argr': 4}, 'then': 'break'}], 'else': 'break'}]]"
    "[if x==3 then return x;;, if x==4 then break; else break;;];";
  
(* JsonLang *)
  make_test "JsonLang"
    "[{'if': [{'cond': {'binop': '==', 'argl': 'x', 'argr': 3}, 'then': {'return': 'x'}}]}, {'if': [{'cond': {'binop': '==', 'argl': 'x', 'argr': 4}, 'then': 'break'}], 'else': 'break'}]"
    "if x==3 then return x;; if x==4 then break; else break;;";
  make_test "JsonLang with declareFunction, declareVariable, if, set, call and return"
    "[{'function': 'function', 'args': ['argA', 'argB'], 'block': {'if': [{'cond': {'binop': '<=', 'argl': 'argA', 'argr': 3}, 'then': {'return': 'something'}}], 'else': {'return': 'nothing'}}}, {'declare': 'ret', 'value': {'call': 'function', 'args': [8, 3]}}, {'if': [{'cond': {'binop': '==', 'argl': 'ret', 'argr': 'nothing'}, 'then': {'call': 'exit', 'args': ['error']}}]}]"
    "func function([argA,argB]){if argA<=3 then return something; else return nothing;;}; let ret = function(8, 3); if ret == nothing then exit(error);;";

(* Other tests *)
  make_test "Remove spaces spaces" "[{'set': 'x', 'value': 1}]" " x   =            1 ;"; 

(* Extended tests *)
  (* Strings *)
  make_test "Strings" "[{'return': {'literal': 'a'}}]" "return 'a';"; 
  (* Arrays *)
  make_test "Array of Expressions"
    "[{'declare': 'x', 'value': {'array': [{'literal': 'a'}, {'literal': 'b'}]}}]" 
    "let x=['a', 'b'];";
  make_test "Array of mixed Expressions"
    "[{'declare': 'x', 'value': {'array': [{'unop': '~', 'arg': 7}, {'call': 'function', 'args': ['s']}, {'literal': 'b'}]}}]" 
    "let x=[~7, function(s), 'b'];";
  (* Disctionarys *)
  make_test "Dictionary" "[{'return': {'dictionary': [{'key': 'a', 'value': 1}]}}]" "return {a: 1};"; 
  make_test "Dictionary with multiple keys" "[{'return': {'dictionary': [{'key': 'a', 'value': 1}, {'key': 'b', 'value': {'literal': 'hello'}}]}}]" "return {a: 1, b: 'hello'};"; 
  (* First Class Citizen *)
  (* Note: 'someFunction' and 'otherFucntion' are considered to been declared previously (not included in test for simplicity)*)
  make_test "Function as parameter" 
    "[{'call': 'someFunction', 'args': [{'literal': 'hello'}, 'otherFunction']}]" 
    "someFunction('hello',otherFunction);";
  make_test "Assign function to variable" 
    "[{'declare': 'x', 'value': 'someFunction'}]" 
    "let x = someFunction;";
  make_test "Return function" 
    "[{'return': 'someFunction'}]" 
    "return someFunction;";
  (* Closures *)
  make_test "Declare function inside function" 
  "[{'function': 'outsideFunction', 'args': ['argA'], 'block': {'function': 'insideFunction', 'args': ['argB'], 'block': {'return': 'nothing'}}}]" 
  "func outsideFunction([argA]){func insideFunction([argB]){return nothing;};};";
]

let _ = run_test_tt_main ("suite" >::: List.flatten tests)
