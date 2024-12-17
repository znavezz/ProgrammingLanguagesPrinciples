open Ast
open Semantics

(* nos function: interpreter for stm *)
let rec nos (stm, state) = match stm with
  | Ass (x, e) -> update x e state
  | Skip -> state
  | Comp (s1, s2) -> nos (s2, nos (s1, state))
  | If (b, s1, s2) -> if solve_b b state then nos (s1, state) else nos (s2, state)
  | While (b, s) -> 
      if solve_b b state then nos (While (b, s), nos (s, state)) 
      else state;;

(* Tests *)
let () =
  print_string "x = ";
  print_int (let new_state = nos (test1, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test2, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test3, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test4, s1) in new_state "x");
  print_endline "";

  print_string "y = ";
  print_int (let new_state = nos (test4, s1) in new_state "y");
  print_endline "";

  (* Additional tests for Shl and Shr *)
  let test_shl = Ass ("x", Shl (Num 3, Num 2)) in
  let test_shr = Ass ("x", Shr (Num 16, Num 3)) in

  print_string "x = ";
  print_int (let new_state = nos (test_shl, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shr, s0) in new_state "x");
  print_endline "";