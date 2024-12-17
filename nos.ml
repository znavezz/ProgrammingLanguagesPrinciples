open Ast
open Semantics
open Bubble_sort  (* Import the bubble sort logic *)

(* nos function: interpreter for stm *)
let rec nos (stm, state) = match stm with
  | Ass (x, e) -> update x e state
  | Skip -> state
  | Comp (s1, s2) -> nos (s2, nos (s1, state))
  | If (b, s1, s2) -> if solve_b b state = "tt" then nos (s1, state) else nos (s2, state)
  | While (b, s) -> 
      if solve_b b state = "tt" then nos (While (b, s), nos (s, state)) 
      else state
  | Repeat (s, b) -> 
      let state' = nos (s, state) in
      if solve_b b state' = "tt" then state'
      else nos (Repeat (s, b), state')

(* Combined Tests *)
let () =
  (* Existing Tests *)
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

  print_string "a = ";
  print_int (let new_state = nos (test5, s0) in new_state "a");
  print_endline "";

  print_string "b = ";
  print_int (let new_state = nos (test5, s0) in new_state "b");
  print_endline "";

  print_string "c = ";
  print_int (let new_state = nos (test5, s0) in new_state "c");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shl, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shr, s0) in new_state "x");
  print_endline "";

  (* Additional Tests for Bit-shifting and Repeat *)
  print_endline "Test Repeat 1: Increment x until x >= 5";
  print_string "x = ";
  print_int (let new_state = nos (test_repeat1, s0) in new_state "x");
  print_endline "";

  print_endline "Test Repeat 2: Increment x and double y until x >= 3";
  print_string "x = ";
  print_int (let new_state = nos (test_repeat2, update "y" (Num 1) s0) in new_state "x");
  print_endline "";
  print_string "y = ";
  print_int (let new_state = nos (test_repeat2, update "y" (Num 1) s0) in new_state "y");
  print_endline "";

  (* Bubble Sort Test for n = 5 *)
  let n = 5 in  (* Define n *)
  let vars = List.init n (fun i -> "x" ^ string_of_int (i + 1)) in
  let values = [3; 5; 1; -2; 100] in  (* Initial values for variables *)

  (* Initialize State *)
  let initial_state =
    List.fold_left2 (fun state var value -> update var (Num value) state)
      default_state vars values
  in

  (* Print Initial State *)
  print_endline "\n--- Bubble Sort Test for n = 5 ---";
  print_endline "Initial State:";
  List.iter (fun var -> Printf.printf "%s = %d\n" var (initial_state var)) vars;

  (* Generate Bubble Sort Statement *)
  let bubble_sort_stm = run_bubble_sort n in

  (* Run Bubble Sort *)
  let final_state = nos (bubble_sort_stm, initial_state) in

  (* Print Final State *)
  print_endline "Sorted State:";
  List.iter (fun var -> Printf.printf "%s = %d\n" var (final_state var)) vars;
