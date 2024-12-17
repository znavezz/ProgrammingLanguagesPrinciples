open Ast
open Semantics

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
      else nos (Repeat (s, b), state');;

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

  print_string "x = ";
  print_int (let new_state = nos (test_shl_zero, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shr_zero, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shl_negative, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shr_negative, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shl_large, s0) in new_state "x");
  print_endline "";

  print_string "x = ";
  print_int (let new_state = nos (test_shr_large, s0) in new_state "x");
  print_endline "";

  (* Test Repeat 1: Increment x until x >= 5 *)
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


  (* Test Repeat 3: Decrement a and increment b until a = 0 *)
  print_endline "Test Repeat 3: Decrement a and increment b until a = 0";

  print_string "a = ";
  print_int (let new_state = nos (test_repeat3, s0) in new_state "a");
  print_endline "";

  print_string "b = ";
  print_int (let new_state = nos (test_repeat3, s0) in new_state "b");
  print_endline "";

 (* Bubble Sort Test *)
  print_endline "\n--- Bubble Sort Test for n = 5 ---";

  (* Define Variables and Values *)
  let vars = ["x1"; "x2"; "x3"; "x4"; "x5"] in
  let values = [3; 5; 1; -2; 100] in

  (* Initialize State *)
  let initial_state =
    List.fold_left2 (fun state var value -> update var (Num value) state)
      default_state vars values
  in

  (* Print Initial State *)
  print_endline "Initial State:";
  List.iter (fun var -> Printf.printf "%s = %d\n" var (initial_state var)) vars;

  (* Generate Bubble Sort Statement *)
  let rec generate_bubble_sort vars =
    let n = List.length vars in
    let rec outer_loop i acc =
      if i >= n then acc
      else
        let rec inner_loop j acc_inner =
          if j >= n - i - 1 then acc_inner
          else
            let var1 = List.nth vars j in
            let var2 = List.nth vars (j + 1) in
            let swap_condition = Gte (Var var1, Var var2) in
            let swap_statement =
              Comp (Ass ("tmp", Var var1),
                    Comp (Ass (var1, Var var2),
                          Ass (var2, Var "tmp")))
            in
            inner_loop (j + 1) (Comp (acc_inner, If (swap_condition, swap_statement, Skip)))
        in
        outer_loop (i + 1) (inner_loop 0 acc)
    in
    outer_loop 0 Skip
  in

  let bubble_sort_stm = generate_bubble_sort vars in

  (* Run Bubble Sort *)
  let final_state = nos (bubble_sort_stm, initial_state) in

  (* Print Final State *)
  print_endline "Sorted State:";
  List.iter (fun var -> Printf.printf "%s = %d\n" var (final_state var)) vars;
