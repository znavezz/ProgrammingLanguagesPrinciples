open Ast
open Semantics

(* Helper function to generate variable names x1, x2, ..., xn *)
let generate_vars n =
  List.init n (fun i -> "x" ^ string_of_int (i + 1))

(* Function to generate bubble sort logic *)
let generate_bubble_sort vars =
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

(* Main function to generate bubble sort statement for n variables *)
let run_bubble_sort n =
  let vars = generate_vars n in
  let bubble_sort_stm = generate_bubble_sort vars in
  bubble_sort_stm
