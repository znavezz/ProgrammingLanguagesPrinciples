open Ast_sol
open Nos_sol
open Semantics_sol

(* Helper function to generate variable names x1, x2, ..., xn *)
let generate_vars n =
  List.init n (fun i -> "x" ^ string_of_int (i + 1))

(* Function to generate statements that initialize variables *)
let initialize_vars vars values =
  List.fold_left2
    (fun acc var value -> Comp (acc, Ass (var, Num value)))
    Skip vars values

(* Function to generate print statements for variables *)
let print_vars vars =
  List.fold_left
    (fun acc var -> Comp (acc, Ass ("_", Var var))) Skip vars

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
            Comp(Ass ("tmp", Var var1),
                 Comp(Ass (var1, Var var2),
                      Ass (var2, Var "tmp"))) in
          inner_loop (j + 1) (Comp (acc_inner, If (swap_condition, swap_statement, Skip)))
      in
      outer_loop (i + 1) (inner_loop 0 acc)
  in
  outer_loop 0 Skip
