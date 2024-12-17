open Ast (* This opens the Ast module so you don't need to prefix every type *)

(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with
    | Ast.Num m -> m
    | Var x -> s x
    | Add (e1, e2) -> solve_a e1 s + solve_a e2 s
    | Mult (e1, e2) -> solve_a e1 s * solve_a e2 s
    | Sub (e1, e2) -> solve_a e1 s - solve_a e2 s;;

let not x = 
    match x with
        | true -> false
        | false -> true;;


 (* solve_b: bexp -> state -> bool *) 
 let rec solve_b e s = match e with
    Ast.True -> true
    | False -> false
    | Neg e1 -> not (solve_b e1 s)
    | Beq (e1, e2) -> (solve_b e1 s == solve_b e2 s)
    | Aeq (e1, e2) -> (solve_a e1 s == solve_a e2 s)
    | Gte (e1, e2) -> (solve_a e1 s >= solve_a e2 s)
    | And (e1, e2) -> (solve_b e1 s && solve_b e2 s);;


(* state update : to get a new state *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *) 
 raise (NotFound "undefined variable");; 

 (* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 