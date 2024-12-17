open Ast (* This opens the Ast module so you don't need to prefix every type *)

(* Helper function for integer exponentiation *)
let rec pow base exp =
  if exp = 0 then 1
  else base * pow base (exp - 1);;

(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with
    | Ast.Num m -> m
    | Var x -> s x
    | Add (e1, e2) -> solve_a e1 s + solve_a e2 s
    | Mult (e1, e2) -> solve_a e1 s * solve_a e2 s
    | Sub (e1, e2) -> solve_a e1 s - solve_a e2 s
    | Shl (e1, e2) -> solve_a e1 s * pow 2 (solve_a e2 s)
    | Shr (e1, e2) -> solve_a e1 s / pow 2 (solve_a e2 s);;


let not x = 
    match x with
        | true -> false
        | false -> true;;


(* solve_b: bexp -> state -> string 
   Evaluates a boolean expression and returns "tt" for true and "ff" for false. *) 
let rec solve_b e s = match e with
  | Aeq (e1, e2) -> if solve_a e1 s = solve_a e2 s then "tt" else "ff"
  | Beq (b1, b2) -> if solve_b b1 s = solve_b b2 s then "tt" else "ff"
  | Gte (e1, e2) -> if solve_a e1 s >= solve_a e2 s then "tt" else "ff"
  | Neg b1 -> if solve_b b1 s = "tt" then "ff" else "tt"
  | And (b1, b2) -> if solve_b b1 s = "tt" && solve_b b2 s = "tt" then "tt" else "ff"
  | _ -> failwith "Unsupported boolean expression";;



(* state update : to get a new state *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *) 
 raise (NotFound "undefined variable");; 

 (* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 