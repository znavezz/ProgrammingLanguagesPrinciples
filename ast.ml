type var = string;;

type aexp = Num of int 
 | Var of var 
 | Add of aexp * aexp 
 | Mult of aexp * aexp 
 | Sub of aexp * aexp;;

type bexp = True 
 | False 
 | Aeq of aexp * aexp
 | Beq of bexp * bexp
 | Gte of aexp * aexp
 | Neg of bexp
 | And of bexp * bexp;;

type stm = Ass of var * aexp 
 | Skip 
 | Comp of stm * stm 
 | If of bexp * stm * stm 
 | While of bexp * stm;;

type state = var -> int ;;

(* test case*) 
let test0 = Ass ("x", Num 5);;
let test1 = Skip;;
let test2 = Comp (Ass ("x", Num 3), Ass ("x", Add(Var "x", Num 1)));; 
let test3 = If(Neg(Aeq(Var "x", Num 1)),Ass ("x", Num 3),Ass ("x", Num 7));;
let test4 = Comp (Ass("y", Num 1), While(Neg(Aeq(Var "x", Num 0)),Comp(Ass("y", Mult(Var "y", Var "x")),Ass("x", Sub(Var "x", Num 1)))));; 

