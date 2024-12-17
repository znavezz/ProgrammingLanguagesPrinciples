type var = string;;

type aexp = Num of int 
 | Var of var 
 | Add of aexp * aexp 
 | Mult of aexp * aexp 
 | Sub of aexp * aexp
 | Shl of aexp * aexp
 | Shr of aexp * aexp;;

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

(* Additional tests for Shl and Shr *)
let test_shl = Ass ("x", Shl (Num 3, Num 2));;        (* 3 << 2 = 12 *)
let test_shr = Ass ("x", Shr (Num 16, Num 3));;      (* 16 >> 3 = 2 *)
let test_shl_zero = Ass ("x", Shl (Num 5, Num 0));;  (* Shift left by 0 *)
let test_shr_zero = Ass ("x", Shr (Num 5, Num 0));;  (* Shift right by 0 *)
let test_shl_negative = Ass ("x", Shl (Num (-4), Num 2));; (* Negative number left shift *)
let test_shr_negative = Ass ("x", Shr (Num (-16), Num 2));; (* Negative number right shift *)
let test_shl_large = Ass ("x", Shl (Num 1, Num 30));; (* Large shift left *)
let test_shr_large = Ass ("x", Shr (Num 1073741824, Num 30));; (* Large shift right *)
