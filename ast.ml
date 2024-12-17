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
 | While of bexp * stm
 | Repeat of stm * bexp;;

type state = var -> int ;;

(* test case*) 
let test0 = Ass ("x", Num 5);;
let test1 = Skip;;
let test2 = Comp (Ass ("x", Num 3), Ass ("x", Add(Var "x", Num 1)));; 
let test3 = If(Neg(Aeq(Var "x", Num 1)),Ass ("x", Num 3),Ass ("x", Num 7));;
let test4 = Comp (Ass("y", Num 1), While(Neg(Aeq(Var "x", Num 0)),Comp(Ass("y", Mult(Var "y", Var "x")),Ass("x", Sub(Var "x", Num 1)))));; 

(* test5: Bit-shifting program *)
let test5 = 
    Comp(
        Ass("a", Num 84),
        Comp(
            Ass("b", Num 22),
            Comp(
                Ass("c", Num 0),
                While(
                    Neg(Aeq(Var "b", Num 0)),
                    Comp(
                        Ass("a", Shl (Var "a", Num 1)),
                        Ass("b", Shr (Var "b", Num 1))
                    )
                )
            )
        )
    );;

(* Additional tests for Shl and Shr *)
let test_shl = 
  Ass ("x", Shl (Num 3, Num 2));;
  (* 3 << 2 = 12 *)

let test_shr = 
  Ass ("x", Shr (Num 16, Num 3));;
  (* 16 >> 3 = 2 *)

let test_shl_zero = 
  Ass ("x", Shl (Num 5, Num 0));;
  (* Shift left by 0 *)

let test_shr_zero = 
  Ass ("x", Shr (Num 5, Num 0));;
  (* Shift right by 0 *)

let test_shl_negative = 
  Ass ("x", Shl (Num (-4), Num 2));;
  (* Negative number left shift *)

let test_shr_negative = 
  Ass ("x", Shr (Num (-16), Num 2));;
  (* Negative number right shift *)

let test_shl_large = 
  Ass ("x", Shl (Num 1, Num 30));;
  (* Large shift left *)

let test_shr_large = 
  Ass ("x", Shr (Num 1073741824, Num 30));;
  (* Large shift right *)


(* Repeat tests *)
let test_repeat1 = 
  Repeat(
    Ass("x", Add(Var "x", Num 1)),
    Gte(Var "x", Num 5)
  );;
  (* Starts with x = 0, increments x by 1 until x >= 5 *)

let test_repeat2 = 
  Repeat(
    Comp(
      Ass("y", Mult(Var "y", Num 2)),
      Ass("x", Add(Var "x", Num 1))
    ),
    Gte(Var "x", Num 4)
  );;
  (* Starts with x = 0, y = 1, increments x and doubles y until x >= 3 *)

let test_repeat3 = 
  Comp(
    Ass("a", Num 5),              (* Initialize a = 5 *)
    Comp(
      Ass("b", Num 0),            (* Initialize b = 0 *)
      Repeat(
        Comp(
          Ass("a", Sub(Var "a", Num 1)),  (* Decrement a by 1 *)
          Ass("b", Add(Var "b", Num 2))   (* Increment b by 2 *)
        ),
        Aeq(Var "a", Num 0)               (* Repeat until a = 0 *)
      )
    )
  );;

  (* Starts with a = 5, b = 0, decrements a and increments b by 2 until a = 0 *)

