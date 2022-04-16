open System;;

type expr = Num of int | Plus of expr*expr | Times of expr*expr | Minus of expr*expr | Neg of expr | Andbool of expr * expr | Orbool of expr * expr | Notbool of expr | Equalbool of expr * expr | Ifelse of expr * expr * expr 



// these need to be added? 

//

// 

//ADDENDUM: EDITED EVAL
let mutable eval = fun (x:expr) -> 0

//don't know whats going on here. I'm changing the value of eval (because its mutable) but then specifying a return value of the result?

let beval = fun e -> 
  match e with 
   | Num(n) -> n
   | Plus(a,b) -> eval(a) + eval(b)
   | Times(Num(0),a) | Times(a,Num(0)) -> 0  // ***
   | Times(a,b) -> eval(a) * eval(b)
   | Neg(a) -> eval(a) * -1
   | _ -> 0 ;;



eval <- beval
  

let base_eval = eval


//modularly modifying eval expression


eval <- fun e -> 
  match e with
  | Minus(a,b) -> eval(a) - eval(b)
  | Andbool(Num(0),Num(0)) -> 0  //if both expressions are false, return true
  | Andbool(a,b) -> eval(Times(a,b)) //if both expressions are true, the result will be > 0, if one of the expressions is zero, the result will be zero
  | Orbool(Num(0),Num(0)) -> 0  //false or false = false
  | Orbool(a,b) -> eval(Plus(Times(a,a),Times(b,b)))// will return a value greater than 0 
  | Notbool(a) -> if eval(Andbool(a,Num(1))) <> 0 then 0 else 1 //true and false = false, false and false = true --> will always invert
  | Equalbool(a,b) -> eval(Notbool(Minus(a,b))) // if a-b = 0 (a==b), then 0 and 0 true, else false
//  | ifelse(Num(0),b,c) -> eval(c)
  | Ifelse(a,b,c) -> if eval(a)> 0 then eval(b) else eval(c)
  | _ -> base_eval e ;;
  





 





  //testing modified eval: 



//EXERCISE C1: MODIFIED TOSTRING FUNCTION -- COMPLETE 
let rec tostring = function
   | Num(n) -> string(n)
   | Plus(a,Neg(b)) -> "(" + tostring(a) + " - " + tostring(b) + ")" //special case 1 
   | Plus(a,b) -> "(" + tostring(a) + " + " + tostring(b) + ")"  
   | Minus(a,Neg(b)) -> "(" + tostring(a) + " + " + tostring(Neg(Neg(b))) + ")"  //Special case 2
   | Minus(a,b) -> "(" + tostring(a) + " - " + tostring(b) + ")"   
   | Times(a,b) -> tostring(a) + "*" + tostring(b)
   | Neg(Neg(x)) -> tostring(x)     // ***
   | Neg(x) -> "-" + tostring(x)
   | _ -> "Not a compatible operator";;


let t1 = Minus(Num(5),Neg(Num(2))); // 5 - -2
let t2 = Plus(Num(1),Neg(Neg(Neg(Num(1))))); // 1 + -1
let t3 = Plus(Num(1),Num(2))

let t8 = Times(Num(0),Times(Num(1),Neg(Num(2))))

let t5 = Orbool(Num(1),Neg(Num(1))) // 1 or -1 should be > 0 
let t6 = Notbool(Num(0))
let t7 = Notbool(Neg(Num(1)))


let t10 = Equalbool(Num(2),Num(2))
let t11 = Times(Num(3),Num(4))
let t12 = Andbool(Minus(Num(2),Num(2)),Num(0)) //And (0,0)

let t9 = Ifelse(Equalbool(Num(2),Num(2)),Num(1),Num(2))
printfn "This is the value I care about: %d" (eval t5)
printfn "DOES EQUAL WORK? EQUAL 2 2 %d"(eval t10)
printfn "This is not(0): %d" (eval t6)
printfn "This is not(-1): %d" (eval t7)
printfn "And (0,0): %d"(eval t12)
//printfn "This is negative mult %d" (eval t8)
printfn "This should be 1 %d" (eval t9)
printfn "Checking extension worked::::: %d"(eval t11)
printfn "%s" (tostring t1);; // prints (5- -2) now, but should print (5 + 2)
printfn "%s" (tostring t2);; // prints (1 + -1) now, but should print (1 - 1)
printfn "%s" (tostring t3);;



//EXERCISE C2: function to return the length of a stack

type 'a stack = Emp | Apnd of 'a stack * 'a;;
let s = Apnd(Apnd(Apnd(Emp,2),3),5);;

let pushon a stk = Apnd(stk,a);;
let popoff = function
  | Emp -> raise (System.Exception("stack underflow"))
  | Apnd(s,a) -> a;;

//tail recursive tolist
//2 curried functions --> first is ax, second is passed to the pattern matching clauses
let rec tolist2 ax = function
  | Emp -> ax    // ax is the accumulator, which starts as an empty list
  | Apnd(s,a) -> tolist2 (a::ax) s;;



//need to form a closure somehow out of this
let size (*(M:'a stack)*) M = 
  

  //don't like having to call size with an extra initial parameter to intialize the accumulator, using helper to hide it
  let rec inner M x =
    let mutable iax = x
    match M with
    | Emp -> iax
    | Apnd(s,a) -> iax<-iax+1; inner s iax //increase accumulator, pop top, call on remaining stack
  
  let result = inner M 0
  result;;



size s;;



//EXERCISE C3: convert list into stack without reversing the elements
let rec tostack (ax, (M: List<'T>))  = 
  match M with 
  | []  -> ax   // ax is an empty stack
  | head :: tail -> tostack (Apnd(ax,head), tail) ;;



let l1 = ["123","456"];;
let l2 = [50;100;150];;

let s1 = tostack(Emp,l2);;

