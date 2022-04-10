open System;;

type expr = Num of int | Plus of expr*expr | Times of expr*expr | Minus of expr*expr | Neg of expr;;

let rec eval = function
   | Num(n) -> n
   | Plus(a,b) -> eval(a) + eval(b)
   | Minus(a,b) -> eval(a) - eval(b)
   | Times(Num(0),a) | Times(a,Num(0)) -> 0  // ***
   | Times(a,b) -> eval(a) * eval(b)
   | Neg(a) -> eval(a) * -1;;

//EXERCISE C1: MODIFIED TOSTRING FUNCTION -- COMPLETE 
let rec tostring = function
   | Num(n) -> string(n)
   | Plus(a,Neg(b)) -> "(" + tostring(a) + " - " + tostring(b) + ")" //special case 1 
   | Plus(a,b) -> "(" + tostring(a) + " + " + tostring(b) + ")"  
   | Minus(a,Neg(b)) -> "(" + tostring(a) + " + " + tostring(Neg(Neg(b))) + ")"  //Special case 2
   | Minus(a,b) -> "(" + tostring(a) + " - " + tostring(b) + ")"   
   | Times(a,b) -> tostring(a) + "*" + tostring(b)
   | Neg(Neg(x)) -> tostring(x)     // ***
   | Neg(x) -> "-" + tostring(x);;


let t1 = Minus(Num(5),Neg(Num(2))); // 5 - -2
let t2 = Plus(Num(1),Neg(Neg(Neg(Num(1))))); // 1 + -1
let t3 = Plus(Num(1),Num(2))


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

