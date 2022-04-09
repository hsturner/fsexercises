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