exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Song: Gangnam Style *)




(* Lecture 2: Natural Numbers and the Axiom of Induction *)

(* 
    Fun Fact: Insects have ears in unusual places--crickets on legs, 
    grasshoppers on abdomens, moths on thoraxes, and mantises with a single 
    ear on their chest to dodge bats!
*)


















(* In the last class we say native type int *)

let x : int = 5
let y : int = 2
let z : int = -3
let sum = x + y
let product = x * y
let quotient = x / y
let remainder = x mod y

(* Integer division truncates towards zero *)
let div1 = 5 / 2     (* 2 *)
let div2 = (-5) / 2  (* -2, not -3 *)

(* Remainder (modulus) behaves accordingly *)
let rem1 = 5 mod 2    (* 1 *)
let rem2 = (-5) mod 2 (* -1, result sign matches dividend *)








(* What if we want fractions? *)









(* Floats are written with decimal points or exponents *)
let pi = 3.14159
let temp = -2.5
let three = 3.

let sum = 0.1 +. 0.2
let product = 0.1 *. 0.2


let a = float_of_int 5 

let b = int_of_float 3.7
let c = int_of_float (-3.7)

let d = 3 + 0.1









(* Can the native types be wrong? *)

let add_int (x: int) (y: int) : int = x + y 

(* What is right? What is wrong? *)

(*  az kufr-o-ze-islam barun sahrayist
    ma ra ba-miyan-e-aan faza saudayist  *)









(* 
    Let Z be the set of all integers. And let 
    (+): Z * Z -> Z be the standard addition function. 
    
    Then the function add_int should implement it. 
    
    That is, if x : int and y : int, let [[x]] and [[y]] denote the 
      corresponding mathematical values in Z. 

    Then add_int is correct if for all x : int and y : int, we require

    [[add_int x y]] = [[x]] + [[y]]
*)

let m : int = 4611686018427387903
















(* 64-bit signed number *)

let upper_bound = max_int
let lower_bound = min_int

(*
    In 2014, Psy's Gangnam style vidoes view counter got stuck at 2,147,483,647
    because thats the maximum value a 32 bit integer can store. 
*)










(* 
    Case Study III: Ariane flight V88, 1996
    
    Ariane flight V88 carried the Cluster spacecraft, a constellation of four 
    European Space Agency research satellites. Their code had inadequate 
    protection against integer overflow. This caused the rocket to veer off 
    and finally self-destruct. Caused a loss of US$370 million.
*)










(* How do you protect against integer overflow? *)



let safe_add_int (x : int) (y : int) : int =
  if (x > 0) && (y > 0) && (x > max_int - y) then
    todo "Positive Overflow" 0
  else if (x < 0) && (y < 0) && (x < min_int - y) then
    todo "Negative Overflow" 0
  else
    x + y









(* 
    This is like astrophysics. Weird things happen at large values. 
    
    It is also like quantum physics. Weird things also happen at small values! 
*)


(* Floating point arithmetic in interactive mode. *)















(* 
    Case Study IV: Patriot Missile Failure, 1991
    
    During the Gulf War, a Patriot missile battery in Dhahran, Saudi Arabia, 
    failed to intercept an incoming SCUD missile, resulting in the deaths and 
    injuries of 128 U.S. soldiers. The root cause was a floating point error 
    in the system's time calculation: converting 100 hours into tenths of a 
    second introduced a small but critical error due to the limitations of 
    floating point representation, causing the missile tracking software to 
    miscalculate the SCUD's position.
*)






(* 
    There is a lot of such bad code. And LLMs are trained on it! So they are 
    bound to make errors. 
*)















(* If I were to implement numbers in OCaml, I wouldn't make such mistakes! *)



(* But what is a number? What is a natural number? *)





type nat = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine








(* Peano Axioms *)


(* 1. 0 is a natural number *)
(*    
    That is at least:
      type nat = Zero
*)

(* 2. For every natural number n, Succ n is a natural number *)
(*  How do we capture this? *)



(* 3. 0 is not the successor of any natural number 
      There is no n such that Succ n = Zero *)




(* 4. Different natural numbers have different successors
      For all m, n: if Succ m = Succ n then m = n *)



(* 5. Axiom of induction: 
       If a property P holds for 0, that is P(0)
       and if it holds for n then it holds for Succ n, 
            that is P(n) -> P(Succ n)
       then it holds for all natural numbers *)


type nat = Zero | Succ of nat

let a : nat = Succ (Succ (Succ (Succ (Succ (Zero)))))
let b : nat = Succ (Succ (Succ (Succ (Zero))))
       


(* How do we define addition? *)



let plusZero (n : nat) : nat = n

let plusOne (n : nat) : nat = Succ n

let plusTwo (n : nat) : nat = Succ (Succ n)



let plusThree (n : nat) : nat = Succ (Succ (Succ n))


let plusThree (n : nat) : nat = Succ (plusTwo n)




let plus_succk (n : nat) : nat = Succ (plus_k n) 


       
let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 m)


(* Is this addition necessarily correct? *)




(* How do you multiply? *)