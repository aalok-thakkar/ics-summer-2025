exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]


(* Lecture 4: An Algorithm of Pingala *)

(* 
    Fun Fact: In 2017, a nature paper showed that bees can quickly master an 
    insect version of football just by watching another bee handle the ball.
    
    The bumblebees watched a fellow bee tugging a ball into a goal, which 
    earned the athlete a gulp of sugar water. The observing bees could soon do 
    the task themselves.
*)


(* In the last class we saw division *)

type nat = Zero | Succ of nat



let rec plus (n: nat) (m: nat) : nat =
  match n with 
  | Zero -> m
  | Succ n' -> Succ (plus n' m)

let rec mult (n: nat) (m: nat) : nat =
  match n with 
  | Zero -> Zero
  | Succ n' -> plus (m (mult n' m))



let rec safe_minus_nat (n: nat) (m: nat): nat option =
  match (n, m) with 
  | (Zero, Zero) -> Some Zero
  | (Zero, _ ) -> None
  | (_, Zero) -> Some n
  | (Succ n', Succ m') -> safe_minus_nat n' m'





let rec divmod (num: nat) (den: nat) : (nat * nat) option = 
  match den with 
  | Zero -> None
  | _ -> 
    match num with 
    | Zero -> Some (Zero, Zero) 
    | _ -> 
      match (safe_minus_nat n m) with 
      | None -> Some (Zero, n)
      | Some diff -> 
        match divmod diff m with 
        | None -> None
        | Some (q', r') -> Some (Succ q', r')






(* Prove the correctness of division, that is prove that: 

divmod n m = Some (q, r) iff n = plus (mult m q) r
and divmod n m = None iff m = Zero. 

This requires the proof of correctness of safe_minus_nat, as well as induction on 
both n and m.



Blueprint 


We require that: 
    There exist k, such that n = plus m k
    
Then we can guarantee that: 
      n = plus m (safe_minus_nat n m)
  
Proof by Induction: 

  Start with induction on n. 

  Base Case: 
  n = Zero 
    if m = Zero, then k = Zero. 

      safe_minus_nat Zero Zero = Some Zero. (Line 37)

    if m = Succ m', then for sake of contradiction consider k such that 
      n = plus m k

    Then n = plus (Succ m') k 
           = Succ (plus m' k)      (definition of plus)

    That is Zero = Succ (plus m' k), which contradicts Peano's Axioms. 

    Therefore the requirement does not hold. 


  Inductive Hypothesis: For all n', 
      if there exist k, such that n' = plus m k
        then n' = plus m (safe_minus_nat n' m)

  Inductive Step: 

      To show: 
      if there exist k, such that (Succ n') = plus m k
      then Succ n' = plus m (safe_minus_nat (Succ n') m)

      If m = Zero, using function definition ... we are done. 

      Otherwise, m = Succ m'

      Then, as (Succ n') = plus (Succ m') k
                         = Succ (plus m' k)     (rules of plus)

      As Succ is injective (Peano's Axioms), we have: n' = plus m' k

      By Induction Hypothesis, n' = plus m (safe_minus_nat n' m)

      
      Observe that: 
      
      = plus m (safe_minus_nat (Succ n') m)
      = plus (Succ m') (safe_minus_nat (Succ n') (Succ m'))
      = plus (Succ m') (safe_minus_nat n' m')                 (Line 40)
      = Succ (plus m' (safe_minus_nat n' m'))            (Def. plus)
      = Succ n'                                           (Using IH)



*)





(*  Then prove:


    requires:
      m <> Zero

    guarantees:
      divmod n m = Some (q, r) such that:

       n = plus (mult m q) r



    Proof:

    By induction on n. 

    Base Case: n = Zero

    Then, if m <> Zero, 
      divmod n m = Some (Zero, Zero). 

      Zero = plus (mult (Zero Zero)) Zero        (simplify)



    Inductive Hyp: 
        
    For all n' such that there exists k such that n = plus n' k: 
     if divmod n' m = Some (q, r) then
       n' = plus (mult m q) r


    Inductive Step: 

    if divmod (Succ n') m = Some (q, r) then
       (Succ n') = plus (mult m q) r

    

    If safe_minus n m = None, then ...
        If safe_minus n m = Some diff, then consider (divmod diff m).
            It must be the case that (divmod diff m) = Some (q, r)          
            (In which case apply Induction Hypothesis)

*)


(* So now we have a calculator: 

  1. Addition
  2. Multiplication
  3. Subtraction
  4. Division
*)


(* Adding Exponentiation *)

let rec myexp (n: nat) (k: nat) : nat =          (* We mean n^k *)
  match k with 
  | Zero -> Succ Zero
  | Succ k' -> mult (myexp n k') n         (* n^k = n^(k-1) * n        *)










(* Case Study VII: Apache Solr (2013) 
In 2013, Apache Solr, a popular open-source search platform, faced a significant issue due to a missing base case in its query parsing logic. This bug led to a StackOverflowError when processing certain complex queries, causing the Solr server to crash or become unresponsive. While there are no publicly documented figures for this specific incident, it highlights how missing base cases in recursion can have significant consequences, even in widely used production systems.
*)






(* Case Study VIII: Java's Math.pow Inefficiency (Pre-Java 15)

In earlier versions of Java, the Math.pow function was known to be inefficient for integer exponentiation
due to its reliance on floating-point arithmetic and native method calls, even for simple cases like squaring a number. Benchmarks showed that using Math.pow(x, 2) could be up to 700 times slower than using x * x in tight loops, with times like 5 ms for multiplication versus over 3,000 ms for Math.pow under certain conditions. *)








(* Pingala, an ancient Indian scholar (circa 3rd century BCE), is credited with authoring the Chhandasutra, the earliest known treatise on Sanskrit prosody—the study of poetic meters. One of his key contributions is on enumerating patterns of prosodaic meters that anticipates the binary numeral system and recursive algorithms. In this context, he introduces the idea of exponentiation by squaring, a method to efficiently compute powers by reducing the number of multiplications. *)




let one : nat = Succ Zero
let two : nat = Succ (Succ Zero)

let rec pingala_exp (n: nat) (k: nat) : nat = 
  match k with 
  | Zero -> one
  | _ -> 
    match (divmod k two) with
    | None -> Zero                (* Impossible! Only if two = Zero *)
    | Some (k', r) -> 
      match r with 
      | Zero -> exp (mult n n) k'
      | one -> mult n (exp (mult n n) k')
      | _ -> Zero











(* Prove that for all n, k: nat

    exp n k = pingala_exp n k

    
*)


(* Square Roots *)

let sqrt (n: nat) : nat = todo "Implement Fast Exponentiation" Zero


(* Dealing with Floats *)















(* Rational Numbers *)

(* 
   A rational number is a number that can be expressed as the quotient or fraction 
   of two integers, where the numerator is an integer and the denominator is a non-zero integer.
   In this case, we will represent rational numbers as a pair of natural numbers *)


type rational = { num : nat; den : nat }
