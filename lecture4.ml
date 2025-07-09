exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Song: Bezos I *)




(* Lecture 4: An Algorithm of Panini *)

(* 
    Fun Fact: In 2017, Bees quickly master an insect version of football — with a sweet reward at the end — just by watching another bee handle the ball, suggesting that the tiny pollinators are capable of sophisticated learning, says a study in Science1.

Bumblebees watched a fellow bee tugging a ball into a goal, which earned the athlete a gulp of sugar water. The observing bees could soon do the task themselves. They even figured out how to nab the reward with less effort. “They’re not just blindly copying. They’re doing something better,” says study co-author and behavioural ecologist Olli Loukola of Queen Mary University of London.
*)


(* In the last class we saw custom defined int *)

type nat = Zero | Succ of nat

let a : nat = Succ (Succ (Succ (Succ (Succ (Zero)))))
let b : nat = Succ (Succ (Succ (Succ (Zero))))
       

(* Define Addition: *)

let plusZero (n : nat) : nat = n
let plusOne (n : nat) : nat = Succ n
let plusTwo (n : nat) : nat = Succ (Succ n)
let plusThree (n : nat) : nat = Succ (Succ (Succ n))

let plusThree (n : nat) : nat = Succ (plusTwo n)
       
let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 m)

(* Is this addition necessarily correct? *)


(* Proof of correctness of addition using denotational semantics:

    We need to show that [[plus n m]] = [[n]] + [[m]] *)


let rec nat_to_int (n : nat) : int =
  match n with
  | Zero -> 0
  | Succ k -> 1 + nat_to_int k

(* We want to prove: nat_to_int (plus k n) = (nat_to_int k) + (nat_to_int n) *)

(* Proof by induction on k:

  Base case: k = Zero
    plus Zero n = n
    nat_to_int (plus Zero n) = nat_to_int n
    nat_to_int Zero + nat_to_int n = 0 + nat_to_int n = nat_to_int n

  Inductive step: Assume true for k = k2
    plus (Succ k2) n = Succ (plus k2 n)
    nat_to_int (plus (Succ k2) n) = nat_to_int (Succ (plus k2 n))
                        = 1 + nat_to_int (plus k2 n)
    By induction hypothesis:
     nat_to_int (plus k2 n) = nat_to_int k2 + nat_to_int n
    So,
     nat_to_int (plus (Succ k2) n) = 1 + (nat_to_int k2 + nat_to_int n)
                         = (1 + nat_to_int k2) + nat_to_int n
                         = nat_to_int (Succ k2) + nat_to_int n

  Therefore, by induction, the denotational semantics of plus matches integer 
  addition.
*)




(* Proof of correctness of addition using axiomatic semantics:

    To prove that addition is correct, we need to show that it satisfies the 
    following properties: 
    
    1. Zero is the identity element for addition.
    2. The successor of a natural number n is the result of adding one to n.
*)


(* How do we define subtraction? *)

(* 
    Subtraction is not defined for all natural numbers, as it can lead to 
    negative results. However, we can define subtraction in a way that it only 
    returns a natural number if the result is non-negative. We can define 
    subtraction recursively as follows:

    - For any natural number n, subtracting Zero gives n.
    - For any natural number n, subtracting Succ k gives minus k n if n is greater than Succ k, otherwise Zero.
*)

let rec minus (k : nat) (n : nat) : nat = todo "minus not implemented yet"

(* How do we define multiplication? *)

(* 
    Multiplication can be defined recursively as follows:
    - For any natural number n, multiplying by Zero gives Zero.
    - For any natural number n, multiplying by Succ k gives n + (n * k). *)
    
let rec mult (k : nat) (n : nat) : nat = todo "multiplication not implemented yet"

(* How do we define division? *)











(* 
    Division can be defined recursively as follows:
    - For any natural number n, dividing by Zero is undefined.
    - For any natural number n, dividing by Succ k gives the largest natural number m such that m * (Succ k) <= n.  
*)

let rec div (n : nat) (k : nat) : nat  = todo "division not implemented yet"


(* A First and Naive Implementation of Division *)
let rec div (n: nat) (m: nat): nat =
    match n with
    | Zero -> Zero
    | Succ n' -> Succ (div (minus n m) m)
;;













(* You can define new exception messages such as: *)
exception Custom_Division_by_zero ;;

let rec div_2 (n: nat) (m: nat): nat =
    match (n, m) with
    | (_ , Zero) -> raise Custom_Division_by_zero
    | (Zero, _) -> Zero
    | (Succ n', _) -> Succ (div_2 (minus n m) m)
;;

print_nat (div_2 (Succ Zero) Zero);;

(* However, this code is still wrong, for example consider: 

  div_2 two three = Zero 

  (this can be stated using assertions)

We want to differentiate between the following:
'minus two three = Zero' and 'minus three three = Zero' *)

(* Let us introduce comparison! *)
let rec less_than (n: nat) (m: nat): bool = 
    match (n, m) with 
    | (_, Zero) -> false
    | (Zero, _) -> true
    | (Succ n', Succ m') -> less_than n' m'

let rec safe_and_correct_div (n: nat) (m: nat): nat =
    match (n, m) with
    | (_, Zero) -> raise Custom_Division_by_zero
    | (Zero, _) -> Zero
    | _ when (less_than n m) -> Zero  (* Stop when n < m *)
    | _ -> Succ (safe_and_correct_div (minus n m) m)



(* But I don't like this. Let us clean it up *)

let rec clean_div (n: nat) (m: nat): nat =
  match (n, m) with
  | (_, Zero) -> raise Custom_Division_by_zero
  | (Zero, _) -> Zero
  | _ ->
      match (minus n m) with
      | Zero when not (n = m) -> Zero    (* not (n = m) is same as (n <> m) *)
      | diff -> Succ (clean_div diff m)  (* Only count full subtractions *)


(* Our code ends with an exception as soon as one is raised! The same will 
happen with nested functions. The world breaks with exceptions! *)


(* We instead want to support a way to handle exceptions well. For this, we 
will use option types! Option types in OCaml represent values that may or may 
not be present. They are defined as 

    'type 'a option = None | Some of 'a' where 

'a' is any type. This is useful for functions that may not return a valid 
result, such as division by zero.

*)

let safe_minus_int (n: int) (m: int): int option =
    if n >= m then Some (n - m) else None
;;

let safe_reciprocal (n: float): float option =
    if n = 0.0 then None else Some (1.0 /. n)
;;

let rec safe_minus (n: nat) (m: nat): nat option = 
    match (n, m) with 
    | (_, Zero) -> Some n
    | (Zero, _ ) -> None
    | (Succ n', Succ m') -> safe_minus n' m'
;;

let rec new_safe_div (n: nat) (m: nat) : nat option = 
    match (n, m) with 
    | (_, Zero) -> None
    | (Zero, _) -> Some Zero 
    | (Succ n', Succ m') ->
        match (safe_minus n m) with
        | None -> Some Zero
        | Some diff -> Some (Succ (new_safe_div (diff) m))
;;

(* What about the remainder? *)




(* But we need a quotient and a remainder. Let us first figure out how we can 
have a function that has two outputs. *)  

(* A tuple is a fixed-size collections of values. For example, a pair of values
written as (a, b), but tuples can have more elements, e.g., (a, b, c).

  Example: A pair of integers
    let p : int * int = (3, 4)

  Example: A triple of different types
    let t : int * string * bool = (42, "hello", true)

  Accessing tuple elements is usually done via pattern matching:
    let (x, y) = p in
    (* x = 3, y = 4 *) 

*)

let min_max (a : int) (b : int) : int * int =
  match (a < b) with
  | true -> (a, b)
  | false -> (b, a) 


let min_max (a : int) (b : int) : int * int =
  match (a, b) with
  | (x, y) when x < y -> (a, b)
  | _ -> (b, a)


(* Now we can define a function that returns both the quotient and the 
remainder of a division operation. *)


let rec divmod (n : nat) (m: nat) : (nat*nat) option =
    match (n, m) with
    | (_, Zero) -> None
    | (Zero, _) -> Some (Zero, Zero)
    | _ -> 
        match (safe_minus n m) with
        | None -> Some (Zero, n)
        | Some diff -> 
            match (divmod diff m) with
            | None -> None
            (* If safe_minus is correct, this case should never match! 
            Hence we can introduce a custom exception statement here. 
            This would be a good use of an exception. *)
            | Some (q, r) -> Some (Succ (q),r)
;;

(* Prove the correctness of division, that is prove that: *)
(* divmod n m = Some (q, r) if and only if n = plus (mult m q) r and 
  (m <> Zero)

This requires the proof of correctness of safe_minus, as well as induction on 
both n and m.

First prove that:
    
    if (n <= m),    safe_minus n m = Some r 
    else,           safe_minus n m = None

    Use Induction on n as well as m. Here is a sketch:

    Base Case:  n = Zero, then safe_minus Zero m = None  (by definition)
                m = Zero, then safe_minus n Zero = n     (by definition)

    Induction Hypothesis: Assume safe_minus n' m' is correct for all n' < n, 
    m' < m.
    
    Inductive Step: By definition, for n = Succ n' and m = Succ m', 
    
        safe_minus (Succ n') (Succ m') = safe_minus n' m'.
        
        By the inductive hypothesis, safe_minus n' m' is correct, so 
        safe_minus (Succ n') (Succ m') is also correct.
*)





(*  Then prove:

    divmod n m = Some (q,r) iff n = plus (mult m q) r and (m <> Zero)
    
    Here is a sketch for your reference. This is an incomplete proof. Attempt 
    to write it down. First let us get rid of the case of m = Zero. Then, 
    focus on m <> Zero.
    
    Proof by Induction on n:
    
    Base Case: n = Zero. This case can be completed by definition.
                    
            So for all m <> Zero, divmod Zero m = Some (Zero, Zero)

    Induction Hypothesis: 

            For all n' < n, 
                for all m <> Zero, divmod n' m = Some (q, r) iff 
                  n = plus (mult m q) r

    Induction Step: 

        If safe_minus n m = None, then ...
        If safe_minus n m = Some diff, then consider (divmod diff m).
            It must be the case that (divmod diff m) = Some (q, r)          
            (In which case apply Induction Hypothesis)
*)