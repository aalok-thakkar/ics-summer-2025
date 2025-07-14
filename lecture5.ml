exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]


(* Lecture 5: Fixed Size Binary Numbers *)

(* In the last week we saw division and exp *)

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

(* So now we have a calculator: 

  1. Addition
  2. Multiplication
  3. Subtraction
  4. Division
  5. Exponentiation 
*)




(* What about logarithms? *)


(* function: 
      
      log_base_2 (n : nat) : nat = ...
      .*)


(* 2 ^ (log_base_2 n) = n


  Tests: 

      1. log_base_2 1 = 0
      2. log_base_2 2 = 1
      3. log_base_2 3 = 1
      4. log_base_2 4 = 2
      5. log_base_2 5 = 2
      6. log_base_2 6 = 2
      7. log_base_2 7 = 2
      8. log_base_2 8 = 3

      Requires: 
      n <> Zero 

      Ensures: 
      2^(log_base_2 n) <= n
      2^((log_base_2 n) + 1) > n

*)







(* Let us start with the Blueprint: 

      Who Builds a Skyscraper Without Drawing Blueprints?
      https://www.youtube.com/watch?v=iCRqE59VXT0

      Leslie Lamport (2014)










Function Contract: 
    Requires:
    Ensures:
*)



(* Requires: n <> Zero 

  Ensures: output such that 2^(output) <= n < 2^(output + 1)
*)


(* Operations: *)

(*

  1. If n is one, then return zero
  2. Else, say m = div n two. 
  3. Compute (log_base_2 m). 
          Then: 2^(log_base_2 m) <= m < 2^((log_base_2 m) + 1).
          That is: 2^((log_base_2 m) + 1) <= n < 2^((log_base_2 m) + 2)
  4. Return Succ (log_base_2 m)
*)

let one : nat = Succ Zero
let two : nat = Succ one

let rec log_base_2 (n: nat) : nat option = 
  match n with 
  | Zero -> None
  | one -> Some Zero
  | _ -> 
    match (log_base_2 (div n two)) with
    | None -> raise NotImplementedError
    | Some k -> Some (Succ k)

(* Proof: *)

(* Exercise. 

    Induction on n. 

    For n = Zero, it doesn't satisfy the requires clause
    For n = Succ Zero, that is the base case. 

    For n > 1, ... 

    IH: If n' <> Zero, then 2^(log_base_2 n') <= n' < 2^((log_base_2 n') + 1)

    Induction Step: ...

*)













(* But no one defines natural numbers when they want to use them. *)






(* Fixed Size Binary Numbers *)


type bit = Zero | One
type half_byte = {
  b3: bit;  (* Most significant bit *)
  b2: bit;
  b1: bit;
  b0: bit;  (* Least significant bit *)
}

let add_hbyte (hb1: half_byte) (hb2: half_byte) : half_byte = todo "Implement" hbyte_zero

(* Blueprint with requires and ensures clauses. *)

(* Requires: (hb_to_int a) + (hb_to_int b) = hb_to_int (add_hbyte a b). *)

(* Operations, OCaml, and Proof *)






(* Space-efficient, fast hardware-level operations, predictable memory layout *)





(* Limited Range (only up to 2^n), and overflow is silent and dangerous unless 
explicitly checked. *) 





(* Case Study IX: The Y2K Bug (2000)

In the 20th century, many systems stored years using only two digits 
(e.g., "99"). As the year rolled over to "00", systems misinterpreted it as 
1900 instead of 2000. This caused widespread concern about banking, airline 
systems, etc.

Looking ahead, in the year 2038 we can expect another problem. Many UNIX-like 
systems represent time as a 32-bit signed integer. This overflows on Jan 19, 
2038, at 03:14:07 UTC. After that, the integer "wraps around" to a negative 
number, corrupting time data. This is the 32-bit equivalent of the Y2K bug.
*) 


(* Case Study X: Bitcoin's Value Overflow Incident (2010)

A bug in Bitcoin's transaction validation allowed an output value to overflow.
A transaction created ~184 billion BTC out of thin air — far above the intended
cap. Root Cause: Integer overflow due to unchecked addition of 64-bit integers. 
Fixed by a hard fork that reversed the transaction and patched the validation 
logic.

*)



