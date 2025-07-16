exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]


(* Lecture 6: An Algebra of Lists *)

(* Fixed Size Binary Numbers *)

type bit = Zero | One
type half_byte = {
  b3: bit;  (* Most significant bit *)
  b2: bit;
  b1: bit;
  b0: bit;  (* Least significant bit *)
}

let add_hbyte (hb1: half_byte) (hb2: half_byte) : half_byte = todo "Implement" hbyte_zero






(* Requires: (hb_to_int a) + (hb_to_int b) < 16 *)
(* Ensures: hb_to_int (add_hbyte a b) = hb_to_int a + hb_to_int b *)



(* 1101 = 1*8 + 1*4 + 0*2 + 1*1   *)



let hb_to_int (hb : half_byte) : int =
  (match hb.b3 with One -> 8 | Zero -> 0) +
  (match hb.b2 with One -> 4 | Zero -> 0) +
  (match hb.b1 with One -> 2 | Zero -> 0) +
  (match hb.b0 with One -> 1 | Zero -> 0)


let b_to_int (b: bit) : int = 
  match b with 
  | Zero -> 0
  | One -> 1

let hb_to_int2 (hb : half_byte) : int = 
  match hb with 
  | {a; b; c; d} -> b_to_int(a) * 8 + b_to_int(b) * 4 + b_to_int(c) * 2 + b_to_int(d)


(* Requires: (hb_to_int a) + (hb_to_int b) < 16 *)
(* Ensures: hb_to_int (add_hbyte a b) = hb_to_int a + hb_to_int b *)

(* Design choice: How to deal with cases that violate requires clause? 
   - return mod 16
*)

(* Examples: 

   1011 + 0001 = 

     1 1
   1 0 1 1
  +0 0 0 1
  ---------
   1 1 0 0 
   
   
   *)


let add_bit (a : bit) (b : bit) : bit =
  match (a, b) with
  | (Zero, Zero) -> Zero
  | (Zero, One) -> One
  | (One, Zero) -> One
  | (One, One) -> Zero

(* What about carry? *)


(* returns (carry, sum) *)
let add_bit (a : bit) (b : bit) : (bit * bit) =
  match (a, b) with
  | (Zero, Zero) -> (Zero, Zero)
  | (Zero, One ) | (One, Zero) -> (One, Zero)
  | (One, One) -> (Zero, One)







(* returns (sum, carry) *)
let add_bit (a : bit) (b : bit) (carry : bit) : bit * bit =
  match (a, b, carry) with
  | (Zero, Zero, Zero) -> (Zero, Zero)
  | (Zero, Zero, One ) | (Zero, One, Zero) | (One, Zero, Zero) -> (One, Zero)
  | (One, One, Zero) | (One, Zero, One) | (Zero, One, One) -> (Zero, One)
  | (One, One, One) -> (One, One)



let add_hbyte (a : half_byte) (b : half_byte) : half_byte =
  let sum0, carry1 = add_bit a.b0 b.b0 Zero in
  let sum1, carry2 = add_bit a.b1 b.b1 carry1 in
  let sum2, carry3 = add_bit a.b2 b.b2 carry2 in
  let sum3, carry_out = add_bit a.b3 b.b3 carry3 in
  { sum3; sum2; sum1; sum0 }


(* add_hbyte 1010 0100
  = let sum0, carry1 = add_bit Zero Zero Zero in
  let sum1, carry2 = add_bit One Zero carry1 in
  let sum2, carry3 = add_bit Zero One carry2 in
  let sum3, carry_out = add_bit One Zero carry3 in
  { sum3; sum2; sum1; sum0 }

  ={ One; One; One; Zero }


  


*)



(* Variable and Scope *)

let x = 5           (* x is in global scope *)
let y = x + 1 in    (* x is in scope here *)
let z = y * 2 in    (* y is in scope here *)
x + y + z           (* x, y, z are in scope *)
(* y, z are now out of scope *)
(* x remains in scope *)


let x = 5           (* x is in global scope *)
let y = x + 1 in (   (* x is in scope here *)
let x = 6 in        (* y is in scope here *)
x + y)
(* What is the value of x + y *)


(* What is the value of x? *)




(* What is does this evaluate to? *)

let name = "Alice" in
let name = "Bob" in
"Hello, " ^ name







(* Return None in case of overflow*)



let safe_add_hbyte (a : half_byte) (b : half_byte) : half_byte option =
  let sum0, carry1 = add_bit a.b0 b.b0 Zero in
  let sum1, carry2 = add_bit a.b1 b.b1 carry1 in
  let sum2, carry3 = add_bit a.b2 b.b2 carry2 in
  let sum3, carry_out = add_bit a.b3 b.b3 carry3 in
  match carry3 with
  | Zero -> Some { b0 = sum0; b1 = sum1; b2 = sum2; b3 = sum3 }
  | One -> None












let safe_add_int (x: int) (y: int) : int = 
  match (x > 0, y > 0, x > max_int - y), (x < 0, y<0, x < min_int - y) with 
  | (true, true, true), _ -> failwith "Positive Overflow"
  | _, (true, true, true) -> failwith "Negative Overflow"
  | _ -> a + b




(* Halfbyte. 
    x = 1100 (12)
    y = 0101 (5)

    maxint = 15 = 1111
    maxint - y = 1111 - 0101 = 1010 (15 - 5 = 10)

    Is x > maxint - y?
    No! 
    Then x + y will be > maxint! That is, it will overflow. 


    If x > 0, y > 0, then (x > maxint - y) -> (x + y > maxint)
    


*)


(* Why is this not correct? *)
let safe_add_int (x: int) (y: int) : int = 
  match (x > max_int - y), (x < min_int - y) with 
  | (true), _ -> failwith "Positive Overflow"
  | _, (true) -> failwith "Negative Overflow"
  | _ -> a + b


(* This is correct *)
let safe_add_int (x : int) (y : int) : int =
  match (x > 0, y > 0, x > max_int - y), (x < 0, y < 0, x < min_int - y) with
  | (true, true, true), _ -> failwith "Positive Overflow"
  | _, (true, true, true) -> failwith "Negative Overflow"
  | _ -> x + y





(* Fixed Size Binary Numbers vs Binary Numbers *)


type bit = Zero | One

type half_byte = {
  b3: bit;
  b2: bit;
  b1: bit;
  b0: bit;
}

type bin = Nil | B of bit * bin











let empty_bin = Nil
let one_bit = B (One, Nil)
let two_bits = B (Zero, B (One, Nil))
let three_bits = B (One, B (Zero, B (One, Nil))) 
let four_bits = B (One, B (One, B (One, B (Zero, Nil))))











type bin = Nil | B of bit * bin

type 'a mylist = Nil | Cons of 'a * 'a mylist
(* A list is either Nil or Cons(head, tail).  *)
(* Here, 'a denotes an arbitary type. That is, the list will have elements of type 'a. *)
(* We can instantiate it with say bit, to get something like this: *)

let sample_mylist: bit mylist = Cons(Zero, Cons(One, Nil))
let mylist2: string mylist = Cons("1", Cons("2", Nil))





(* Observe that this parallels Peano axioms. *)

type nat = Zero | Succ of nat
type 'a mylist = Nil | Cons of ('a * 'a mylist)

(* One can map the Nil constructor to Zero and Cons to Succ, giving us the following function: *)

let rec foo (l: 'a mylist) : nat = 
  match l with
  | Nil -> Zero
  | Cons (h, tail) -> Succ (foo tail)

(* Let us run it on a simple example: 

      foo Cons(1, Cons(2, Cons(4, Nil)))
    = Succ (foo Cons(2, Cons(4, Nil)))
    = Succ (Succ(foo Cons(4, Nil)))
    = Succ (Succ( Succ (foo Nil)))
    = Succ (Succ( Succ (Zero)))
*)
(* Observe closely. The function foo computes the length of the list. *)






(* For ease of notation, we will use native lists instead of explicitely spelling out the type constructors Nil and Cons. That is, instead of sample_mylist, we will have lists of the format: *)

let empty_list : int list = []
let sample_list: int list = [1; 2]

(* Syntax: *)

[1 ; 2; 3] = 1 :: [2; 3] = 1 :: 2 :: [3] = 1 :: 2 :: 3 :: []



(* Convert a native list to our custom list *)
let rec of_list (l : 'a list) : 'a mylist =
  match l with
  | [] -> Nil
  | h :: tail -> Cons (h, of_list tail)     (* Form: head :: tail *)


(* Convert from our custom list to a native list *)
let rec to_list (l : 'a mylist): 'a list =
  match l with
  | Nil -> []
  | Cons (h, tail) -> h :: to_list tail



(* Find first element of a list *)

let first (l : 'a list): 'a option = 
  match l with 
  | [] -> None
  | h :: t -> Some h


(* first_nat is effectively isnt_zero *)
let first_nat (n : nat): bool = 
  match n with 
  | Zero -> false
  | Succ n' -> true
   





let first (l : 'a list): 'a option = 
  match l with 
  | [] -> None
  | h :: t -> h


let rec concat (l1: 'a list) (l2: 'a list) : 'a list = 
  match l1 with 
  | [] -> l2
  | h :: t -> h :: (concat t l2)



let rec plus (n1: int) (n2: int) : int = 
  match n1 with 
  | Zero -> n2
  | Succ n1' -> Succ (plus n1' n2)


(* The algebra that you have with nat, can be replicated on lists *)