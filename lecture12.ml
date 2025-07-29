(* Lecture 12: The While Loop *)



let factorial_helper (a : int) (n : int) (i : int): int = 
  match (i > n) with 
  | true -> a
  | false -> factorial_helper (a * i) (n) (i + 1) 
;;

let factorial (n: int) : int =
  let a = ref 1 in
  for i = 1 to n do
    a := !a * i
  done;
  !a
;;


(* How do you prove the correctness of a program with a for loop? By defining properties on the state of the program. *)

(* A loop invariant is a property (or condition) of the state of the program that:

1. Holds before the loop starts.
2. Remains true after every iteration of the loop.
3. When the loop ends, it helps prove that the program produces the correct result. *)

let factorial (n: int) : int =
  let a = ref 1 in
  (* Loop Invariant: At the start of each iteration !a = factorial(i - 1) *)
  for i = 1 to n do
    (* At the start of iteration i:
       Invariant holds: !a = (i - 1)!
       We are about to compute i-th factorial by multiplying !a by i.
    *)
    a := !a * i;

    (* After this step:
       Invariant holds: !a = factorial(i)
    *)
  done;

  (* When the loop terminates:
     i = n + 1
     Therefore, !a = factorial(n)
  *)
  !a
;;



(* How would you do GCD? *)

let rec gcd (a: int) (b: int) : int =
  if b = 0 then a
  else gcd b (a mod b)
;;


(* 

We have (a > 0) and (b > 0). 

Start from min a b, and go all the way down to 1 and check at each step. 

  1. Let k = min a b
  2. If k | a and k | b, then we are done. 
  3. Else k = k - 1 and repeat. 


*)


let rec gcd (a: int) (b: int) : int = 
  match (a = 0, b = 0) with 
  | (true, true) -> failwith "gcd: both inputs are zero"
  | (true, _) -> b
  | (_, true) -> a 
  | - -> 
    match (a > b) with
    | true -> gcd b (a - b)
    | false -> gcd a (b - a)




(* You do not know how many times to repeat the operation,
   because the number of recursive calls depends on the size of the inputs
   and how quickly they reduce through the modulo operation.
   
   For this, we will use the while loop! *)

let rec while_loop (condition: bool) (body: unit) =
  if condition then (
    body;
    while_loop condition body
  )
;;


let gcd (a : int) (b : int) : int = 
  match (a = 0, b = 0) with 
  | (true, true) -> failwith "gcd: both inputs are zero"
  |  _ -> 
    let x = ref a in 
    let y = ref b in 
    (* Invariant: (gcd !x !y) = (gcd a b).  *)
    while (!x <> 0 && !y <> 0) do
      if (!x > !y) then 
      x := !y ;
      y := !x - !y; 
      else 
        x := !y - !x;
        y := !x ; 
    done
    !x + !y
;;



(* Back to division *)


type nat = Zero | Succ of nat 

(* Define a function div such that it takes two natural numbers a and b, and returns the quotient and the remainder when a is divided by b. *)


(* Requires: b <> Zero *)
(* Ensures:

      Let's say div a b = (q, r)

      (i) a = (b * q) + r
      (ii) r < b


*)


















let minus ( n : nat ) ( m : nat ) : nat =
  let n_ref = ref n in
  let m_ref = ref m in
  
  while !m_ref <> Zero do
    if !n_ref = Zero then failwith " minus : function requires n >= m "
  
    else 
      match !n_ref , !m_ref with
      | Succ n' , Succ m' -> n_ref := n'; m_ref := m'
      | _ -> assert false
    done ;
    !n_ref
;;
7 else
  
let div ( a : nat ) ( b : nat ) : ( int * int ) =
  match b with
  | Zero -> failwith " Division by zero "
  | _ ->
    let q = ref Zero in
    let r = ref a in 
    while not ( less_than !r b ) do
      r := minus !r b ;
      q := Succ !q
    done ;
    
    (!q , !r)
;;



(* The Integer Square Root Question *)


(* Given a number n > 0, find an a such that: 

  a^2 <= n < (a + 1)^2  
  
*)


















let find_integer_sqrt (n : int) : int =
  if n <= 0 then invalid_arg "n must be > 0";
  let a = ref 0 in
  while (!a + 1) * (!a + 1) <= n do
    a := !a + 1
  done;
  !a
;;


(* Find the maximum element in a list *)

















let max (l : int list) : int option =
  let max_val = ref None in
  let rest = ref l in
  while !rest <> [] do
    match !rest with
    | x :: t ->
        begin
          match !max_val with
          | None -> max_val := Some x
          | Some current_max -> if x > current_max then max_val := Some x
        end;
        rest := t
    | [] -> ()
  done;
  !max_val
;;

(* Search for a target element in a list *)













let search_list (l : 'a list) (target: 'a) : bool =
  let found = ref false in
  let rest = ref l in
  while !rest <> [] && not !found do
    match !rest with
    | h :: t ->
        if h = target then found := true
        else rest := t
    | [] -> ()
  done;
  !found
;;