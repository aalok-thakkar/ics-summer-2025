(* Lecture 14: More While Loops *)



(* Search for a target element in a list *)

(* Blueprint *)


(* 
  Requires: List l : a' list and target t : 'a
  Ensures: 
      If there exists i in [0, length(l) - 1] st l[i] = t then return True
      Else for all i in [0, length(l) - 1], if l[i] <> t return False.
*)

(* 

Termination: (l[i] = t)
Loop Invariant: (for all j before i, l[j] <> t)


let search1 (l : 'a list) (t : 'a) : bool = 
  let i = ref 0 in 
  let list_ended = ref false in
  while (l[i] <> t) do 
    if (!i = length(l) - 1): 
      list_ended := true
    else:
       i := !i + 1 
  done;
  list_ended <> true
;;


let search2 (l : 'a list) (t : 'a) : bool = 
  let i = ref 0 in 
  while ((!i <> length(l)) && (l[i] <> t)) do 
       i := !i + 1 
  done;
  (!i < length(l))
;;

let search3 (l: 'a list) (t: 'a) : bool = 
  let i = ref 0 in
  let have_i_found_it = ref false in 
  while (i < length(l)) do
    have_i_found_it := (l[i] = t)
  done;
  !have_i_found it
;;




  If my i before executing loop body is i, what can I say about i?

  1. l[i] <> t
  2. (for all j before i, l[j] <> t)
  3. Update i to (i + 1) so that we have: 
        l[i] <> t
    and (for all j before i, l[j] <> t)
    ->  (for all j before (i + 1), l[j] <> t)

*)






















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








(* Is Prime? *)


(* 
  Requires: n > 1
  Ensures: 
      For all d in [2, ... , (n - 1)] d does not divide n -> is_prime n = true
      There exists d in [2, ... , (n - 1)] that divides n -> is_prime n = true

*)












let is_prime (n: int) : bool =
  if n < 2 then false
  else
    let d = ref 2 in
    let found_divisor = ref false in
    while (!d <= (n - 1)) do
      if n mod !d = 0 then found_divisor := true;
      i := !i + 1
    done;
    not !has_divisor
;;








(* Try to count the number of steps in Collatz Conjecture *)

let rec collatz (n: int) : int =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1)
;;

let rec collatz_helper (a: int) (n: int) : int =
  if n = 1 then a
  else if n mod 2 = 0 then collatz (a + 1) (n / 2)
  else collatz (a + 1) (3 * n + 1)
;;

(* While Loop? 

let collatz (n : int) : int = 
  let a = ref 0 in 
  let m = ref n in 
  while (!m <> 1) do
    a := !a + 1; 
    if m mod 2 = 0 then 
      m := m / 2;
    else
      m := (3 * !m + 1)
  done;
  !a

*)








let collatz_loop (n: int) : int =
  let num = ref n in
  let steps = ref 0 in
  while !num <> 1 do
    (* Invariant: 
        !steps = number of collatz steps required to get from n to !num *)
    assert(!steps = collatz(n) - collatz(!num)); 

    if !num mod 2 = 0 then
      num := !num / 2
    else
      num := 3 * !num + 1;
    steps := !steps + 1
  done;

  (* From loop condition: !num = 1 *)
  (* From loop invariant: !steps = number of collatz steps required to get from n to !num *)

  (* Therefore, !steps = number of collatz steps required to get from n to 1. *)

  assert(!steps = collatz(n)); 

  !steps
;;






(* Nested Loops *)


let print_grid (n: int) (m: int) =
  for i = 1 to n do
    for j = i to 2*i do
      Printf.printf "(%d,%d) " i j
    done;
    print_newline ()
  done
;;


(* Example usage *)
(* print_grid 3 4;; *)









(* Sorting *)



(* What is the Blueprint? *)
















let rec insert x = function
  | [] -> [x]
  | h :: t as l -> if x <= h then x :: l else h :: insert x t

let rec insertion_sort = function
  | [] -> []
  | h :: t -> insert h (insertion_sort t)



(* How to make it tail recursive? *)















(* Arrays are fixed-size, mutable sequences of elements of the same type.  
   Unlike lists, arrays allow O(1) random access but are not persistent (updates modify the original array). *)


(* Method 1: Array literal *)
let arr1 = [| 1; 2; 3; 4 |] ;;  (* [|1; 2; 3; 4|] *)

(* Method 2: Array.make (size, initial_value) *)
let arr2 = Array.make 3 0 ;;     (* [|0; 0; 0|] *)

(* Method 3: Array.init (size, generator_function) *)
let arr3 = Array.init 5 (fun i -> i * 2) ;; (* [|0; 2; 4; 6; 8|] *)


(* Get element at index i: arr.(i) *)
let x = arr1.(2) ;;  (* x = 3 *)

(* Set element at index i: arr.(i) <- new_value *)
arr1.(1) <- 99 ;;    (* arr1 is now [|1; 99; 3; 4|] *)


(* Length of an array *)
let len = Array.length arr1 ;;  (* len = 4 *)

(* Copy an array (shallow copy) *)
let arr_copy = Array.copy arr1 ;;

(* Concatenate two arrays *)
let combined = Array.append [|1; 2|] [|3; 4|] ;; (* [|1; 2; 3; 4|] *)

(* Iterate over an array *)
Array.iter (fun x -> print_int x) arr1 ;; (* Prints all elements *)

(* Map over an array (creates a new array) *)
let doubled = Array.map (fun x -> x * 2) arr1 ;;

(* Fold over an array (left-to-right) *)
let sum = Array.fold_left ( + ) 0 arr1 ;;


(* Arrays of arrays (jagged arrays) *)
let matrix = [| [|1; 2|]; [|3; 4|] |] ;;
let elem = matrix.(1).(0) ;;  (* elem = 3 *)

(* True 2D arrays (using Array.make_matrix) *)
let grid = Array.make_matrix 2 2 0 ;; (* 2x2 matrix filled with 0s *)
grid.(0).(1) <- 5 ;;  (* Modifies the matrix *)


(* Arrays are mutable (unlike lists) *)
let a = [|1; 2; 3|] ;;
a.(1) <- 5 ;;  (* a is now [|1; 5; 3|] *)

(* If you need immutability, use lists instead! *)


(* - Fast random access (O(1))
   - Updates are in-place (O(1))
   - Fixed size (use Array.append/resize for dynamic growth)
   - Less memory-efficient than lists for sequential access *)


(* - Out-of-bounds access raises Invalid_argument *)
try arr1.(10) with Invalid_argument _ -> -1 ;;

(* - Arrays are mutable; copying may be needed *)
let a = [|1; 2|] ;;
let b = a ;;        (* b and a share the same memory! *)
b.(0) <- 9 ;;       (* Now a.(0) is also 9 *)
