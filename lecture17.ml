(* Lecture 17: Arrays *)



(* 
  Requires: l : 'a list (where comparison is defined for 'a)

  Ensures: 
      For all i in [0, ... , len(l) - 1]
         l'[0] <= l'[1] <= l'[2] <= ... <= l'[len(l) - 1]

      l' is a permutation of l
*)


(* 

Let us say we have: 

1. Let l_seen = [] and l_unseen = l at the start
2. Invariant is l_seen remains sorted
3. While l_unseen is non-empty, take an element out of l_unseen
4. Put it in l_seen in sorted order. 
5. Once l_unseen is empty, I am done!
*)


let insert (elem : int) (l : int list) : int list = 
  let left = ref [] in
  let right = ref l in
  let is_inserted = false in 
  while ((!right <> []) && not (is_inserted)) do
    match !right with 
    | [] -> ()
    | h :: t -> 
      match (h > elem) with 
      | true -> 
        right := elem :: !right;
        is_inserted := true;
      | false -> 
        left := left @ [h];
        right := t;
  done
  left @ right 
;;



(* insert 3 [1; 7]
   left = []
   right = [1; 7]


   left = [1]
   right = [7]

   left = [1]
   right = [3; 7]


   return [1; 3; 7]
*)

let rec sort (l : int list) : int list = 
  let seen = ref [] in 
  let unseen = ref l in 
  while (unseen <> []) do 
    match unseen with 
    | [] -> ()
    | h :: t -> 
      unseen := t; 
      seen := insert h !seen;
  done 
  !seen
;;








let rec insert (x: int) (l : int list) =
  match l with 
  | [] -> [x]
  | h :: t as l -> 
    match (x <= h) with 
    | true ->  x :: l 
    | false -> h :: insert x t

let rec insertion_sort (l : int list) : int list = 
  match l with
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








(* Write selection sort and insertion sort for arrays *)







(* Selection sort using only while loops *)
let selection_sort (arr : int array) : int array =
  let len : int = Array.length arr in
  let sorted : int array = Array.copy arr in
  let i : int ref = ref 0 in
  while !i < len - 1 do
    let min_index : int ref = ref !i in
    let j : int ref = ref (!i + 1) in
    while !j < len do
      if sorted.(!j) < sorted.(!min_index) then
        min_index := !j;
      j := !j + 1
    done;
    let temp : int = sorted.(!i) in
    sorted.(!i) <- sorted.(!min_index);
    sorted.(!min_index) <- temp;
    i := !i + 1
  done;
  sorted

(* Insertion sort using only while loops *)
let insertion_sort (arr : int array) : int array =
  let len : int = Array.length arr in
  let sorted : int array = Array.copy arr in
  let i : int ref = ref 1 in
  while !i < len do
    let key : int = sorted.(!i) in
    let j : int ref = ref (!i - 1) in
    while !j >= 0 && sorted.(!j) > key do
      sorted.(!j + 1) <- sorted.(!j);
      j := !j - 1
    done;
    sorted.(!j + 1) <- key;
    i := !i + 1
  done;
  sorted