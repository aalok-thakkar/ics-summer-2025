exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Lecture 8: Lambda Function and Fold *)

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)

let double (x : int) = x * 2
(* map double [1 ; 2; 3] = [2; 4; 6] *)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | h :: t ->
    match pred h with
    | true -> h :: filter pred t
    | false -> filter pred t

let is_even (n : int) = (n mod 2 = 0)
(* filter is_even [2; 4; 5; 6; 10002] = [2; 4; 6; 10002] *)


let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b


let is_divisor (n: int) (x: int) : bool = (n mod x = 0)

let divisors (n: int) : int list =
  filter (is_divisor n) (range 1 n)

let is_prime (x: int) : bool =
  match divisors x with
  | [a; b] when a = 1 && b = x -> true
  | _ -> false

let primes (n: int) : int list = filter is_prime (range 1 n)


(* Lambda Functions *)


let add = fun x y -> x + y


let double = fun x -> x * 2

assert (map (fun x -> x * 2) [1; 2; 3] = map (double) [1; 2; 3])






let divisors (n: int) : int list =
  filter (fun x -> (n mod x = 0)) (range 1 n)

let is_prime (x: int) : bool = (divisors x = [1; x])

let primes (n: int) : int list = filter (is_prime) (range 1 n)




let rec sumlist (l: int list) : int =
  match l with
  | [] -> 0
  | h :: t -> h + sumlist t

let rec multlist (l: int list) : int =
  match l with
  | [] -> 1
  | h :: t -> h * multlist t

let rec all_true (l : bool list) : bool = 
  match l with 
  | [] -> true
  | h :: t -> h && (all_true t)

let rec length (l : 'a list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> 1 + length (t) 






let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)

let add (x: int) (y: int) : int = x + y

let sumlist l = foldr add 0 l

(* sumlist [1; 3; 6; 8; 10] 
 = foldr add 0 [1; 3; 6; 8; 10]
 = add 1 (foldr add 0 [3; 6; 8; 10])
 = add 1 (add 3 (foldr add 0 [6; 8; 10]))
 = add 1 (add 3 (add 6 (foldr add 0 [8; 10])))
 = add 1 (add 3 (add 6 (add 8 (foldr add 0 [10]))))
 = add 1 (add 3 (add 6 (add 8 (add 10 (foldr add 0 [])))))
 = add 1 (add 3 (add 6 (add 8 (add 10 (0)))))
 = add 1 (add 3 (add 6 (add 8 (10))))
 = add 1 (add 3 (add 6 18))
 = add 1 (add 3 24) 
 = add 1 27
 = 28  
 
 *)



let rec sumlist (l: int list) : int =
  match l with
  | [] -> 0
  | h :: t -> h + sumlist t

let sumlist l = foldr (fun (h: int) (fold_r_applied_on_tail: int) -> h + fold_r_applied_on_tail) 0 l 

let rec multlist (l: int list) : int =
  match l with
  | [] -> 1
  | h :: t -> h * multlist t

let multlist l = foldr (fun (x: int) (y: int) -> x * y) 1 l 

let rec all_true (l : bool list) : bool = 
  match l with 
  | [] -> true
  | h :: t -> h && (all_true t)

let all_true l = foldr (fun (x : bool) (y: bool) -> x && y) true l

let rec length (l : 'a list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> 1 + length (t) 

let length (l : 'a list) : int = foldr (fun x y -> 1 +  y) 0 l

(* length ["a", "bba", "er"]
=  foldr (fun x y -> 1 + y) 0 ["a", "bba", "er"]
=  (fun x y -> 1 + y) "a" (foldr (fun x y -> 1 + y) 0 ["bba", "er"])
=  1 + (foldr (fun x y -> 1 + y) 0 ["bba", "er"])
=  1 + (1 + (foldr (fun x y -> 1 + y) 0 ["er"]))
=  1 + (1 + ((fun x y -> 1 + y) "er" foldr (fun x y -> 1 + y) 0 [])))
=  1 + (1 + (1 + foldr (fun x y -> 1 + y) 0 [])))
=  1 + (1 + (1 + 0))
= 3
*)


(* This question will be in the viva: *)
let funkyfold (f : 'a -> 'b) (l : 'a list) : 'b list = 
  foldr (fun (x: 'a) (y : 'b list) -> f (x) :: y) l





let rec all_pairs_under (m: int) (p: int) : (int * int) list =
  match m <= 1 with
  | true -> []
  | false -> (map (fun x -> (m, x)) (range 1 p)) @ all_pairs_under (m - 1) p


let all_pairs_under (l : int list) : (int * int) list = 
  match l with 
  | [] -> ??
  | h :: t -> ??

let all_pairs_under (l : int list) : (int * int) list =
  foldr ?? ?? ??
  


let rec all_triples_under (n: int) (m: int) (p: int) : (int * int * int) list =
  match n <= 0 with
  | true -> []
  | false -> (map (fun (x, y) -> (n, x, y)) (all_pairs m p)) @ all_triples_under (n - 1) m p

let all_pythagorean_triples (n: int) : (int * int * int) list =
  filter (fun (x, y, z) -> x * x + y * y = z * z) (all_triples_under n n n)


let temp = foldr (fun (x: int) (y: int) -> x - y) 0 [4; -2; 1; 0]

(* What does the above expression evaluate to?

  1. (4 - (- 2 - (1 - (0 - 0))))
  2. (((4 - (-2)) - 1) - 0) - 0
*)

let rec factorial (n: int): int =
  match n with
  | 0 -> 1
  | _ when n < 0 -> failwith "usual"
  | n -> n * factorial_match (n - 1)

let factorial_fold (n: int): int = foldr ( * ) 1 (range 1 n)



let max_elem (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> Some (foldr max h t)


let concatenate (l1 : 'a list) (l2 : 'a list) = 
  foldr (fun (x : 'a) (s : 'a list) -> x :: s) l2 l1

(* 

let concatenate (l1 : 'a list) (l2 : 'a list) = 
  foldr (fun (x : 'a) (s : 'a list) -> x :: s) l2 l1

   concatenate [2; 3] [4; 32]
=  foldr (fun (x : 'a) (s : 'a list) -> x :: s) [4; 32] [2; 3] 
=  2 :: (foldr (fun (x : 'a) (s : 'a list) -> x :: s) [4; 32] [3]) 
=  2 :: 3 :: (foldr (fun (x : 'a) (s : 'a list) -> x :: s) [4; 32] []) 
=  2 :: 3 :: ([4; 32]) 
=  [2; 3; 4; 32]

*)


let flatten (l: ('a list) list) : 'a list = foldr concatenate [] l

(* flatten [[1;2]; [3;4]; [5]] = [1; 2; 3; 4; 5])






let rec fold_left (f: 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> fold_left f (f v x) xs

let rec zip (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let zipped_list = zip [1;2;3] ["a";"b";"c"]

let square: int -> int = fun x -> x * x
let l2 = map (fun (x: int) -> x * x) [2; 4; 6]

let double: int -> int = fun x -> x * 2
let doubled = map (fun x -> x * 2) [1;2;3]
let evens = filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6]

let count_occurrences elem l =
  foldr (fun x n -> match x = elem with true -> 1 + n | false -> n) 0 l

let count_twos = count_occurrences 2 [1;2;2;3;2;4;5]

let reverse (l: 'a list): 'a list = fold_left (fun s x -> x :: s) [] l
let reversed = reverse [1; 2; 3; 4; 5]

let full_range (n: int) : int list = range 2 n

let rec sieve (l: int list) : int list =
  match l with
  | [] -> []
  | x :: xs -> x :: sieve (filter (fun y -> y mod x <> 0) xs)

let sieve_fold (l: int list) : int list = foldr (fun x s -> x :: filter (fun y -> y mod x <> 0) s) l []

let primes_less_than n = sieve_fold (range 2 n)

let big_prod_plus_one n = foldr ( * ) 1 (sieve_fold (range 2 n)) + 1

let big_prod_plus_one_ugly n = foldr ( * ) 1 (foldr (fun x s -> x :: filter (fun y -> y mod x <> 0) s) (range 2 n) []) + 1

let rec smallest_divisor (n: int) (k: int) : int =
  match (k * k > n, n mod k = 0) with
  | (true, _) -> n
  | (_, true) -> k
  | _ -> smallest_divisor n (k + 1)

let euclid_tester (n: int) : int = smallest_divisor (big_prod_plus_one n) 2
