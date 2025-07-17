exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]


(* Lecture 7: Map, Filter, Fold *)


type 'a mylist = Nil | Cons of 'a * 'a mylist

(* type list = [] | h :: tail *)

let empty_list : int list = []
let sample_list: int list = [1; 2]

(* [1 ; 2; 3] = 1 :: [2; 3] = 1 :: 2 :: [3] = 1 :: 2 :: 3 :: [] *)

let sample_list2: string list = ["1"; "2"]

let sample_list3: bool list = [true; true; false; false]


(* Find first element of a list *)

let first (l : 'a list): 'a option = 
  match l with 
  | [] -> None
  | h :: t -> Some h




(* Can you write a function that takes a list and gives you the last element of the list? *)

let last (l: 'a list) : 'a = 
  match l with 
  | [] -> failwith "Empty List"
  | [h] -> h
  | h :: tail -> last tail 
  



(* concat [1; 3; 4] [23; 1] = [1; 3; 4; 23; 1] *)
   
let rec concat (l1: 'a list) (l2: 'a list) : 'a list = 
  match l1 with 
  | [] -> l2
  | h :: t -> h :: (concat t l2)


(*   concat [1; 3; 4] [23; 1]
  =  concat (1 :: [3; 4]) [23; 1]
  = 1 ::  concat [3; 4] [23; 1]
  = 1 :: 3 :: concat [4] [23; 1]
  = 1 :: 3 :: 4 :: concat [] [23; 1]
  = 1 :: 3 :: 4 :: [23; 1]
  = [1; 3; 4; 23; 1]
  
*)


(* For ease of notation, you may also use the infix operator (@). That is
   concat l1 l2 = l1 @ l2 *)

(* How do you reverse a list? *)



let rec reverse (l : 'a list) : 'a list = 
  match l with 
  | [] -> []
  | h :: t -> concat (reverse (tail)) [h] 


(* 
   1. Given list l, take an empty list l2 on the side
   2. For each element in l, one by one, add it to my empty list. 

   For example: 

   l = [232; 321; 3] 
   l2 = []

   l = [321; 3]
   l2 = [232]

   l = [3]
   l2 = [321; 232]

   l = []
   l2 = [3; 321; 232]

*)

let reverse (l : 'a list) : 'a list = reverse1 l []

let rec reverse1 (l1 : 'a list) (l2 : 'a list) : 'a list = 
  match l1 with 
  | [] -> l2
  | h :: t -> reverse1 t (h :: l2)

(* To understand reverse, first let us look at an execution of reverse1: 

    reverse1 [1; 2; 3; 4] [5; 6]
  = reverse1 1::[2; 3; 4] [5; 6]
  = reverse1 [2; 3; 4] 1::[5; 6]
  = reverse1 [2; 3; 4] [1; 5; 6]
  = reverse1 2 :: [3; 4] [1; 5; 6]
  = reverse1 [3; 4] (2 :: [1; 5; 6])
  = reverse1 [4] (3 :: 2 :: [1; 5; 6])

  Now let us look at reverse: 

    reverse [1; 2; 3; 4]
  = reverse1 [1; 2; 3; 4] []
  = reverse1 1::[2; 3; 4] []
  = reverse1 [2; 3; 4] 1::[]
  = reverse1 [2; 3; 4] [1]
  = reverse1 [3; 4] [2; 1]
  = reverse1 [4] [3; 2; 1]
  = reverse1 [] [4; 3; 2; 1]
  = [4; 3; 2; 1]
*)


(* Take the last element and put it as the first *)

let all_but_the_last (l : 'a list) : 'a list = 
  match l with 
  | [] -> failwith "Empty list"
  | [h] -> []
  | h :: t -> h :: (all_but_the_last t)

(* all_but_the_last [1 ; 2 ; 3; 4] = [1; 2; 3] *)

let rec reverse (l : 'a list) : 'a list = 
  match l with 
  | [] -> []
  | _ -> (last l) :: (reverse (all_but_the_last l))




  






(* Can you write a function that takes two lists and returns the list with the lowest sum? *)

let rec sum (l : int list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> h + sum (t) 

let myfunction (l1 : int list) (l2 int list) : int list = 
  match (sum l1 > sum l2) with
  | true -> l2
  | false -> l1


(* myfunction [maxint] [maxint; 1]    *)



(* Can you write a function that takes a list l, and sums up all the positive elements in l? *)

let rec pos_sum (l : int list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> 
    match (h > 0) with 
    | true -> h + pos_sum (t)
    | false -> pos_sum(t)


let rec neg_sum (l : int list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> 
    match (h < 0) with 
    | true -> h + neg_sum (t)
    | false -> neg_sum(t)

    



(* Find minimum *)
exception EmptyListUnexpected

let min (a : int) (b : int) : int = 
  match (a < b) with
  | true -> a 
  | false -> b

let max (a : int) (b : int) : int = 
  match (a > b) with
  | true -> a 
  | false -> b

let rec findmax (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match (findmax t) with
      | None -> h
      | Some minimum_element_in_t -> max h (minimum_element_in_t)







(* Let us consider a case where we have a general comparison operation (not just minimum). This can be for example maximum, or the minimum with respect to a function (say f(x) = x^2 - 4x + 2). Let us write the code for it *)

let eval_poly (x: int) : int = (x*x) - (4*x) + 2 

let mycompare (a : int) (b: int) : int = 
  match (eval_poly a < eval_poly b) with
  | true -> a
  | false -> b

let rec findmin_poly (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match findopt_poly t with
      | None -> Some h
      | Some min_t -> mycompare (h) (min_t)


(* This function parallels find_min_simple. Instead of writing a function every time, it would be cool to get a general function: 

findopt (comparison : a' -> a' -> bool) (l: 'a list) : 'a = ...

For a fixed comparison function, we could write: 

*)

let rec findopt_general (compare: int -> int -> int) (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match findopt_poly t with
      | None -> Some h
      | Some min_t -> compare (h) (min_t)


let findmin l = findopt_general min l 
let findmax l = findopt_general max l 
let findpolymin l = findopt_general compare_poly l 




(* Can we pass compare_poly as a parameter to findopt_gen? Yes! Using higher order functions. *)

(* Higher Order Functions *)
(* A higher order function is one that takes another function as an input. *)
(* Consider this almost useless function apply, that takes in a function f and applies it to an argument x. That is 
      apply f x = f x ;; *)

let apply (f: 'a -> 'b) (x: 'a) : 'b  = f x 

let apply_twice (f: 'a -> 'a) (x: 'a) : 'a = f (f x)

let rec apply_k_times (f: int -> int) (x: int) (k : int ) : int = 
  match k with 
  | 0 -> x
  | 1 -> f x 
  | _ where k > 0 -> f (apply_k_times f x (k - 1))
  | _ -> failwith "k is negative"


(* apply_k_times f x 2 
   = f (apply_k_times f x 1)
   = f (f x)      *)



(* This is useless as putting x next to f automatically applies f to x. However, this gives us an example of a higher order function. *)


(* Similar to this, we can now define a findopt as: *)




(* Another use case is a way to apply a Function to a list, for example, we want to map such that: 
    map f [x0; x1; ...; xk] -> [(f x0); (f x1); ...; (f xk)]

    Concretely:
    
    map ((+) 3) [2;6;8] = [5;9;11])
    map (( * ) 2) [2;6;8] = [4;12;16])
    
    map (+) [2;6;8]
    
    As this evaluates to:

   map (+) [2;6;8] = [(+) 2; (+) 6; (+) 8];;

  As: 
  (+) : int -> int -> int
  2 : int
  (+) 2 : int -> int

  This is from what we have seen in currying and evaluation of partial functions.  
  
  What is the type of map?
  let rec map (f: 'a -> 'b) (l: 'a list) : 'b list = 
  ... 

  That is:
  map: ('a -> 'b) -> 'a list -> 'b list
*)

(* map ((+) 3) [2;6;8] = [5;9;11])
    map (( * ) 2) [2;6;8] = [4;12;16])
    
    *)

let rec map1 (l : int list) : int list = 
  match l with 
  | [] -> []
  | h :: t -> (h + 3) :: (map1 tail)

let rec map2 (l : int list) : int list = 
  match l with 
  | [] -> []
  | h :: t -> (h * 2) :: (map1 tail)




let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)

let double (x : int) = x * 2 

(* map2 l = map (double) l    *)





(* Let us consider another operation on lists: *)

let rec even_only (l: int list) : int list =
  match l with
  | [] -> []
  | h :: t -> 
      match (h mod 2) with
      | 0 -> h :: even_only t
      | _ -> even_only t

let rec odd_only (l: int list) : int list =
  match l with
  | [] -> []
  | h :: t -> 
      match (h mod 2) with
      | 1 -> h :: even_only t
      | _ -> even_only t




assert(even_only [3; 5; 7] = [])

(* What about a higher order function for such processes?

This corresponds to filtering a list. A filter selects all items from list l that satisfy property p. 

Say for example, filter (is_even) l = even_only l 

We can implement filter as: *)




(* Let us say we have a predicate. It checks whether our element satisfies some property, like being even. *)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | h :: t -> 
    match pred h with
    | true -> h :: filter pred t
    | false -> filter pred t





(* Now let us use filters! *)

(* Find all divisors of a number *)
(* divisors 12 = [1; 2; 3; 4; 6; 12] *)



let is_divisor (n: int) (x: int) : bool = (x mod n = 0)

let rec create_range (a : int) : int list = 
  match a with 
  | 0 -> []
  | _ when (a > 0) -> a :: create_range (a - 1) 
  | _ -> failwith "input is negative"

let divisors (n : int) : int list = 
  match n with 
  | n when n > 0 -> reverse (filter (is_divisor n) (create_range n))
  | _ -> failwith "input is non positive"





(* range a b = [a; ...; b]*)
let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b


(* 
  range 1 4
= 1 :: (range (1 + 1) 4)
= 1 :: range 2 4
= 1 :: 2 :: range 3 4
= 1 :: 2 :: 3 :: range 4 4
= 1 :: 2 :: 3 :: 4 :: range 5 4
= 1 :: 2 :: 3 :: 4 :: []
= [1; 2; 3; 4]
*)

(* List of divisors = all numbers between 1 and n, that divide n *)
(* List of divisors = range 1 n, then filter with (is divisor n) *)

let divisors (n: int) : int list = filter (is_divisor n) (range 1 n) 

(* Observe that this is basically constructing the set: 
   { x| x in [1; ...; n] and (is_divisor n x) }

   filter creates this list:
  = [ x | (x in l) and (p x = true) ]
*)

(* Can we use it for primality testing? *)
(* is_prime: int -> bool *)
(* Primes below n *)
let is_prime (x: int) : bool = (divisors x = [1; x]) 

(* 
   Note that lists are not sets. That is: 
   [2; 1] <> [1; 2]
   [2; 2; 1] <> [2; 1]
 *)

(* Generate all primes less than or equal to a number n *)
(* All primes in [1; ...; n] = range 1 n *)
let primes (n: int) : int list = filter is_prime (range 1 n)

(* That is, filter can be composed with interesting functions to process lists *)

(* Let us consider the problem of finding all find all pythagorean triples under 20

We first write it down mathematically: 

  { (a, b, c) | a in [1; ...; 20], b in [1; ...; 20], c in [1; ...; 20] and a^2 + b^2 = c^2 }

  To construct this, we need: 

  all_triples_under_20 = [(1,1,1), (1, 1, 2), ... (20, 20, 20)]

  and 

  is_pythagorean (a, b, c) = (a^2 + b^2 = c^2)

  Then the solution is: filter is_pythagorean all_triples_under_20

  And we are done!
*)

let rec range (a : int) (b : int) : int list = 
  match (a > b) with
  | true -> []
  | false -> a :: range (a + 1) b 

let all_num_under (n: int) : int list = range 1 n


(* I want [(a, b) | a <- all_num_under n ; b <- all_num_under n ] *)
(* For example, all_pairs_under 4 = [(1,1), (1, 2), (1, 3), (1, 4)
                                     (2,1), (2, 2), (2, 3), (2, 4)
                                     (3,1), (3, 2), (3, 3), (3, 4)
                                     (4,1), (4, 2), (4, 3), (4, 4) ]*)

let all_pairs_under (n: int) : (int * int) list = todo "Implement This"

(* Can you write: 

    all_pairs_under (n: int) (m: int) = 
                    [(a, b) | a <- all_num_under n ; b <- all_num_under m ]
    

    all_pairs_under 3 4 = [(1,1), (1, 2), (1, 3), (1, 4)
                           (2,1), (2, 2), (2, 3), (2, 4)
                           (3,1), (3, 2), (3, 3), (3, 4) ]

    Say I have all_pairs_under n m, can I use it to construct all pairs under n (m + 1)? Alternatively, can I go from all_pairs_under n m to all_pairs_under (n + 1) m?
                    
   all_pairs_under 3 5 = (all_pairs_under 3 4) @ [(1, 5), (2, 5), (3, 5)]

   let pair_with_5 (x : int) : int * int = (x , 5)

   all_pairs_under 3 5 = (all_pairs_under 3 4) @ (map pair_with_5 (all_num_under 3))
*)

let pair_with (k: int) (n: int) : int * int = (k , n)

let rec pairs_under (n: int) (m: int) : int * int list = 
  match n with
  | _ when n <= 0 -> [] 
  | _ -> (map (pair_with n) (range 1 m)) @ (pairs_under (n - 1) m)


(* pairs_under 2 3
 = (map pair_with 2 (range 1 3)) @ (pairs_under 1 3)
 = (map pair_with 2 [1; 2; 3]) @ (pairs_under 1 3)
 = [(2, 1); (2, 2); (2, 3)] @ (pairs_under 1 3)
 = [(2, 1); (2, 2); (2, 3)] @ (map pair_with 1 (range 1 3))
 = [(2, 1); (2, 2); (2, 3)] @ (map pair_with 1 [1; 2; 3])
 = [(2, 1); (2, 2); (2, 3)] @ [(1, 1); (1, 2); (1, 3)]
 = [(2, 1); (2, 2); (2, 3); (1, 1); (1, 2); (1, 3)]
*)


let triple_with (p : int) ((n,m) : int * int) : int * int * int = (n, m, p)

let rec triples_under (n : int) (m : int) (p : int) : (int * int * int) with =
  match p with 
  | _ when p <= 0 -> []
  | _ -> map (triple_with p) (pairs_under n m) @ (triples_under n m (p - 1))


(* You can also try for cartesian product: *)

(* Here is a first try: *)

let rec cart_prod_first_try (l1: 'a list) (l2: 'b list): ('a * 'b) list = 
  match (l1, l2) with 
  | ([], _) -> []
  | (_, []) -> []
  | (x:: xs, y:: ys) -> (x, y)::(cart_prod_first_try xs ys)


(* Consider its run:

  cart_prod [1; 2] [3; 4; 5]
= (1, 3):: cart_prod [2] [4; 5]
= (1, 3):: (2, 4) :: cart_prod [] [5]
= (1, 3):: (2, 4) :: []
= [(1, 3); (2, 4)]

Instead, we wanted [(1, 3); (1, 4); (1; 5); (2, 3); (2, 4); (2, 5)]

For this, the recursive case should be something like: 
  (pair up x with every thing in l2) :: (cart_prod xs l2) 
  
  That is (1 times [3; 4; 5]) :: cart_prod [2] [3; 4; 5] 
*)

let rec multiply_list (a: 'a) (l: 'b list) : ('a * 'b) list = 
  match l with 
  | [] -> [] 
  | x :: xs -> (a, x) :: multiply_list a xs




(* multiply_list 1 [3; 4; 5]
= (1, 3) :: multiply_list 1 [4; 5]
= (1, 3) :: (1, 4) :: multiply_list 1 [5]
= (1, 3) :: (1, 4) :: (1, 5) :: multiply_list 1 []
= (1, 3) :: (1, 4) :: (1, 5) :: []
= [(1, 3); (1, 4); (1, 5)]
*)

let rec cartesian_product (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  match l1 with
  | [] -> []
  | x :: xs -> concatenate (multiply_list x l2) (cartesian_product xs l2)


let range20 : int list = range 1 20

let all_triples : ((int * int) * int) list = cartesian_product (cartesian_product range20 range20) range20

(* What is the type of all_triples? 
   It could be: 
   1) int list * int list * int list
   2) (int * int * int) list
   3) Something else 
    

  It happens to be something else. Try printing it and checking!

  Notice that: 
  ([], [1; 3; 4], [2; 19]) : int list * int list * int list

  And:
  [(1, 2, 3); (1, 4, 6)]: (int * int * int) list

  But as tuples aren't natively associative, we have: 

  all_triples = [((1, 1), 1); ... ]

  That is all_triples: ((int * int) * int) list

  Let us fix that! *)



let bar (p: (int * int) * int) : (int * int * int) = 
  match p with
  | ((x, y), z) -> (x, y, z)


(* Easier: let bar ((x, y), z) = (x, y, z) *)

(* All well formed triples: *)
let awf_triples = map bar all_triples

let is_pythagorean (p: int * int * int) : bool =
  match p with
  | (x, y, z) when (x * x + y * y = z * z) -> true
  | _ -> false


let all_pythagorean_triples = filter is_pythagorean awf_triples



(* Combining map and filter *)

let sqr (a: int) : int = a * a
let is_even (x: int) : bool = x mod 2 = 0
let sqr_even l = map sqr (filter is_even l)

(* This is the same as:

    S = { a^2 | a in l, a is even}
    S = { sqr(a) | a in l, is_even(a)}

In general, most set builder notation can be converted to a list construction using the appropriate map and filter.

*)

(* Combining Elements *)

let rec sumlist (l: int list) : int =
  match l with
  | [] -> 0
  | x :: xs -> x + sumlist xs


let rec multlist (l: int list) : int =
  match l with
  | [] -> 1
  | x :: xs -> x * multlist xs


(* What is the common pattern? 


let rec combinelist (l: int list) : int =
  match l with
  | [] -> default_value
  | h :: tail -> combine h (combinelist tail)


  Same as before, we turn it into a higher order function: 

let rec combine func value list =
  match list with
  | [] -> value
  | h :: tail -> func h (combine func value tail)
;;

What is its type?

f : 'a -> 'b -> 'b
v : 'b
l : 'a list

This is built in as List.fold_right
Same way, map and filter are available as List.map and List.filter

*)

let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)


let add (x: int) (y: int) : int = x + y

let sumlist l = foldr add 0 l

(* 

  sumlist [2; 4]
= foldr add 0 [2; 4]
= add 2 (foldr add 0 [4])
= add 2 (foldr add 0 [4])
= add 2 (add 4 foldr add 0 [])
= add 2 (add 4 0)
= add 2 4
= 6
*)

