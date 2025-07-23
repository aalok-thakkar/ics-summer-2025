exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Lecture 9: Origami Programming *)

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | h :: t ->
    match pred h with
    | true -> h :: filter pred t
    | false -> filter pred t


let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b


let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)

let length (l: 'a list) = foldr (fun x y -> 1 + y) 0 l

(* This question will be in the viva: *)


let funkyfold (f : 'a -> 'b) (l : 'a list) : 'b list = 
  foldr (fun (x: 'a) (y : 'b list) -> f (x) :: y) [] l


let funkyfold2 (p: 'a -> bool) (l: 'a list) : 'a list = 
  foldr (fun x y -> match (p x) with true -> x :: y | false -> y) [] l




let count_occurrences (elem : 'a) (l: 'a list) = 
  foldr (fun x n -> match (x = elem) with true -> 1 + n | false -> n) 0 l
  

(* let count_twos = count_occurrences 2 [1;2;2;3;2;4;5] *)



  

let rec zip (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys
  
(* [(1, "a"), (2, "b"), (3, "c")] = zip [1;2;3] ["a";"b";"c"] *)
  

(* zip [1 ; 2; 3; 4] [4; 2; 1] = [(1,4); (2, 2); (3, 1)]*)

(* Prove that: 
    length (zip l1 l2) = min (length l1) (length l2)


    By induction on l1. 

    Base Case: l1 = []
    Then zip [] l2 = [] (by definition)

    Therefore length (zip [] l2) = 0 = min (length []) (length l2).

    Induction Hypothesis: 

    Induction on l1 (that is on the length of l1). 
    Let property hold for all l1 of length = k. 

    Then for some (x :: l1)??

    length (zip (x :: l1) l2) = min (length (x :: l1)) (length l2)

    Case 1: l2 = []

    In which case, we get 0 on both sides

    Case 2: l2 = (y :: ys)

        length (zip (x :: l1) (y :: ys))
      = length ((x, y) :: (zip l1 ys))
      = 1 + length (zip l1 ys)
      = 1 + (min (length l1) (length ys))


      RHS: 

      min (length (x :: l1)) (length (y:: ys))
      = min (1 + length l1) (1 + length ys)
      = 1 + min (length l1) (length ys)
*)



let rec foldl (f: 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> foldl f (f v x) xs

let sumlist_l (l : int list) = foldl ( + ) 0 l

(* 

  sumlist_l [1; 4; 34; 12]
= foldl ( + ) 0 [1; 4; 34; 12]
= foldl ( + ) (0 + 1) [4; 34; 12]
= foldl ( + ) (0 + 1 + 4) [34; 12]
= foldl ( + ) (0 + 1 + 4 + 34) [12]
= foldl ( + ) (0 + 1 + 4 + 34 + 12) []
= (0 + 1 + 4 + 34 + 12)

*)


(* Try subtraction on foldl and foldr and see they are different *)

let rec foldl (f : 'b -> 'a -> 'b) (v : 'b) (l: 'a list) : 'b = 
  match l with 
  | [] -> v
  | x :: xs -> foldl f (f v x) xs


(* Write reverse using foldl *)

let reverse (l : 'a list) : 'a list = 
  foldl (fun x y -> y :: x) [] l

(*  reverse [1; 2; 3] 
  = foldl f [] [1; 2; 3]
  = foldl f [1] [2; 3]
  = foldl f [2; 1] [3]
  = foldl f [3; 2; 1] []
  = [3; 2; 1]
*)

(* Can you write reverse with foldr? *)







(* Init as Unfolding *) 
let rec init (n : int) (f : int -> 'a) : 'a list =
  match n with 
  | _ when n <= 0 -> []
  | _ -> (init (n - 1) f) @ [f n]


(* Compute [f(1), f(2), f(3), ... , f(n)]*)




(* Testing Euclid's Proof *)

let factorial (n : int) : int = foldl ( * ) 1 (range 1 n)

let prime_factor (n : int) : int = 
  match (filter (fun x -> (n mod x = 0)) (range 2 n)) with 
  | [] -> failwith "invalid input"
  | h :: _ -> h 

let euclid_tester1 (n : int) : int = 
  prime_factor ((factorial n) + 1)






let rec sieve (l: int list) : int list =
  match l with
  | [] -> []
  | x :: xs -> x :: sieve (filter (fun y -> y mod x <> 0) xs)


let sieve_fold (l: int list) : int list = 
  foldr (fun x s -> x :: filter (fun y -> y mod x <> 0) s) [] l 

let big_prod_plus_one n = (foldr ( * ) 1 (sieve_fold (range 2 n))) + 1

let euclid_tester2 (n: int) : int = prime_factor (big_prod_plus_one n)



(* N Queens *)


let cube (x: int): int = x * x * x
let result1 : int = cube (12) + cube (1)
let result2 : int = (12 |> cube) + (1 |> cube)

(*       x |> f = f (x)        *)

(* Folding with Lists and Pipes *)

let cumbersome_result : int = 
  foldr (fun x s -> x + s) 0 (map (fun x -> x * x) [1; 2; 3; 4])

let result : int =
  [1; 2; 3; 4]
  |> map (fun x -> x * x)
  |> foldr (fun x s -> x + s) 0 


(* This can be written as just one fold *)

let result : int = foldr (fun x s -> (x * x) + s) 0 [1; 2; 3; 4]





(* Can all maps be written as fold? *)

let map_fold (f: 'a -> 'b) (l: 'a list) : 'b list = 
  foldr (fun x s -> (f x) :: s) [] l

(* How about filter? *)

let filter_fold (p: 'a -> bool) (l: 'a list) : 'a list = 
  foldr (fun x s -> if (p x) then (x :: s) else s) [] l 








(* The n-queens problem *)
(* Max Bezzel published the eight queens puzzle in 1848. Franz Nauck published the first solution in 1850 and extended the puzzle to the n queens problem. Since then, many mathematicians, including Carl Friedrich Gauss, have worked it. 

Consider a 4x4 chess board. Can you place four queens that do not threaten each other? 

        - Q - - 
        - - - Q 
        Q - - - 
        - - Q - 
*)

(* Given n, can you find a nxn chess board configuration where n queens do not threaten each other? How do you even represent such a chessboard? *)

(* As a chess board with "-" and "Q" *)
let representation1 = [
  ["-"; "Q"; "-"; "-"]; 
  ["-"; "-"; "-"; "Q"]; 
  ["Q"; "-"; "-"; "-"]; 
  ["-"; "-"; "Q"; "-"]
] 

(* As just booleans *)
let representation2 = [
  [false; true; false; false]; 
  [false; false; false; true]; 
  [true; false; false; false]; 
  [false; false; true; false]
]

(* As a list of positions of the queens, where index is the row. *)
let representation3 = [
  1; 
  3; 
  0; 
  2
] 

(* As pairs (col, row). We will use this going ahead. *)
let representation4 = [
  (1, 0); 
  (3, 1); 
  (0, 2); 
  (2, 3)
]


(* Check if two queens threaten each other *)
let threatens ((col1, row1) : int * int) ((col2, row2) : int * int) : bool =
  col1 = col2 || row1 = row2 || abs (col1 - col2) = abs (row1 - row2) 

let forall (p : 'a -> bool) (l : 'a list) = 
  foldr (fun (x : 'a) (y: bool) -> p (x) && y) true l

  
(* Check if a new queen is safe with respect to existing queens *)
let is_safe (new_queen : int * int) (existing_queens : (int * int) list) : bool =
  forall (fun q -> not (threatens new_queen q)) existing_queens 
  
(* Helper function: Generate all possible positions for the next row *)
let next_row (existing_queens: (int * int) list) (n: int) (r: int) : (int * int) list  =
  init n (fun x-> (x, r))                             (* make a list of all positions *)
  |> filter (fun q -> is_safe q existing_queens)      (* and then you filter *)


(* Solve the n-Queens problem starting with a given row *)
let rec solve (n: int) (r: int) (existing_queens: (int * int) list) : ((int * int) list) list =
  match r with
  | _ when r = n -> [existing_queens]
  | _ ->
    next_row existing_queens n r
    |> map (fun r -> (r :: existing_queens))
    |> foldr (fun qs s -> (solve n (r + 1) qs @ s)) []

  
let n_queens n = solve n 0 [] 

(* Representation of a chessboard as a list of lists of strings *)

let chessboard_row (c : int) (n : int) : string = 
  init n (fun x -> x)
  |> map (fun x -> if (x = c) then "Q" else "-")
  |> foldr (fun x s -> x ^ " " ^ s) ""


let chessboard (queens: (int * int) list) : string = 
  queens
  |> map (fun (c, r) -> (chessboard_row c (length queens)))
  |> foldr (fun x s -> x ^ "\n" ^ s) ""


(* And now finally: pretty-print all solutions *)
let rec print_solutions solutions =
  match solutions with
  | [] -> ()
  | x :: xs ->
      print_endline (chessboard x);
      print_endline "----------";
      print_solutions xs

  
let n = 10
let () = print_solutions (n_queens n)