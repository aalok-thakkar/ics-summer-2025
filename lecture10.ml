exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Lecture 10: Origami Programming *)

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

let rec foldl (f: 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> foldl f (f v x) xs

let rec init (n : int) (f : int -> 'a) : 'a list =
  match n with 
  | _ when n <= 0 -> []
  | _ -> (init (n - 1) f) @ [f n]



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



(* 

        - Q - - 
        - - - Q 
        Q - - - 
        - - - - 

*)




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










(* Tail Recursion *)

let rec factorial (n : int) : int = 
  match n with
  | 0 -> 1
  | _ -> n * factorial (n - 1)

(* Let us look at how these functions are implemented by the computer.

When we call factorial 3, we have the following:

factorial 3 -> Not the base case -> Write that you need to compute 
'3 * factorial 2' and then compute 'factorial 2'

This process is called creating a stack frame. Basically, the computer is writing down that after the recursive call, it needs to do something else. In this case, it needs to multiply by 3. 

That is ...

factorial 3 -> Create a stack frame for 3 * factorial 2
factorial 2 -> Create a stack frame for 2 * factorial 1
factorial 1 -> Create a stack frame for 1 * factorial 0
factorial 0 -> Base case -> Return 1

Then, the process unwinds ... 

factorial 1 = 1 * 1 -> Return 1
factorial 2 = 2 * 1 -> Return 2
factorial 3 = 3 * 2 -> Return 6

When factorial is called, the computer stores all the waiting function calls. All the stack frames. This can take a lot of memory, as well as a lot of time. One way to reduce it is called tail recursion! *)

(* Tail Recursion *)
(* The recursive call is the last operation in the function *)

let rec factorial_helper (a : int) (n : int) : int =
  match n with
  | 0 -> a
  | _ -> factorial_helper (a * n) (n - 1)


let factorial_tail (n : int) : int = factorial_helper 1 n


(* 

What is happening here? 

factorial_tail 3 -> calls factorial_helper 1 3

factorial_helper 1 3 -> Not the base case -> create stack frame `factorial_helper (1 * 3) 2'
factorial_helper 3 2 -> ...

But hey, I can just reuse the stack frame, as the previous stack frame had only a recursion call, no other operations. 

Hence:

factorial_helper 3 2 -> factorial_helper (3 * 2) 1
factorial_helper 6 1 -> factorial_helper (6 * 1) 0
factorial_helper 6 0 -> Base case reached -> returns 6

So we had only one stack frame, reused. At the implementation level, this needs low memory. And is faster. 

This optimization is called Tail Call Optimization (TCO).

*)

(* Is it really faster? We can measure it as follows: *)

let measure_time (f : 'a -> 'b) (arg : 'a) : float =
  let start_time = Sys.time () in
  let _ = f arg in
  Sys.time () -. start_time

(* Measure the time for both the implementations *)
let n = 1000000


let () =
  Printf.printf "Naive Factorial Time: %f seconds)\n" (measure_time factorial n);
  Printf.printf "Tail-Recursive Factorial Time: %f seconds)\n" (measure_time factorial_tail n)


(* What about triangle? *)

let rec triangle (n : int) : int = 
  match n with 
  | 0 -> 0
  | _ -> n + triangle (n - 1)


let rec triangle_helper (a: int) (n : int) : int = 
  match n with 
  | 0 -> a
  | _ when n < 0 -> 0
  | _ -> triangle_helper (a + n) (n - 1)

let triangle n = triangle_helper 0 n

(* What about fibonacci? *)

let rec fib (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

let rec fib_helper (a: int) (b : int) (n : int) : int = 
  match n with
  | _ when n < 0 -> failwith "invalid input" 
  | 0 -> a
  | 1 -> b
  | _ -> fib_helper (a + b) (a) (n - 1)

let fib_tail_recursive (n : int) : int = fib_helper 1 1 n

(* 

   fib_tail 5 
-> fib_helper 0 1 5
-> fib_helper 1 (0+1) 4
= fib_helper 1 1 4
-> fib_helper 1 2 3
-> fib_helper 2 3 2
-> fib_helper 3 (2+3) 1
-> 5 (BASE CASE)

*)





(* Can map, filter, and fold be converted to tail recursions? *)

let rec foldl (f : 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> foldl f (f v x) xs


(* Turns out foldl is already tail recursive. What about foldr? *)

let rec foldr (f : 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> f x (foldr f v xs)
;;

(* Fold left is tail recursive. Fold right is just fold right in the reverse. As the composition of tail recursive functions is tail recursive, all we need is a tail recursive list reversal *)

let rec reverse_helper (a : 'a list) (l: 'a list) : 'a list =
  match l with
  | [] -> a
  | x :: xs -> reverse_helper (x :: a) xs
;;  

let reverse (l : 'a list) : 'a list = reverse_helper [] l ;;

(* Are all recursive functions tail recursive? Clearly not by default. Can every recursive function be transformed into a tail-recursive form? Think about it. *)


(* Consider this: *)

let rec layer (f : int -> int) (x : int) (n: int) : int = 
  match n with 
  | 0 -> x
  | _ -> f (layer f x (n - 1))


(* This can be made into a tail recursive function without new arguments or helper functions. *)

let rec layer_tail (f : int -> int) (x : int) (n : int) : int = 
  match n with 
  | 0 -> x
  | _ -> layer_tail f (f x) (n - 1)



(* 
  The unit type in OCaml is a special type that has only one value: (). 
  It is used to indicate that a function does not return any meaningful value, 
  but may perform side effects such as printing, modifying a reference, or interacting with the outside world.

  For example:
*)

(* let print_hello = print_endline "Hello, world!" *)
(* The type of print_hello is unit -> unit *)

(* You call it as: 
let () = print_hello
*)

(* Functions returning unit are often used for their side effects. 
  For example, printing all elements of a list: *)

let rec print_list (l : int list) : unit =
  match l with
  | [] -> ()
  | x :: xs -> Printf.printf "%d " x; print_list xs; 
;;

(* print_list [1;2;3;4]; print_endline "" *)


(* 
for i = 1 to 5 do
  Printf.printf "i = %d\n" i
done
*)

(* In summary, use unit when you want to indicate that a function is called for its effect, not for its result. 
  This is common in printing, mutation, assertion, or other imperative actions.
*)

let rec layer_unit (f : int -> unit) (n : int) : unit =
  match n with
  | 0 -> ();
  | _ -> f n; layer_unit f (n - 1);
;;

(* Here is a concrete example:  *)

(* 
let print_n n = Printf.printf "%d\n" n ;;
let () = layer_unit print_n 4 ;;

*)

(* This prints:

4
3
2
1

How to do it in the other order? *)

let rec layer_unit_asc (f : int -> unit) (n : int) : unit =
  match n with
  | 0 -> ();
  | _ -> layer_unit_asc f (n - 1); f n;
;;

(* This correct. Here, the recursive function layer is called first, and then f is called. However, this is not tail recursive. How can you make it tail recursive? We will use a helper function. Instead of tracking an object representing data, we will track the number of times the recursion has been called. Consider *)

let rec layer_asc_helper (i: int) (f : int -> unit) (n : int) : unit =
  match (i = n) with 
  | true -> f n;
  | false -> f i; layer_asc_helper (i+1) f n


let layer_unit_tail (f : int -> unit) (n : int) : unit = layer_asc_helper 1 f n 

(* Alternatively, using if ... then ... else, and with (i < n), just to cover all cases: *)

let rec layer_asc_helper_ite (i: int) (f : int -> unit) (n : int) : unit =
  if i > n then 
    ()
  else begin
    f i;
    layer_asc_helper_ite (i + 1) f n
  end


(* This is a very specific type of tail recursion. Such a tail recursion is called a for loop. *)

let layer_unit_for (f : int -> unit) (n : int) : unit =
  for i = 1 to n do
    f i
  done
