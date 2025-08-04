(* Lecture 15: Sorting and Arrays *)


(* Tarides Tomorrow *)












let collatz (n : int ref) : int =
  let a = ref 0 in
  while !n <> 1 do
    a := a + 1; 
    if !n mod 2 = 0 then
      n := n / 2; 
    else
      n := 2 * !n + 1
  done;
  !a
;;












let gcd (a : int) (b : int) : int =
  let x = ref a in
  let y = ref b in
  while !y <> 0 do
    let temp = !y in
    y := !x mod !y;
    x := temp
  done;
  !x
;;

(* 

while loop terminates in :

  1 -> (1, 1)
  2 -> (3, 2)
  3 -> (5, 3)
  4 -> (7, 4)
  5 -> ... 

  k -> (fib_k, fib_{k - 1})

  gcd 7 4 = gcd 4 3 = gcd 3 1 = 1
*)



(* Progress Measure: 

  Parameter 
    (i)   that is initialised to 0.
    (ii)  increases by a non-zero quantity that is bounded below in each iteration
    (iii) is bounded above. 



*)




let sqrt_da (x : float) : float =
  let guess = ref (x /. 2.0) in
  while abs_float ((!guess *. !guess -. x) > 0.01) do
    guess := 0.5 *. (!guess +. x /. !guess)
  done;
  !guess
;;













let random_walk (start : int) : unit =
  let pos : int ref = ref start in
  while !pos <> 0 do
    print_int !pos; print_newline ();
    if Random.bool () then
      incr pos
    else
      decr pos
  done;
  print_endline "Reached zero!"
;;







(* Sorting *)



(* What is the Blueprint? *)



(* 

  Requires: l : 'a list (where comparison is defined for 'a)

  Ensures: 
      For all i in [0, ... , len(l) - 1]
         l'[0] <= l'[1] <= l'[2] <= ... <= l'[len(l) - 1]

      l' is a permutation of l

*)


(* Introduce i 

For all j in [0, ... , (i-1)]
         l'[0] <= l'[1] <= l'[2] <= ... <= l'[i-1]

         Let remaining elements be l_remaining (l_remaining is a sublist of l)

         Find the l'[i]'th element??
         
         What are the properties of the l'[i]'th element?
         1. l'[i] is in l_remaining
         2. l'[i] >= l'[i-1]
         3. l'[i] <= elem in l_remaining

    Then we can go from i to i + 1. 

    Go all the way to n, and you are done! 

*)

let rec min (l : int list) : int = 
  match l with 
  | [] -> failwith "min: empty list"
  | h :: t -> 
    match (h < min t) with 
    | true -> h
    | false -> min t

let rec remove (l : int list) (elem : int) : int list = 
  match l with 
  | [] -> failwith "remove: elem not found"
  | h :: t -> 
    match (h = elem) with 
    | true -> t
    | false -> h :: (remove t elem)

let rec sort (l : int list) : int list = 
  match l with 
  | [] -> []
  | _ -> 
    let m = (min l) in 
    m :: sort (remove l m) 