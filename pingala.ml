(* Fixed Exponentiation Functions *)

(* Naive exponentiation *)
let rec exp (n : int) (k : int) : int =
  match k with
  | 0 -> 1
  | _ when k < 0 -> 0
  | _ -> n * exp n (k - 1)

(* Fast exponentiation using divide and conquer - CORRECT VERSION *)
let rec fast_exp (n : int) (k : int) : int =
  match k with
  | 0 -> 1
  | _ when k < 0 -> 0
  | _ ->
    if (k mod 2) = 0 then
      fast_exp (n * n) (k / 2)
    else
      n * fast_exp n (k - 1)

(* Fibonacci Functions *)

(* Naive recursive fibonacci *)
let rec fib_naive (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib_naive (n - 1) + fib_naive (n - 2)

(* Tail-recursive fibonacci *)
let fib_tail (n : int) : int =
  let rec fib_helper acc1 acc2 count =
    if count = 0 then acc1
    else fib_helper acc2 (acc1 + acc2) (count - 1)
  in fib_helper 0 1 n

(* Fast fibonacci using fast doubling algorithm - FASTEST METHOD *)
let fib_fast_doubling (n : int) : int =
  let rec fib_double k =
    if k = 0 then (0, 1) (* F(0) = 0, F(1) = 1 *)
    else
      let (fk, fk1) = fib_double (k / 2) in
      let c = fk * (2 * fk1 - fk) in
      let d = fk * fk + fk1 * fk1 in
      if k mod 2 = 0 then (c, d)
      else (d, c + d)
  in
  fst (fib_double n)

(* Matrix operations for fast fibonacci - OPTIMIZED *)
type matrix = int * int * int * int

let matrix_mult (a11, a12, a21, a22) (b11, b12, b21, b22) =
  (a11 * b11 + a12 * b21,
   a11 * b12 + a12 * b22,
   a21 * b11 + a22 * b21,
   a21 * b12 + a22 * b22)

let rec matrix_power (mat : matrix) (n : int) : matrix =
  if n = 0 then (1, 0, 0, 1)  (* Identity matrix *)
  else if n = 1 then mat
  else if n mod 2 = 0 then
    let half_power = matrix_power mat (n / 2) in
    matrix_mult half_power half_power
  else
    matrix_mult mat (matrix_power mat (n - 1))

(* Fast fibonacci using matrix exponentiation - CORRECTED *)
let fib_matrix (n : int) : int =
  if n = 0 then 0
  else if n = 1 then 1
  else
    let base_matrix = (1, 1, 1, 0) in
    let (_, f_n, _, _) = matrix_power base_matrix n in
    f_n

(* Function to measure execution time *)
let measure_time (f : 'a -> 'b) (arg : 'a) : float =
  let start_time = Sys.time () in
  let _ = f arg in
  Sys.time () -. start_time

(* Test values *)
let base = 2
let exponent = 20
let fib_n = 35  (* Use smaller number for naive fib to avoid long wait *)

let () =
  Printf.printf "=== Exponentiation Tests ===\n";
  Printf.printf "Testing %d^%d:\n" base exponent;
  Printf.printf "Naive Exponentiation Time: %f seconds\n" (measure_time (exp base) exponent);
  Printf.printf "Fast Exponentiation Time: %f seconds\n" (measure_time (fast_exp base) exponent);
  Printf.printf "Naive result: %d\n" (exp base exponent);
  Printf.printf "Fast result: %d\n" (fast_exp base exponent);
  
  Printf.printf "\n=== Fibonacci Tests ===\n";
  Printf.printf "Testing Fib(%d):\n" fib_n;
  Printf.printf "Naive Fibonacci Time: %f seconds\n" (measure_time fib_naive fib_n);
  Printf.printf "Tail-Recursive Fibonacci Time: %f seconds\n" (measure_time fib_tail fib_n);
  Printf.printf "Matrix Fibonacci Time: %f seconds\n" (measure_time fib_matrix fib_n);
  Printf.printf "Fast Doubling Fibonacci Time: %f seconds\n" (measure_time fib_fast_doubling fib_n);
  Printf.printf "Naive result: %d\n" (fib_naive fib_n);
  Printf.printf "Tail-recursive result: %d\n" (fib_tail fib_n);
  Printf.printf "Matrix result: %d\n" (fib_matrix fib_n);
  Printf.printf "Fast doubling result: %d\n" (fib_fast_doubling fib_n);
  
  Printf.printf "\n=== Large Fibonacci Test ===\n";
  let large_fib_n = 10000 in
  Printf.printf "Testing Fib(%d):\n" large_fib_n;
  Printf.printf "Tail-Recursive Time: %f seconds\n" (measure_time fib_tail large_fib_n);
  Printf.printf "Matrix Time: %f seconds\n" (measure_time fib_matrix large_fib_n);
  Printf.printf "Fast Doubling Time: %f seconds\n" (measure_time fib_fast_doubling large_fib_n);
  Printf.printf "Results should be equal: %b\n" 
    (fib_matrix large_fib_n = fib_fast_doubling large_fib_n)