(* Lecture 13: The While Loop *)


(* The Integer Square Root Question *)


(*  

  Given n, m, and c... find a such that: 

  Requires: n > 0, m > 0, c > 0
  Ensures : a^2 <= n and n < (m * a + c)^2


  Solve this problem.



  a := ref 0
  while (n >= (m*!a + c)) {
     a := ??
  }
  return a
  
  Update a to a' so that: 

  If invariant holds for a. 

  That is a^2 <= n

  And termination condition does not hold,

  and n >= (m * a + c)^2
  
  Then the invariant holds for a'
  
  Update a to a' such that 

  a'^2 <= n. What are the candidates for a'?


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

let maxlist1 (l : int list) : int = 
  match l with 
  | [] -> failwith " NOOOOOOO"
  | h :: t -> 
    match (h > maxlist1 t) with
    | true -> h
    | false -> maxlist1 t


let max_list (l : int list) : int = 
  match l with 
  | [] -> failwith " NOOOOOOO"
  | h :: t -> max_list_helper h t

let rec max_list_helper (a: int) (l : int list) = 
  match l with 
  | [] -> a
  | h :: t ->
    match (a > h) with
    | true -> max_list_helper a t
    | false -> max_list_helper h t






let rec max_list_anushka (l : int list) : int = 
  match l with 
  | [] -> failwith "NOOOOOOO"
  | h :: t -> 
    match t with 
    | [] -> h
    | h2 :: t2 -> 
      match (h > h2) with
      | true -> max_list_anushka (h :: t2)
      | false -> max_list_anushka (h2 :: t2)













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








(* Is Prime? *)

let is_prime (n: int) : bool =
  if n < 2 then false
  else
    let i = ref 2 in
    let has_divisor = ref false in
    while !i * !i <= n && not !has_divisor do
      if n mod !i = 0 then has_divisor := true;
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
