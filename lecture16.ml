(* Lecture 16: More on Sorting *)


(* Selection Sort-ish *)

(* 

Requires: l : 'a list (and comparison is defined on 'a, for example int, float, string)
Ensures: sort (l) = l'
      a) l' is a permutation of l
      b) l'[0] <= l'[1] <= ... <= l'[len(l') - 1] 


*)

let rec min (l : int list) : int = 
  match l with 
  | [] -> failwith "min: empty list"
  | [h] -> h
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

let rec remove_tail_recursive (seen: int list) (unseen : int list) : int list = 
  match unseen with 
  | [] -> failwith "remove: elem not found"
  | h :: t -> 
    match (h = elem) with 
    | true -> seen @ t
    | false -> remove_tail_recursive (h :: seen) (t)

let rec sort (l : int list) : int list = 
  match l with 
  | [] -> []
  | _ -> 
    let m = (min l) in 
    m :: sort (remove l m)


let rec max (l : int list) : int = 
  match l with 
  | [] -> failwith "min: empty list"
  | [h] -> h
  | h :: t -> 
    match (h > min t) with 
    | true -> h
    | false -> min t

let rec sort_tail_recursive_helper (l : int list) (a : int list) : int list = 
  match l with 
  | [] -> a
  | _ -> 
    let m = (max l) in sort_tail_recursive_helper (remove l m) (m :: a) 

let sort_tail_recursive (l : int list) : int list = 
  sort_tail_recursive_helper l []






  
let sort_loop (l : int list) : int list = 
  let a = ref [] in 
  let l2 = ref l in
  while (!l2 <> []) do
    let (m, l_removed_max) = max_loop (!l2) in 
    a := m :: !a; 
    l2 := l_removed_max; 
  done; 
  !a 

let max_loop (l : int list) : (int * int list) = 
  let max = ref None in 
  let seen = ref [] in 
  let unseen = ref l in 
  while (!unseen <> []) do 
    match unseen with 
    | [] -> failwith "max_loop: not possible"
    | h :: t -> 
      match !max with 
      | None -> max := Some h;
      | Some m -> 
        if (h > m) then max := Some h;
      seen := h :: !seen;
      unseen := t ;
    done 

  match !max with 
  | None -> failwith "max_loop: list is empty"
  | Some m -> (m, seen) 
;;




