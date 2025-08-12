(* Lecture 18: Merge Sort *)


let rec take (l : 'a list) (n : int) : 'a list = 
  match n with 
  | _ when n <= 0 -> []
  | _ -> 
    match l with 
    | [] -> []
    | h :: t -> h :: (take (n - 1) t) 

let rec drop (l : 'a list) (n : int) : 'a list = 
  match n with 
  | _ when n >= length l -> l
  | _ -> 
    match l with 
    | [] -> []
    | h :: t -> (take n t) 

let split (l : 'a list) : ('a list * 'a list) = 
  (take l ((length l) // 2), drop l ((length l) // 2))


let rec split2 (l : 'a list) : ('a list * 'a list) = 
  match l with 
  | [] -> ([], [])
  | [x] -> ([x], [])
  | h1 :: h2 :: rest -> 
    let (left, right) = split2 rest in 
    ([h1:: left], [h2 :: right])

let rec mergesort (l : int list) : int list = 
  match l with 
  | [] -> []
  | _ -> 
    let (left, right) = split l in 
    merge (mergesort left) (mergesort right)

let rec merge (left : int list) (right : int list) : int list = 
  match left, right with 
  | [], _ -> right
  | _, [] -> left 
  | x :: xs, y :: ys ->
    match (x < y) with 
    | true -> x :: merge xs right
    | false -> y :: merge left ys



let merge (a1 : int array) (a2 : int array) : int array =
  let len1 = Array.length a1 in
  let len2 = Array.length a2 in
  let result = Array.make (len1 + len2) 0 in
  let i = ref 0 in
  let j = ref 0 in
  let k = ref 0 in
  while !i < len1 && !j < len2 do
    if a1.(!i) <= a2.(!j) then (
      result.(!k) <- a1.(!i);
      incr i
    ) else (
      result.(!k) <- a2.(!j);
      incr j
    );
    incr k
  done;
  while !i < len1 do
    result.(!k) <- a1.(!i);
    incr i; incr k
  done;
  while !j < len2 do
    result.(!k) <- a2.(!j);
    incr j; incr k
  done;
  result

let split (arr : int array) : int array * int array =
  let n = Array.length arr in
  let mid = n / 2 in
  let left = Array.sub arr 0 mid in
  let right = Array.sub arr mid (n - mid) in
  (left, right)

let rec mergesort_array (arr : int array) : int array =
  let n = Array.length arr in
  if n <= 1 then arr
  else
    let left, right = split arr in
    merge (mergesort_array left) (mergesort_array right)





