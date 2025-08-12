(* Lecture 19: Merge Sort and Quick Sort *)


let rec merge (l1 : int list) (l2 : int list) : int list =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | x::xs, y::ys ->
    match (x <= y) with 
    | true -> x :: (merge xs l2)
    | false -> y :: (merge l1 ys)

let rec split (l : int list) : int list * int list =
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x::y::rest ->
    let (left, right) = split rest in
    (x::left, y::right)

let rec mergesort (l : int list) : int list =
  match l with
  | [] | [_] -> l
  | _ ->
    let left, right = split l in
    merge (mergesort right) (mergesort left)














let rec filter (pred : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | h :: t ->
    match pred h with
    | true -> h :: filter pred t
    | false -> filter pred t


let rec quicksort (l : int list) : int list = 
  match l with 
  | [] -> []
  | h :: t -> 
    let left = filter (fun x -> x < h) l in
    let right = filter (fun x -> x >= h) l in 
    (quicksort left) @ (quicksort right)











    


let swap (arr : int array) (i : int) (j : int) =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp

let partition (arr : int array) (low : int) (high : int) =
  let pivot = arr.((low + high) / 2) in
  let i = ref low in
  let j = ref high in
  while !i <= !j do
    while arr.(!i) < pivot do incr i done;
    while arr.(!j) > pivot do decr j done;
    if !i <= !j then (
      swap arr !i !j;
      incr i;
      decr j
    )
  done;
  (!i, !j)

let rec quicksort_helper (arr : int array) (low : int) (high : int) =
  if low < high then
    let i, j = partition arr low high in
    if low < j then quicksort_helper arr low j;
    if i < high then quicksort_helper arr i high

let quicksort (arr : int array) =
  if Array.length arr > 1 then
    quicksort_helper arr 0 (Array.length arr - 1)
