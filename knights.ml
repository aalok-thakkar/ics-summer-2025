(* M Knights *)
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
  | _ -> (init (n - 1) f) @ [f (n - 1)]

let forall (p : 'a -> bool) (l : 'a list) : bool =
  foldr (fun (x : 'a) (y: bool) -> p (x) && y) true l

(* Check if two knights threaten each other *)
let threatens ((col1, row1) : int * int) ((col2, row2) : int * int) : bool =
  (abs (col1 - col2) = 1 && abs (row1 - row2) = 2)
  || (abs (col1 - col2) = 2 && abs (row1 - row2) = 1)

(* Check if a new knight is safe with respect to existing knights *)
let is_safe (new_knight : int * int) (existing_knights : (int * int) list) : bool =
  forall (fun q -> not (threatens new_knight q)) existing_knights

(* Generate an n * n chessboard - creates list of all (col, row) positions *)
let board (n : int) : (int * int) list =
  range 0 (n - 1)
  |> map (fun row -> map (fun col -> (col, row)) (range 0 (n - 1)))
  |> foldr (fun x y -> x @ y) []

(* Test the board function *)
let test_board () =
  let b = board 3 in
  print_endline "3x3 board positions:";
  List.iter (fun (x, y) -> Printf.printf "(%d,%d) " x y) b;
  print_endline ""

(* Helper function to check if element is in list *)
let rec mem (x : 'a) (l : 'a list) : bool =
  match l with
  | [] -> false
  | h :: t -> if h = x then true else mem x t

let safe_spots (existing_knights : (int * int) list) (n : int): (int * int) list =
  filter (fun x -> (is_safe x existing_knights) && not (mem x existing_knights)) (board n)

(* Solve the m-Knights problem starting with a given row *)
let rec solve (n: int) (m: int) (existing_knights: (int * int) list) : ((int * int) list) list =
  match m with
  | _ when m = (length existing_knights) -> [existing_knights]
  | _ ->
    safe_spots existing_knights n
    |> map (fun s -> solve n m (s :: existing_knights))
    |> foldr (fun x y -> x @ y) []

let m_knights n m = solve n m []

let chessboard_row (knights: (int * int) list) (r : int) (n : int) : string =
  range 0 (n - 1)
  |> map (fun c -> if mem (c, r) knights then "N" else "-")
  |> foldr (fun x s -> x ^ " " ^ s) ""

let chessboard_simple (knights: (int * int) list) (n: int) : string =
  range 0 (n - 1)
  |> map (fun r -> chessboard_row knights r n)
  |> foldr (fun x s -> x ^ "\n" ^ s) ""

let rec print_solutions (solutions: (int * int) list list) (n: int) =
  match solutions with
  | [] -> ()
  | x :: xs ->
    print_endline (chessboard_simple x n);
    print_endline "";
    print_solutions xs n

let n = 3
let m = 5
let solutions = m_knights n m
let () = 
  test_board ();
  print_endline "";
  print_solutions solutions n


(* But how do you get rid of duplicates? *)