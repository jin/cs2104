(*******************************************)
(* Lab/Tutorial 3 : Higher-Order Functions *)
(*******************************************)
(*  Week of 7 Sept *)

(*
  Q1: Last via List.fold_Left

	Consider the last function below.
	Re-implement it using fold_left.
*)

let last (xs:'a list) : 'a =
	let rec aux xs prev =
		match xs with
		| [] -> prev
		| x::ys -> aux ys x in
	match xs with
	| [] -> failwith "no last element"
	| x::xs -> aux xs x

(* replace failwith by your code *)
let last2 (xs:'a list) : 'a =
  match xs with
  | [] -> failwith "no last element"
  | y::ys -> List.fold_left (fun a b -> b) y ys;;

(*
  Q2 : Sorting

	Consider the insertion sort method below.
	Re-implement the two methods using List.fold_right.
*)

let rec insert x ys =
  match ys with
    | [] -> [x]
    | y::ys ->
          if x<=y then x::y::ys
          else y::(insert x ys);;

let rec sort xs =
	match xs with
	| [] -> []
	| y::ys -> insert y (sort ys);;

(* replace failwith by your code *)
let insert2 x ys =
  fst (List.fold_right
    (fun y (r, ys) ->
      if x <= y then (x::y::ys, y::ys)
      else (y::r, y::ys)) ys ([x], []));;

let sort2 xs = List.fold_right (insert2) xs ([])

(*
  Q3 : You can compute the average of a list of
	numbers by dividing the sum of the elements by
	the length of the list. Use a single fold_left to
	compute both these values, and then compute
	the average. Throw an exception if the list is empty.
*)

(* replace failwith by your code *)
let average (ys: int list) : float =
  let length = match ys with
    | [] -> 0
    | x::xs -> List.fold_left(fun z y -> z + 1) 1 xs in
  let sum = match ys with
  | [] -> failwith "empty list"
  | x::xs -> List.fold_left (fun z y -> z +. float_of_int y) (float_of_int x) xs
  in
  sum /. float_of_int length;;

let average2 (xs: int list) : float =
  let sum = List.fold_left (fun z y -> z + y) 0 xs in
  let len = List.fold_left (fun z y -> z + 1) 0 xs in
  (float_of_int sum) /. (float_of_int len);;

let average3 (xs: int list) : float =
  let (sum, len) =
    List.fold_left (fun (zsum, zlen) y -> (zsum + y, zlen + 1)) (0, 0) xs in
  (float_of_int sum) /. (float_of_int len);;

let sum_alternate (xs: int list) : int =
  let sum = List.fold_left
  (fun (sum, flag) y ->
    if flag then (sum + y, false)
    else (sum, true)) (0, true) xs in
  fst sum;;

(*
  Q4 : Using Pipeline

	You can compute the median of a list of
	integers by sorting the list and computing its
	length, and then finding a middle element in
	the list. If there is an even number of elements,
	you are expected to compute the average of middle two
	elements. You are to use the |> operator as
	below in your median method.
*)

let ( |> ) (x:'a) (f:'a->'b) : 'b =  f x;;

(* your implementation for mid need not use higher-order functions *)
let mid (xs:int list) : float =
  xs
  |> List.length
  |> fun n ->
      let mid = n / 2 in
      if n mod 2 == 0 then
        float_of_int (List.nth xs (mid - 1) + List.nth xs (mid)) /. 2.0
      else float_of_int(List.nth xs (mid))

let median xs =
	xs
	|> sort
	|> mid

(*
  Q5 : Higher-Order functions for Trees

	You have designed a new tree data structure.
	It is a good practice	to provide a set of higher-order functions.

	(i) Based on your understanding of List.map, implement
		  a corresponding version for the map_tree function.

	(ii) Similarly, based on your understanding of List9.fold_right, implement
		   a corresponding version for the fold_tree function.

	Some examples of their uses are given below. They may be
	used as test cases for your implementation.

*)

type 'a tree =
	| Leaf of 'a
	| Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,t1);;
let t3 = Node (5,t2,t1);;

let rec map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
  match t with
  | Leaf l -> Leaf(f l)
  | Node (x, y, z) -> Node(f x, map_tree f y, map_tree f z);;

(*
   map_tree f (Node a1,Leaf a2,Leaf a3)
    ==> Node (f a1, Leaf (f a2), Leaf (f a3))
*)

let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
  let rec aux t =
    match t with 
    | Leaf l -> f1 l 
    | Node (x, lt, rt) -> f2 x (aux lt) (aux rt) in
  aux t;;
(*
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3)
    ==> f2 a1 (f1 a2) (f1 a3)
*)

let sum_tree t = fold_tree (fun x -> x) (fun x sum_lt sum_rt -> x + sum_lt +
sum_rt) t;;

let count_nodes t = fold_tree (fun x -> 1) (fun x count_lt count_rt -> 1 +
count_lt + count_rt) t;;

let ht_tree t = fold_tree (fun x -> 1) (fun 1 count_lt count_rt -> 1 +
max(count_lt, count_rt)) t;;

let max_tree t = fold_tree (fun x -> x) (fun x max_lt max_rt -> max(x,
max(max_lt, max_rt))) t;;

let count_leaves t = fold_tree (fun x -> 1) (fun x lt rt -> lt + rt) t;;

let t4=map_tree (fun x -> 2*x) t3;;
(* expecting a doubled version of t3
   Node (10, Node (8, Node (6, Leaf 2, Leaf 4), Node (6, Leaf 2, Leaf 4)),
     Node (6, Leaf 2, Leaf 4))
*)
fold_tree (fun x -> x) (fun a b c -> a+b+c) t3;;
(* expecting 27 *)
fold_tree (fun x -> [x]) (fun a b c -> b@(a::c)) t1;;
(* in-order traversal [1; 3; 2] *)
fold_tree (fun x -> [x]) (fun a b c -> a::(b@c)) t1;;
(* pre-order traversal [3; 1; 3] *)

(*
  Q6 : Writing one higher-order function in terms of another.

	Implement List.map, List.filter and List.partition
        in terms of LIst.fold_right.


*)
