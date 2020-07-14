type nat = Zero | Succ of nat

let rec int_of_nat nat = match nat with 
  | Zero -> 0
  | Succ(child) -> int_of_nat child + 1 

let my_nat = Succ(Succ(Zero))

let () = Printf.printf "Answer: %d\n" (int_of_nat my_nat)

type bin_tree = Leaf | Node of bin_tree * bin_tree * int

(* Optional<a> *)
type 'a option = None () | Some of 'a

type boolean = True | False

type one = One

type traffic_light = Red | Yellow | Green

type traffic_light' = RedYellow of boolean | Green

(* product type or tuple type, size of set is the product of the size of the two smaller sets *)
type t = T of traffic_light * boolean * 

type t' = T' of traffic_light * boolean

(* union type or sum type, size of set is the sum of the size of the two smaller sets *)
type t2 = T2b of boolean | T2tl of traffic_light 
 
type t2' = T2b' of boolean | T2tl' of traffic_light | T2bottom of _|_


type t3 =  T3 of boolean -> traffic_light

(* might work *)
let rec (max_node : bin_tree -> int option) tree = match tree with
  | Leaf -> None   
  | Node (left, right, data) -> max (Some(data)) (max (max_node left) (max_node right)) 


let rec max_node (tree : bin_tree)  : int option = match tree with
  | Leaf -> None   
  | Node (left, right, data) -> max (Some(data)) (max (max_node left) (max_node right)) 


(* int -> (int -> (int)) *)
let add a b = a + b

let add_one = add 1 

let three = (add 1) 2 

