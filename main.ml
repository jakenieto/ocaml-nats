type nat = Zero | Succ of nat

let rec int_of_nat nat = match nat with 
  | Zero -> 0
  | Succ(child) -> int_of_nat child + 1 

let my_nat = Succ(Succ(Zero))

let rec nat_of_int n = match n with
  | 0 -> Zero
  | n -> if n > 0 
         then Succ(nat_of_int(n-1))
         else raise (Failure("invalid input: negative numbers are invalid"))

(* type ('a,'b) result = Error of 'a | Ok of 'b  *)

(* 
let rec nat_of_int_result n = match n with
  | 0 -> Ok(Zero)
  | n -> if n > 0 
         then Ok(Succ(nat_of_int_result(n-1)))
         else Error(Failure("invalid input: negative numbers are invalid")) *)

let rec nat_add a b = match (a,b) with 
  | (a, Zero)
  | (Zero, a) -> a
  | (Succ(c), Succ(d)) ->  Succ(Succ(nat_add c d))

let rec nat_mult a b = match (a,b) with 
  | (a, Zero)
  | (Zero, a) -> Zero
  | (a, Succ(c)) -> nat_add a (nat_mult a c )
(* 
let rec nat_sub a b = match (a,b) with 
  | (a, Zero) -> Ok(a)
  | (Succ(c), Succ(d)) -> Ok(nat_sub c d)
  | (Zero, a) -> Error(Failure("invalid input: negative result")) *)

let rec nat_to_string n = match n with 
  | Zero -> "Zero"
  | Succ(a) -> "Succ(" ^ nat_to_string(a) ^ ")"


let () = Printf.printf "Answer: %d\n" (int_of_nat my_nat)
let () = Printf.printf "Answer: %s\n" (nat_to_string (nat_add (nat_of_int 2) (nat_of_int 3)))


