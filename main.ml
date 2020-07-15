
type nat = Zero | Succ of nat | Undef

let rec int_of_nat nat = match nat with 
  | Undef -> raise (Failure("undefined"))
  | Zero -> 0
  | Succ(child) -> int_of_nat child + 1 

let rec nat_of_int n = match n with
  | 0 -> Zero
  | n when n > 0 -> Succ(nat_of_int(n-1))
  | n -> Undef
        
(* type ('a,'b) result = Error of 'a | Ok of 'b  *)

let rec nat_of_int_result n = 
  if n < 0 
    then Ok(nat_of_int n)
    else Error(Failure("invalid input: negative numbers are invalid")) 

let rec nat_add a b = match (a,b) with 
  | (Undef, _)
  | (_,Undef) ->  Undef
  | (a, Zero)
  | (Zero, a) -> a
  | (Succ(c), Succ(d)) ->  Succ(Succ(nat_add c d))

let rec nat_mult a b = match (a,b) with 
  | (Undef, _)
  | (_,Undef) ->  Undef
  | (_, Zero)
  | (Zero, _) -> Zero
  | (a, Succ(c)) -> nat_add a (nat_mult a c )

let rec nat_sub a b = match (a,b) with
  | (Undef, _)
  | (_, Undef) ->  Undef
  | (a, Zero) -> a
  | (Succ(c), Succ(d)) -> nat_sub c d
  | (Zero, _) -> Undef
  
let rec nat_to_string n = match n with 
  | Undef -> "Undef"
  | Zero -> "Zero"
  | Succ(a) -> "Succ(" ^ nat_to_string(a) ^ ")"

let rec nat_divide a b = match (a, b) with
| (Undef, _)
| (_, Undef) ->  Undef
| (_, Zero) -> Undef
| (Zero, _) -> Zero
| (c, d) -> 
    let diff = nat_sub c d  in 
      match diff with 
        | Undef -> Zero
        | nat -> Succ(nat_divide (nat_sub c d) d)

(* let () = Printf.printf "Answer: %d\n" (int_of_nat my_nat) *)
let () = Printf.printf "Answer: %s\n" (nat_to_string (nat_add (nat_of_int 2) (nat_of_int 3)))



