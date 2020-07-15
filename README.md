# ocaml-nats - Learning OCaml

  This is my first project in OCaml. I implemented the natural numbers and their basic operations (addition, subtraction, multiplication, etc...) in OCaml from first principles (without using OCaml’s built in Integer type).

## Basic Structure
The type for a natural number is defined as either 0, or the successor of a natural number. 
```
  type nat = Zero | Succ of nat
```

## Addition
```
let rec nat_add a b = match (a,b) with 
  | (a, Zero)
  | (Zero, a) -> a
  | (Succ(c), Succ(d)) ->  Succ(Succ(nat_add c d))
```
## Subtraction
```
let rec nat_sub a b = match (a,b) with
  | (Undef, _)
  | (_, Undef) ->  Undef
  | (a, Zero) -> a
  | (Succ(c), Succ(d)) -> nat_sub c d
  | (Zero, _) -> Undef
```
## Multiplication
```
let rec nat_mult a b = match (a,b) with 
  | (a, Zero)
  | (Zero, a) -> Zero
  | (a, Succ(c)) -> nat_add a (nat_mult a c )
  
```
## Division
```
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
```
## String Representation 
```
let rec nat_to_string n = match n with 
  | Undef -> "Undef"
  | Zero -> "Zero"
  | Succ(a) -> "Succ(" ^ nat_to_string(a) ^ ")"
```

## Inspiration
I would like to thank Michael Delmonaco for his help in teaching me OCaml and giving me the inspiration for this project. 
<https://quasarbright.github.io>
