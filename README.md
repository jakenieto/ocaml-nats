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

## Multiplication
```
let rec nat_mult a b = match (a,b) with 
  | (a, Zero)
  | (Zero, a) -> Zero
  | (a, Succ(c)) -> nat_add a (nat_mult a c )
```

## Inspiration
I would like to thank Michael Delmonaco for his help in teaching me OCaml and giving me the inspiration for this project. 
<https://quasarbright.github.io>
