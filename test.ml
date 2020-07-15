open OUnit2
open ExtLib
open Main

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_int_of_nat (name : string) (n: nat) (expected: int) = t_any name expected (int_of_nat n)
  
let suite = "suite">:::[
    t_any "test" 2 (1+1);
    t_any_err "test-error" (fun () -> failwith "rip") (Failure("rip"));
]

let int_of_nat_tests = "int_of_nat_tests">:::[
    t_int_of_nat "zero" Zero 0;
    t_int_of_nat "one" (Succ(Zero)) 0;
]


let () = 
    run_test_tt_main suite; 
    run_test_tt_main int_of_nat_tests
    
